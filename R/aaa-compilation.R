# type validate ----------------------------------------------------------------

type_validate_expr <- function(type, obj_sym, obj_name, env) {
  if (is_named_type(type)) {
    parent_type <- type
    parent_traits <- type@refinements
    refinement_traits <- list()
  } else {
    parent_type <- type@parent_type
    parent_traits <- parent_type@refinements
    refinement_traits <- type@refinements
  }

  type_validate_factory <- type_inline_validate_factory(parent_type)

  if (!is.null(type_validate_factory)) {
    validate_exprs <- list(type_validate_factory(type, obj_sym, obj_name, env))
    prior_traits <- parent_traits
    traits_to_inline <- refinement_traits
  } else {
    validate_exprs <- list()
    prior_traits <- list()
    traits_to_inline <- c(parent_traits, refinement_traits)
  }

  validate_exprs <- c(
    validate_exprs,
    map(
      traits_to_inline,
      function(trait) {
        inline_rules <- trait_inline_rules(trait)
        factory <- resolve_inline_rules(inline_rules, parent_type, prior_traits)
        factory(trait, obj_sym, obj_name, env)
      }
    )
  )

  # Bypass lazy evaluation when performing a single validation
  if (length(validate_exprs) == 1L) {
    return(validate_exprs[[1L]])
  }

  rlang::call2("lazy_validate", !!!validate_exprs, .ns = "type")
}

# predicate and message evaluation ---------------------------------------------

lazy_validate <- function(...) {
  for (i in rlang::seq2(1, ...length())) {
    if (!is.null(...elt(i))) return(...elt(i))
  }
}

# requirements -----------------------------------------------------------------

has_trait_class <- function(parent_type, refinements, target_trait_refiner) {
  all_refinements <- c(parent_type@refinements, refinements)
  target_class <- trait_class(target_trait_refiner)
  for (trait in all_refinements) {
    if (S7::S7_inherits(trait, target_class)) return(TRUE)
  }
  FALSE
}

has_trait_classes <- function(parent_type, refinements, target_trait_refiners) {
  all_refinements <- c(parent_type@refinements, refinements)
  target_classes <- map(target_trait_refiners, trait_class)
  for (target_class in target_classes) {
    has_target <- FALSE
    for (trait in all_refinements) {
      if (S7::S7_inherits(trait, target_class)) {
        has_target <- TRUE
        break
      }
    }
    if (!has_target) return(FALSE)
  }
  TRUE
}

has_bare_trait <- function(parent_type, refinements, target_trait) {
  all_refinements <- c(parent_type@refinements, refinements)
  for (trait in all_refinements) {
    if (!identical(trait, target_trait)) return(FALSE)
  }
  TRUE
}

inherits_type <- function(parent_type, target_type) {
  S7::S7_inherits(parent_type, type_class(target_type))
}

# inlining ---------------------------------------------------------------------

new_inline_rule <- function(
  validate_factory = NULL,
  requires = NULL
) {
  list(
    validate_factory = validate_factory %||% default_trait_validate_factory(),
    requires = requires %||% function(parent_type, refinements) TRUE
  )
}

resolve_inline_rules <- function(rules, parent_type, refinements) {
  can_inline <- function(rule, parent_type, refinements) {
    rule$requires(parent_type, refinements)
  }

  for (rule in rules) {
    if (can_inline(rule, parent_type, refinements)) {
      return(rule$validate_factory)
    }
  }

  default_trait_validate_factory()
}

inline_trait_validate <- function(
  test_expr = trait_test(!!trait, !!obj_sym),
  message_expr = trait_absent_message(!!trait, !!obj_sym, !!obj_name),
  env = rlang::caller_env()
) {
  inline_validate(
    test_expr = substitute(test_expr),
    message_expr = substitute(message_expr),
    kind = "trait",
    env = env
  )
}

inline_type_validate <- function(
  test_expr = type_test(!!type, !!obj_sym),
  message_expr = type_absent_message(!!type, !!obj_sym, !!obj_name),
  env = rlang::caller_env()
) {
  inline_validate(
    test_expr = substitute(test_expr),
    message_expr = substitute(message_expr),
    kind = "type",
    env = env
  )
}

custom_inline_type_validate <- function(body, env = rlang::caller_env()) {
  inline_validate(body_expr = substitute(body), kind = "type", env = env)
}

custom_inline_trait_validate <- function(body, env = rlang::caller_env()) {
  inline_validate(body_expr = substitute(body), kind = "trait", env = env)
}

inline_validate <- function(
  test_expr,
  message_expr,
  body_expr,
  kind = c("trait", "type"),
  env = rlang::caller_env()
) {
  kind <- rlang::arg_match0(kind, c("trait", "type"))
  args <- switch(
    kind,
    trait = rlang::pairlist2(trait = , obj_sym = , obj_name = , env = , ),
    type = rlang::pairlist2(type = , obj_sym = , obj_name = , env = , )
  )
  rlang::new_function(
    args = args,
    body = if (rlang::is_missing(body_expr)) {
      rlang::expr(if (!!test_expr) NULL else !!message_expr)
    } else {
      body_expr
    },
    env = env
  )
}

default_trait_validate_factory <- function() {
  function(trait, obj_sym, obj_name, env) {
    rlang::call2("trait_validate", trait, obj_sym, obj_name, .ns = "type")
  }
}
