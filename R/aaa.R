# classes ----------------------------------------------------------------------

relation <- S7::new_class("relation", package = "type", abstract = TRUE)

trait <- S7::new_class("trait", package = "type", abstract = TRUE)

type <- S7::new_class("type", package = "type", abstract = TRUE)

named_type <- S7::new_class(
  "named_type",
  package = "type",
  parent = type,
  properties = list(
    refinements = S7::new_property(S7::class_list, default = list())
  )
)

refined_type <- S7::new_class(
  "refined_type",
  package = "type",
  parent = type,
  properties = list(
    parent_type = named_type,
    refinements = S7::new_property(S7::class_list, default = list()),
    modifications = S7::new_property(S7::class_character, default = character())
  )
)

t_any <- S7::new_class("any", package = "type", parent = named_type)()

is_type <- function(x) {
  S7::S7_inherits(x, type)
}

is_named_type <- function(x) {
  S7::S7_inherits(x, named_type)
}

is_refined_type <- function(x) {
  S7::S7_inherits(x, refined_type)
}

type_class <- function(x) {
  S7::S7_class(x)
}

is_type_any <- function(x) {
  identical(x, t_any)
}

# internal constructors --------------------------------------------------------

# TODO: Allow parameters to be passed in as typed objects so that type checking
# can be added to the trait. Or, point to `as_typed()` instead. An annoying two
# step solution, but I think solves the problem nicely. We will still allow
# `validate_trait()` for checking inter-parameter interactions.
#
# TODO: We'll actually want to make this into a high-level constuctor. In particular,
#       the generated `out` function will need to do some checks for whether the
#       first argument is a <type> and emit a helpful error otherwise.
#
# NOTE: Low-level constructor
new_trait <- function(
  name,
  parameters = list(),
  package = "type"
) {
  parent_env <- rlang::caller_env()

  parameters_names <- names(parameters)
  properties <- rlang::rep_named(parameters_names, list(S7::class_any))
  trait_class <- S7::new_class(
    name = name,
    parent = trait,
    package = package,
    properties = properties
  )

  # TODO: Validate the names provided to parameters, can't have `...` or `.type`.

  parameters_syms <- rlang::set_names(
    rlang::syms(parameters_names),
    parameters_names
  )
  out <- rlang::new_function(
    # TODO: Does it matter where we evaluate this, maybe do so first?
    args = rlang::pairlist2(
      .type = type::t_any,
      !!!parameters,
      # Trait refiner functions look like `function(.type, <params>, ...)`
      # so that we can explicitly handle errors caused by providing too many
      # arguments, e.g. `complete(10L)` must have 0 arguments, not 1.
      ... = ,
    ),
    body = rlang::expr({
      # TODO: `validate_trait()` here for sure, also a better version of
      # assertions for the `type` argument, with a hint, did you mean `param1 = ...`
      trait_instance <- (!!trait_class)(!!!parameters_syms)
      new_refined_type(
        parent_type = .type,
        refinements = list(trait_instance)
      )
    }),
    env = parent_env
  )
  class(out) <- "type_trait_refiner"
  attr(out, "trait_class") <- trait_class
  out
}

trait_class <- function(x) {
  attr(x, "trait_class")
}

is_trait_refiner <- function(x) {
  inherits(x, "type_trait_refiner")
}

is_bare_trait <- function(x) {
  is_refined_type(x) &&
    is_type_any(x@parent_type) &&
    length(x@refinements) == 1L
}

# NOTE: Low-level constructor
#
# We maintain two single inheritance hierarchies in parallel:
# 1. The {S7} class hierarchy, used for dispatch.
# 2. The {type} type hierarchy, used for predicate subtyping.
#
# For example, `t_logical` roughly follows:
# - S7 class:  "bare_logical" > "bare_atomic" > "abstract_vector" > "abstract_any"
# - Predicate: is.logical(x)  > is.atomic(x)  > obj_is_vector(x)  > TRUE
new_named_type <- function(
  name,
  package = "type",
  parent_type = t_any,
  refinements = list(),
  inherit_refinements = FALSE
) {
  if (!S7::S7_inherits(parent_type, named_type)) {
    rlang::abort("`parent_type` must be a <named_type>.", .internal = TRUE)
  }
  s7_class <- S7::new_class(
    name = name,
    parent = S7::S7_class(parent_type),
    package = package
  )

  # Each refinement is packaged as a single refinement on `t_any`
  refinements <- lapply(refinements, \(x) x@refinements[[1]])
  if (inherit_refinements) {
    refinements <- c(parent_type@refinements, refinements)
  }
  s7_class(refinements = refinements)
}

new_refined_type <- function(
  parent_type,
  refinements = list(),
  modifications = character()
) {
  if (!S7::S7_inherits(parent_type, type)) {
    rlang::abort("`parent_type` must be a <type>.", .internal = TRUE)
  }
  if (S7::S7_inherits(parent_type, refined_type)) {
    refinements <- c(parent_type@refinements, refinements)
    modifications <- c(parent_type@modifications, modifications)
    parent_type <- parent_type@parent_type
  }
  refined_type(
    refinements = refinements,
    modifications = modifications,
    parent_type = parent_type
  )
}

# generics ---------------------------------------------------------------------

## obj -------------------------------------------------------------------------

obj_as_type <- S7::new_generic(
  "obj_as_type",
  c("obj"),
  function(obj, type, ...) {
    S7::S7_dispatch()
  }
)

## trait -----------------------------------------------------------------------

trait_name <- S7::new_generic("trait_name", c("trait"))

trait_test <- S7::new_generic(
  "trait_test",
  c("trait"),
  function(trait, obj, ...) {
    S7::S7_dispatch()
  }
)

trait_absent_message <- S7::new_generic(
  "trait_absent_message",
  c("trait"),
  function(trait, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

trait_absent_string <- S7::new_generic(
  "trait_absent_string",
  c("trait"),
  function(trait, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

trait_present_string <- S7::new_generic(
  "trait_present_string",
  c("trait"),
  function(trait, obj_name, ...) {
    S7::S7_dispatch()
  }
)

trait_validate <- S7::new_generic(
  "trait_validate",
  c("trait"),
  function(trait, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

trait_invalid_message <- S7::new_generic("trait_invalid_message", c("trait"))

trait_inline_rules <- S7::new_generic("trait_inline_rules", c("trait"))

## type ------------------------------------------------------------------------

type_name <- S7::new_generic("type_name", c("type"))

# Unexported, user-generated types are tested using their individual traits
type_test <- S7::new_generic("type_test", c("type"), function(type, obj, ...) {
  S7::S7_dispatch()
})

#' @description
#' Header message before the `trait_absent_message()`.
#'
#' ```
#' # Error in `foo()`:
#' ! Argument <object> is mistyped.
#' i <object> must be an employee dataframe.  # <- type_absent_header()
#' i column(<object>, "id") must be complete. # <- trait_absent_message()
#' x column(<object>, "id") contains `NA` elements at locations `c(1, 2)`.
#' ```
type_absent_header <- S7::new_generic(
  "type_absent_header",
  c("type"),
  function(type, obj_name, ...) {
    S7::S7_dispatch()
  }
)

#' @description
#' Message which replaces the `trait_absent_message()`.
#'
#' ```
#' # Error in `foo()`:
#' ! Argument <object> is mistyped.
#' i <object> must be a boolean (`TRUE` or `FALSE`).
#' x <object> is a <character> vector.
#' ```
type_absent_message <- S7::new_generic(
  "type_absent_message",
  c("type"),
  function(type, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

type_absent_string <- S7::new_generic(
  "type_absent_string",
  c("type"),
  function(type, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

type_present_string <- S7::new_generic(
  "type_present_string",
  c("type"),
  function(type, obj_name, ...) {
    S7::S7_dispatch()
  }
)

type_present_message <- S7::new_generic(
  "type_present_message",
  c("type"),
  function(type, obj_name, ...) {
    S7::S7_dispatch()
  }
)

type_validate <- S7::new_generic(
  "type_validate",
  c("type"),
  function(type, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

type_inline_validate_factory <- S7::new_generic(
  "type_inline_validate_factory",
  c("type")
)

# default methods --------------------------------------------------------------

# TODO: Implement default methods for *every* generic.

## trait -----------------------------------------------------------------------

method(trait_validate, trait) <- function(trait, obj, obj_name) {
  if (identical(trait_test(trait, obj), TRUE)) {
    NULL
  } else {
    trait_absent_message(trait, obj, obj_name)
  }
}

method(trait_inline_rules, trait) <- function(trait) {
  list()
}

## type ------------------------------------------------------------------------

# TODO:
# - type_present_string: `obj_name` is type <type_name> with traits: <trait_name>, ...
# - type_present_message:
#   `obj_type` is type <type_name> with traits:
#   * trait_present_string(trait1)
#   * trait_present_string(trait2)

method(type_test, named_type) <- function(type, obj) {
  for (trait in type@refinements) {
    if (!rlang::is_true(trait_test(trait, obj))) return(FALSE)
  }
  TRUE
}

method(type_test, refined_type) <- function(type, obj) {
  for (trait in type@parent_type@refinements) {
    if (!rlang::is_true(trait_test(trait, obj))) return(FALSE)
  }
  for (trait in type@refinements) {
    if (!rlang::is_true(trait_test(trait, obj))) return(FALSE)
  }
  TRUE
}

method(type_absent_message, named_type) <- function(type, obj, obj_name) {
  for (trait in type@refinements) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      return(trait_absent_message(trait, obj, obj_name))
    }
  }
}

method(type_absent_message, refined_type) <- function(type, obj, obj_name) {
  for (trait in type@parent_type@refinements) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      return(trait_absent_message(trait, obj, obj_name))
    }
  }
  for (trait in type@refinements) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      return(trait_absent_message(trait, obj, obj_name))
    }
  }
}

method(type_validate, type) <- function(type, obj, obj_name) {
  if (rlang::is_true(type_test(type, obj))) {
    NULL
  } else {
    type_absent_message(type, obj, obj_name)
  }
}

method(type_inline_validate_factory, type) <- function(type) {
  NULL
}
