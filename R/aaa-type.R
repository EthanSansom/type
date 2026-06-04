# classes ----------------------------------------------------------------------

type <- S7::new_class("type", package = "type", abstract = TRUE)

named_type <- S7::new_class(
  "named_type",
  package = "type",
  parent = type,
  properties = list(
    traits = S7::new_property(S7::class_list, default = list())
  )
)

refined_type <- S7::new_class(
  "refined_type",
  package = "type",
  parent = type,
  properties = list(
    parent_type = named_type,
    traits = S7::new_property(S7::class_list, default = list()),
    modifications = S7::new_property(S7::class_character, default = character())
  )
)

t_any <- S7::new_class("any", package = "type", parent = named_type)()

type_class <- function(x) {
  S7::S7_class(x)
}

is_type_any <- function(x) {
  identical(x, t_any)
}

is_type <- function(x) {
  S7::S7_inherits(x, type)
}

is_named_type <- function(x) {
  S7::S7_inherits(x, named_type)
}

is_refined_type <- function(x) {
  S7::S7_inherits(x, refined_type)
}

type_traits <- function(x) {
  if (is_refined_type(x)) {
    list(x@parent_type@traits, x@traits)
  } else {
    x@traits
  }
}

type_modifications <- function(x) {
  if (is_refined_type(x)) {
    x@modifications
  } else {
    character()
  }
}

# package constructors ---------------------------------------------------------

# We maintain two single inheritance hierarchies in parallel:
# 1. The {S7} class hierarchy, used for dispatch.
# 2. The {type} type hierarchy, used for predicate subtyping.
#
# For example, `t_logical` roughly follows:
# - S7 class:  "bare_logical" > "bare_atomic" > "abstract_vector" > "abstract_any"
# - Predicate: is.logical(x)  > is.atomic(x)  > obj_is_vector(x) > TRUE
new_named_type <- function(
  name,
  package = "type",
  parent_type = t_any,
  traits = list(),
  inherit_traits = FALSE
) {
  s7_class <- S7::new_class(
    name = name,
    parent = S7::S7_class(parent_type),
    package = package
  )
  if (inherit_traits) {
    traits <- c(parent_type@traits, traits)
  }
  s7_class(traits = traits)
}

new_refined_type <- function(
  parent_type,
  traits = list(),
  modifications = character()
) {
  type_traits <- type_traits(parent_type)
  type_modifications <- type_modifications(parent_type)

  traits <- unique(traits)
  duplicated_traits <- map_lgl(
    traits,
    function(trait) any(map_lgl(type_traits, identical, trait))
  )
  traits <- traits[!duplicated_traits]
  modifications <- setdiff(modifications, type_modifications)

  if (is_refined_type(parent_type)) {
    traits <- c(parent_type@traits, traits)
    modifications <- c(parent_type@modifications, modifications)
    parent_type <- parent_type@parent_type
  }

  refined_type(
    traits = traits,
    modifications = modifications,
    parent_type = parent_type
  )
}

# package methods --------------------------------------------------------------

#' @export
type_name <- S7::new_generic(
  "type_name",
  c("type")
)

# method(type_name, type) <- function(type) {}

#' @export
type_test <- S7::new_generic("type_test", c("type"), function(type, obj, ...) {
  S7::S7_dispatch()
})

method(type_test, named_type) <- function(type, obj) {
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) return(FALSE)
  }
  TRUE
}

method(type_test, refined_type) <- function(type, obj) {
  for (trait in type@parent_type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) return(FALSE)
  }
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) return(FALSE)
  }
  TRUE
}

#' @export
type_absent_header <- S7::new_generic(
  "type_absent_header",
  c("type"),
  function(type, obj_name, ...) {
    S7::S7_dispatch()
  }
)

method(type_absent_header, type) <- function(type, obj_name) {
  character() # Defaults to no additional header
}

#' @export
type_absent_message <- S7::new_generic(
  "type_absent_message",
  c("type"),
  function(type, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

method(type_absent_message, named_type) <- function(type, obj, obj_name) {
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      return(c(
        i = type_absent_header(type, obj_name),
        trait_absent_message(trait, obj, obj_name)
      ))
    }
  }
}

method(type_absent_message, refined_type) <- function(type, obj, obj_name) {
  parent_absent_message <- type_absent_message(type@parent_type, obj, obj_name)
  if (!is.null(parent_absent_message)) {
    return(parent_absent_message)
  }
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      return(c(
        i = type_absent_header(type, obj_name),
        trait_absent_message(trait, obj, obj_name)
      ))
    }
  }
}

#' @export
type_present_string <- S7::new_generic(
  "type_present_string",
  c("type"),
  function(type, obj_name, ...) {
    S7::S7_dispatch()
  }
)

method(type_present_string, named_type) <- function(type, obj_name) {
  traits <- type@traits
  if (rlang::is_empty(traits)) {
    return(format_styled("{.arg {obj_name}} is type {.cls type_name(type)}."))
  }
  trait_names <- map_chr(traits, trait_name)
  format_styled(
    "{.arg {obj_name}} is type {.cls type_name(type)} with traits: <<commas(trait_names)>>."
  )
}

method(type_present_string, refined_type) <- function(type, obj_name) {
  parent_traits <- type@parent_type@traits
  refinement_traits <- type@traits
  all_traits <- c(parent_traits, refinement_traits)
  trait_names <- map_chr(all_traits, trait_name)
  format_styled(
    "{.arg {obj_name}} is type {.cls type_name(type)} with traits: <<commas(trait_names)>>."
  )
}

#' @export
type_present_message <- S7::new_generic(
  "type_present_message",
  c("type"),
  function(type, obj_name, ...) {
    S7::S7_dispatch()
  }
)

method(type_present_message, named_type) <- function(type, obj_name) {
  traits <- type@traits
  if (length(traits) == 0L) {
    return(c(v = format_styled(
      "{.arg {obj_name}} is type {.cls type_name(type)}."
    )))
  }

  trait_strings <- map_chr(traits, trait_present_string, obj_name = obj_name)
  c(
    format_styled("{.arg {obj_name}} is type {.cls type_name(type)} with traits:"),
    rlang::set_names(trait_strings, "*")
  )
}

method(type_present_message, refined_type) <- function(type, obj_name) {
  parent_traits <- type@parent_type@traits
  refinement_traits <- type@traits
  all_traits <- c(parent_traits, refinement_traits)
  if (length(all_traits) == 0L) {
    return(format_styled(
      "{.arg {obj_name}} is type {.cls type_name(type@parent_type)}."
    ))
  }

  trait_strings <- map_chr(all_traits, trait_present_string, obj_name = obj_name)
  c(
    format_styled(
      "{.arg {obj_name}} is type {.cls {type_name(type@parent_type)}} with traits:"
    ),
    rlang::set_names(trait_strings, "*")
  )
}

#' @export
type_validate <- S7::new_generic(
  "type_validate",
  c("type"),
  function(type, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

method(type_validate, type) <- function(type, obj, obj_name) {
  if (rlang::is_true(type_test(type, obj))) {
    NULL
  } else {
    type_absent_message(type, obj, obj_name)
  }
}

#' @export
type_test_inline <- S7::new_generic(
  "type_test_inline",
  c("type"),
  function(type, obj_sym, ...) {
    S7::S7_dispatch()
  }
)

method(type_test_inline, type) <- function(type, obj_sym, ...) {
  NULL
}

#' @export
type_validate_inline <- S7::new_generic(
  "type_validate_inline",
  c("type"),
  function(type, obj_sym, obj_name, ...) {
    S7::S7_dispatch()
  }
)

# Should never be called:
# method(type_validate_inline, refined_type) <- function(type, obj_sym, obj_name, ...)

method(type_validate_inline, named_type) <- function(type, obj_sym, obj_name, ...) {
  test_expr <- type_test_inline(type, obj_sym)
  if (!is.null(test_expr)) {
    message_expr <- rlang::call2("type_absent_message", type, obj_sym, obj_name, .ns = "type")
    return(rlang::expr(if (!(!!test_expr)) !!message_expr))
  }

  # If a custom absence message or header is defined for the `type`, we can't
  # inline the constituent traits, as their message would be incorrect.
  if (named_type_has_custom_message(type)) {
    return(rlang::call2("type_validate", type, obj_sym, obj_name, .ns = "type"))
  }

  NULL # Defers inlining to the constituent traits
}

named_type_has_custom_message <- function(type) {
  !identical(
    method(type_absent_header, object = type), 
    method(type_absent_header, class = named_type)
  ) ||
    !identical(
      method(type_absent_message, object = type), 
      method(type_absent_message, class = named_type)
    )
}

# external methods -------------------------------------------------------------

base_print <- S7::new_external_generic("base", "print", "x")

method(base_print, named_type) <- function(x, ...) {
  cli::cat_line(glue::glue("<type<{type_name(x)}>>"))
  cat_bullets(type_present_message(x, "<object>"))
  invisible(x)
}

method(base_print, refined_type) <- function(x, ...) {
  cli::cat_line(glue::glue("<refined_type<{type_name(x@parent_type)}>>"))
  cat_bullets(type_present_message(x, "<object>"))
  invisible(x)
}
