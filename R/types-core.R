# definition -------------------------------------------------------------------

t_atomic <- NULL

t_logical <- t_lgl <- NULL

t_bool <- NULL

# package methods --------------------------------------------------------------

method(type_name, type_class(t_any)) <- function(type) {
  "any"
}

method(type_present_string, type_class(t_any)) <- function(type, obj_name) {
  format_styled("{.arg {obj_name}} is an object in {.emph R}.")
}

method(type_present_message, type_class(t_any)) <- function(type, obj_name) {
  type_present_string(type, obj_name)
}

on_load_core_types <- function() {
  # t_atomic -------------------------------------------------------------------

  t_atomic <<- new_named_type(
    "atomic",
    parent_type = t_vector,
    traits = list(
      is_atomic_trait()
    )
  )

  method(type_name, type_class(t_atomic)) <- function(type) {
    "atomic"
  }

  method(type_present_string, type_class(t_atomic)) <- function(type, obj_name) {
    format_styled("{.arg {obj_name}} is an atomic vector.")
  }

  method(type_present_message, type_class(t_atomic)) <- function(type, obj_name) {
    type_present_string(type, obj_name)
  }

  # t_logical ------------------------------------------------------------------

  t_logical <<- t_lgl <<- new_named_type(
    "bare_logical",
    parent_type = t_atomic,
    traits = list(
      bare_typed_trait("logical")
    )
  )

  method(type_name, type_class(t_logical)) <- function(type) {
    "logical"
  }

  method(type_present_string, type_class(t_logical)) <- function(type, obj_name) {
    bare_type_present_string(obj_name, "logical vector")
  }

  method(type_present_message, type_class(t_logical)) <- function(type, obj_name) {
    type_present_string(type, obj_name)
  }

  # t_bool ---------------------------------------------------------------------

  t_bool <<- new_named_type(
    "bare_bool",
    parent_type = t_logical,
    traits = list(
      sized_trait(1L),
      complete_trait()
    ),
    inherit_traits = TRUE
  )

  method(type_name, type_class(t_bool)) <- function(type) {
    "boolean"
  }

  method(type_present_string, type_class(t_bool)) <- function(type, obj_name) {
    format_styled("{.arg {obj_name}} is a boolean ({.val {TRUE}} or {.val {FALSE}}).")
  }

  method(type_present_message, type_class(t_bool)) <- function(type, obj_name) {
    type_present_string(type, obj_name)
  }

  method(type_absent_message, type_class(t_bool)) <- function(
    type,
    obj,
    obj_name
  ) {
    if (identical(obj, NA)) {
      message <- format_styled(
        "{.arg {obj_name}} must be a boolean ({.val {TRUE}} or {.val {FALSE}}), not {.val {NA}}."
      )
      return(c(x = message))
    }
    if (rlang::is_bare_logical(obj)) {
      what <- if (rlang::is_empty(obj)) {
        "an empty {.cls logical} vector"
      } else {
        "a size {length(obj)} {.cls logical} vector"
      }
    } else {
      what <- "{fmt_r_type(obj)}"
    }
    c(
      i = format_styled(
        "{.arg {obj_name}} must be a boolean ({.val {TRUE}} or {.val {FALSE}})."
      ),
      x = format_styled("{.arg {obj_name}} is <<what>>.")
    )
  }

  method(type_test_inline, type_class(t_bool)) <- function(type, obj_sym) {
    rlang::expr(
      !base::is.object(!!obj_sym) &&
        base::is.logical(!!obj_sym) &&
        base::length(!!obj_sym) == 1L &&
        !base::is.na(!!obj_sym)
    )
  }
}

# helpers ----------------------------------------------------------------------

bare_type_present_string <- function(obj_name, what) {
  format_styled("{.arg {obj_name}} is a bare <<what>>.")
}
