# TODO: Document with roxygen, follow S7 example
t_vector <- NULL
t_atomic <- NULL
t_integer <- t_int <- NULL
t_logical <- t_lgl <- NULL
t_bool <- NULL
t_dots <- NULL

on_load_core_types <- function() {
  t_vector <<- new_named_type(
    "union_vector",
    parent_type = t_any,
    refinements = list(
      trait_obj_is_vector()
    )
  )

  t_atomic <<- new_named_type(
    "union_atomic",
    parent_type = t_vector,
    refinements = list(
      trait_obj_is_atomic()
    )
  )

  t_integer <<- t_int <<- new_named_type(
    "bare_integer",
    parent_type = t_atomic,
    refinements = list(
      bare_typed(typeof = "integer")
    )
  )

  t_logical <<- t_lgl <<- new_named_type(
    "bare_logical",
    parent_type = t_atomic,
    refinements = list(
      bare_typed(typeof = "logical")
    )
  )

  t_bool <<- new_named_type(
    "bare_bool",
    parent_type = t_logical,
    refinements = list(
      sized(size = 1L),
      complete()
    ),
    inherit_refinements = TRUE
  )

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
    if (is_bare_logical(obj)) {
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

  method(type_inline_validate_factory, type_class(t_bool)) <- function(type) {
    inline_type_validate(
      !base::is.object(!!obj_sym) &&
        base::is.logical(!!obj_sym) &&
        base::length(!!obj_sym) == 1L &&
        !base::is.na(!!obj_sym)
    )
  }

  # For use in functions only
  t_dots <<- new_refined_type(
    parent_type = t_any,
    modifications = "endotted"
  )
}
