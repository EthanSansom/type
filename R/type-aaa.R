type <- S7::new_class(
  "type", 
  package = "type", 
  properties = list(
    traits = S7::new_property(S7::class_list, default = list()),
    modifications = S7::new_property(S7::class_character, default = character())
  )
)

#' Test if an object is a type
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a type, `FALSE` otherwise.
#' 
#' @examples
#' is_type(10L)
#' is_type(t_int)
#' 
#' @export
is_type <- function(x) {
  S7::S7_inherits(x, type)
}

#' Check an object against a type
#'
#' @description
#' These functions are used to investiage whether an object `obj` has a type `type`.
#'
#' - `obj_is_type(obj, type)` returns `TRUE` if `obj` has type `type` and `FALSE` otherwise.
#' - `obj_inspect_type(obj, type)` prints a success or failure message for each type check run on `obj` then returns `NULL` invisibly.
#' - `obj_assert_type(obj, type)` raises a `<type_error_mistyped_obj>` error if `obj` is mistyped and returns `NULL` invisibly otherwise.
#'
#' @param obj
#' 
#' An object to check.
#' 
#' @param type 
#' 
#' A type to check against.
#' 
#' @param obj_name 
#' 
#' The name of `obj`, used in messages. Defaults to the expression passed to `obj`.
#'
#' @return
#' - `obj_is_type()`: `TRUE` if `obj` is the correct type, `FALSE` otherwise.
#' - `obj_inspect_type()`: `NULL` invisibly, called for its side effect.
#' - `obj_assert_type()`: `NULL` invisibly if `obj` is the correct type, otherwise raises an error.
#'
#' @examples
#' good <- TRUE
#' bad <- NA
#' 
#' # Test whether an object is a boolean
#' obj_is_type(good, t_bool)
#' obj_is_type(bad, t_bool)
#'
#' # Print the type tests run on the object
#' obj_inspect_type(good, t_bool)
#' obj_inspect_type(bad, t_bool)
#'
#' # Raise an error if the object is not a boolean
#' obj_assert_type(good, t_bool)
#' try(obj_assert_type(bad, t_bool))
#'
#' @name obj-type
NULL

#' @rdname obj-type
#' @export
obj_is_type <- function(obj, type) {
  assert_is_type(type)
  type_test(type, obj)
}

#' @rdname obj-type
#' @export
obj_inspect_type <- function(
  obj,
  type, 
  obj_name = rlang::caller_arg(obj)
) {
  assert_is_type(type)
  assert_is_string(obj_name)

  header <- format_styled("Object {.arg {obj_name}} has the expected type.")
  messages <- character()
  for (trait in type@traits) {
    if (rlang::is_true(trait_test(trait, obj))) {
      messages <- c(messages, rlang::set_names(trait_describe(trait, obj_name), "v"))
    } else {
      header <- format_styled("Object {.arg {obj_name}} does not have the expected type.")
      messages <- c(messages, trait_diagnose(trait, obj, obj_name))
      break
    }
  }
  cat_bullets(c(header, messages))
  invisible()
}

#' @rdname obj-type
#' @export
obj_assert_type <- function(
  obj,
  type, 
  obj_name = rlang::caller_arg(obj)
) {
  assert_is_type(type)
  assert_is_string(obj_name)

  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      inline_abort_mistyped(
        type = type,
        message = c(
          format_styled("Object {.arg {obj_name}} is mistyped."),
          trait_diagnose(trait, obj, obj_name)
        ),
        what = "obj"
      )
    }
  }
  invisible()
}


#' Return the expected type of the last mistyped object
#'
#' @description
#' 
#' `last_type()` returns the expected type of the last object to
#' fail a type check in [obj_assert_type()] or [typed()].
#' 
#' @return 
#' 
#' The last expected type. If no type assertions have been run, returns `NULL`.
#' 
#' @examples
#' # Returns `NULL` if no type checks have been run
#' last_type()
#' 
#' # `last_type()` returns `t_bool` after failed assertion
#' \dontrun{
#' obj_assert_type(10L, t_bool)
#' last_type()
#' }
#' 
#' @export
last_type <- function() {
  the$last_type
}

# generics ---------------------------------------------------------------------

method(base_print, type) <- function(x, ...) {
  cli::cat_line(glue::glue("<type>"))
  cat_bullets(rlang::set_names(type_describe(x, "<object>"), "*"))
  invisible(x)
}

# helpers ----------------------------------------------------------------------

add_trait <- function(type, trait) {
  current_traits <- type@traits
  duplicated <- any(map_lgl(current_traits, identical, trait))
  if (duplicated) {
    return(type)
  }

  type@traits <- c(current_traits, trait)
  type
}

type_test <- function(type, obj) {
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) return(FALSE)
  }
  TRUE
}

type_diagnose <- function(type, obj, obj_name) {
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      return(trait_diagnose(trait, obj, obj_name))
    }
  }
}

type_describe <- function(type, obj_name) {
  if (rlang::is_empty(type@traits)) {
    return(format_styled("{.arg {obj_name}} is an R object."))
  }
  unlist(map(type@traits, trait_describe, obj_name))
}

#' @rdname inlined-functions
#' @export
inline_abort_mistyped <- function(
  type,
  message,
  what = c("obj", "arg"),
  error_call = rlang::caller_env()
) {
  subclass <- paste0("type_error_mistyped_", what)
  the$last_type <- type
  rlang::abort(
    c(
      message,
      i = format_styled("Run {.run last_type()} to get the expected type.")
    ),
    class = c(subclass, "type_error_mistyped", "type_error"),
    call = error_call
  )
}