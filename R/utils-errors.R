# assert ------------------------------------------------------------------------

assert_is_type <- function(
  x,
  x_name = caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (is_type(x)) {
    return(invisible())
  }

  type_abort_bad_input(
    format_styled("{.arg {x_name}} must be a type, not <<fmt_r_type(x)>>."),
    error_call = error_call
  )
}

assert_is_chr <- function(
  x,
  complete = FALSE,
  maybe = FALSE,
  x_name = caller_arg(x),
  error_call = rlang::caller_env()
) {
  if ((maybe && is.null(x)) || (is.character(x) && (!complete || !anyNA(x)))) {
    return(invisible())
  }

  abort_bad_vec_type(
    x = x,
    what = "character vector",
    complete = complete,
    maybe = maybe,
    x_name = x_name,
    error_call = error_call
  )
}

# abort ------------------------------------------------------------------------

abort_bad_vec_type <- function(
  x,
  what,
  complete = FALSE,
  maybe = FALSE,
  x_name = caller_arg(x),
  error_call = rlang::caller_env()
) {
  must <- if (maybe) "{what} or {.val NULL}" else "{what}"

  if (!is.character(x) && !is.null(x)) {
    message <- format_styled(
      "{.arg {x_name}} must be a <<must>>, not <<fmt_r_type(x)>>."
    )
  } else {
    missing_at <- which(is.na(x))
    message <- c(
      format_styled("{.arg {x_name}} must be a complete <<must>>."),
      x = format_styled(
        "{.arg {x_name}} is {.val NA} <<fmt_at_locs(missing_at)>>."
      )
    )
  }

  type_abort_bad_input(
    message = message,
    error_call = error_call
  )
}

# TODO: We'll need to add an `exported_` or `inlined_` prefix to this and other
# functions that aren't meant to be used but *are* inserted into generated
# functions.
#' @export
abort_mistyped <- function(
  message,
  error_call = rlang::caller_env(),
  error_subclass = character(),
  parent = NULL
) {
  rlang::abort(
    message = message,
    class = c(error_subclass, "type_error_mistyped", "type_error"),
    call = error_call,
    parent = parent
  )
}

# Want to differentiate errors during type creation from other input errors, so that:
# - typed(function(x = sized(size = "A")) {}) raises a malformed-type error
# - typed(function(x = stop('AH')) {}) acts like `function(x = stop('AH') {})`, e.g. no error is raised
# - And critically, typed(function(x = type::obj_assert_type(...)) {}), also doesn't raise an error
#
# This is also used in the trait_validate() generic, so that invalid user-created
# traits also cause an error, e.g. `typed(function(x = my_sized(size = "A")) {})`.
abort_malformed_type <- function(
  message,
  parent = NULL,
  error_call = rlang::caller_env()
) {
  rlang::abort(
    message = message,
    class = c("type_error_malformed_type", "type_error"),
    call = error_call,
    parent = parent
  )
}

type_abort_bad_input <- function(
  message,
  parent = NULL,
  error_call = rlang::caller_env()
) {
  rlang::abort(
    message = message,
    class = c("type_error_bad_input", "type_error"),
    call = error_call,
    parent = parent
  )
}

type_abort_internal <- function(message, error_call = rlang::caller_env()) {
  rlang::abort(
    message = message,
    class = c("type_error_internal", "type_error"),
    call = error_call,
    .internal = TRUE
  )
}
