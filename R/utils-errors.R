# assert ------------------------------------------------------------------------

assert_is_type <- function(
  x,
  x_name = caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (is_type(x)) {
    return(invisible())
  }

  abort_bad_input(
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

  abort_bad_input(
    message = message,
    error_call = error_call
  )
}

# TODO: We'll need to add an `exported_` or `inlined_` prefix to this and other
# functions that aren't meant to be used but *are* inserted into generated
# functions.
#
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

abort_bad_input <- function(
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

abort_internal <- function(message, error_call = rlang::caller_env()) {
  rlang::abort(
    message = message,
    class = c("type_error_internal", "type_error"),
    call = error_call,
    .internal = TRUE
  )
}
