#nocov start

# assert ------------------------------------------------------------------------

assert_is_type <- function(
  x,
  x_name = rlang::caller_arg(x),
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

assert_is_trait <- function(
  x,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (is_trait(x)) {
    return(invisible())
  }

  abort_bad_input(
    format_styled("{.arg {x_name}} must be a trait, not <<fmt_r_type(x)>>."),
    error_call = error_call
  )
}

assert_is_relation <- function(
  x,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (is_relation(x)) {
    return(invisible())
  }

  abort_bad_input(
    format_styled(
      "{.arg {x_name}} must be the result of a relation-function, ",
      "e.g. {.fn same_sized}, not <<fmt_r_type(x)>>."
    ),
    error_call = error_call
  )
}

assert_is_symbol <- function(
  x,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (rlang::is_symbol(x)) {
    return(invisible())
  }

  abort_bad_input(
    format_styled("{.arg {x_name}} must be a symbol, not <<fmt_r_type(x)>>."),
    error_call = error_call
  )
}

assert_is_list <- function(
  x,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (is.list(x)) {
    return(invisible())
  }

  abort_bad_input(
    format_styled("{.arg {x_name}} must be a list, not <<fmt_r_type(x)>>."),
    error_call = error_call
  )
}

assert_is_string <- function(
  x,
  maybe = FALSE,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if ((maybe && is.null(x)) || (is.character(x) && length(x) == 1L && !is.na(x))) {
    return(invisible())
  }

  if (is.character(x) && length(x) != 1) {
    abort_bad_input(
      format_styled(
        "{.arg {x_name}} must be a single string, ",
        "not a length {length(x)} character vector."
      ),
      error_call = error_call
    )
  }

  abort_bad_input(
    format_styled("{.arg {x_name}} must be a single string, not <<fmt_r_type(x)>>."),
    error_call = error_call
  )
}

assert_is_count <- function(
  x,
  maybe = FALSE,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if ((maybe && is.null(x)) || (rlang::is_integerish(x) && length(x) == 1L && !is.na(x) && x >= 0)) {
    return(invisible())
  }

  if (rlang::is_integerish(x) && length(x) != 1L) {
    abort_bad_input(
      format_styled(
        "{.arg {x_name}} must be a single count, ",
        "not a length {length(x)} vector."
      ),
      error_call = error_call
    )
  }

  if (rlang::is_integerish(x) && length(x) == 1L && !is.na(x) && x < 0) {
    abort_bad_input(
      format_styled(
        "{.arg {x_name}} must be a non-negative count, ",
        "not {x}."
      ),
      error_call = error_call
    )
  }

  abort_bad_input(
    format_styled("{.arg {x_name}} must be a count, not <<fmt_r_type(x)>>."),
    error_call = error_call
  )
}

assert_is_index <- function(
  x,
  scalar = TRUE,
  maybe = FALSE,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  is_int <- rlang::is_integerish(x) && !anyNA(x) && all(x >= 1)
  is_chr <- is.character(x) && !anyNA(x)
  right_length <- !scalar || length(x) == 1L

  if ((maybe && is.null(x)) || ((is_int || is_chr) && right_length)) {
    return(invisible())
  }

  if (scalar && (rlang::is_integerish(x) || is.character(x)) && length(x) != 1L) {
    abort_bad_input(
      format_styled(
        "{.arg {x_name}} must be a single index, ",
        "not a length {length(x)} vector."
      ),
      error_call = error_call
    )
  }

  if (rlang::is_integerish(x) && !anyNA(x) && any(x < 1)) {
    abort_bad_input(
      c(
        format_styled("{.arg {x_name}} must be {qty(length(x))}{?a positive index/positive indices}."),
        x = format_styled("{.arg {x_name}} is less than 1 <<fmt_at_locs(x < 1)>>.")
      ),
      error_call = error_call
    )
  }

  if ((rlang::is_integerish(x) || is.character(x)) && anyNA(x)) {
    abort_bad_input(
      c(
        format_styled("{.arg {x_name}} must not contain missing values."),
        x = format_styled("{.arg {x_name}} is {.val {NA}} at <<fmt_at_locs(is.na(x))>>.")
      ),
      error_call = error_call
    )
  }

  abort_bad_input(
    format_styled(
      "{.arg {x_name}} must be {qty(if (scalar) 1L else 2L)}{?an index (a position or a name)/indices (positions or names)}, ",
      "not <<fmt_r_type(x)>>."
    ),
    error_call = error_call
  )
}

assert_is_chr <- function(
  x,
  complete = FALSE,
  maybe = FALSE,
  x_name = rlang::caller_arg(x),
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

assert_is_simple_vector <- function(
  x,
  maybe = FALSE,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if ((maybe && is.null(x)) || vctrs::obj_is_vector(x) && !is.list(x)) {
    return(invisible())
  }

  abort_bad_input(
    format_styled(
      "{.arg {x_name}} must be a non-data.frame, non-list vector, ",
      "not <<fmt_r_type(x)>>."
    ),
    error_call = error_call
  )
}

assert_is_size <- function(
  x,
  size,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (!vctrs::obj_is_vector(x)) {
    abort_bad_input(
      format_styled("{.arg {x_name}} must be a vector, not <<fmt_r_type(x)>>."),
      error_call = error_call
    )
  }

  if (vctrs::vec_size(x) == size) {
    return(invisible())
  }

  abort_bad_input(
    c(
      format_styled("{.arg {x_name}} must be size {size}."),
      x = format_styled("{.arg {x_name}} is size {vctrs::vec_size(x)}.")
    ),
    error_call = error_call
  )
}

assert_match <- function(
  x,
  match,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (x %in% match) {
    return(NULL)
  }

  what <- fmt_vec_collapse(
    match,
    n_chr_max = 120,
    n_elm_max = 120,
    last = " or "
  )
  abort_bad_input(
    format_styled("{.arg {x_name}} must be one of <<what>>, not <<fmt_vec(x)>>."),
    error_call = error_call
  )
}

assert_named <- function(
  x,
  unique = FALSE,
  x_name = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  names <- rlang::names2(x)
  if (!any(names == "")) {
    if (!unique || !anyDuplicated(names)) {
      return(invisible())
    }

    abort_bad_input(
      format_styled(
        "{.arg {x_name}} must have unique names, ",
        "but has duplicate names <<fmt_at_locs(duplicated(names))>>."
      ),
      error_call = error_call
    )
  }

  must <- if (unique) "be uniquely named" else "be named"
  if (length(names) == 1L && is.null(names(x))) {
    message <- "{.arg {x_name}} <<must>>, but has no names attribute."
  } else {
    message <- "{.arg {x_name}} <<must>>, but is unnamed <<fmt_at_locs(names == '')>>."
  }
  abort_bad_input(format_styled(message), error_call = error_call)
}

# abort ------------------------------------------------------------------------

abort_mistyped_arg <- function(
  message,
  parent = NULL,
  error_call = rlang::caller_env()
) {
  rlang::abort(
    message = message,
    class = c("type_error_mistyped_arg", "type_error_mistyped", "type_error"),
    call = error_call,
    parent = parent
  )
}

abort_mistyped_obj <- function(
  message,
  parent = NULL,
  error_call = rlang::caller_env()
) {
  rlang::abort(
    message = message,
    class = c("type_error_mistyped_obj", "type_error_mistyped", "type_error"),
    call = error_call,
    parent = parent
  )
}

abort_bad_vec_type <- function(
  x,
  what,
  complete = FALSE,
  maybe = FALSE,
  x_name = rlang::caller_arg(x),
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

#nocov end
