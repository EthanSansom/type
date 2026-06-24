#nocov start

context_env <- rlang::new_environment()

context_local <- function(name, frame = rlang::caller_env()) {
  old <- context_env[[name]]
  context_env[[name]] <- TRUE
  expr <- rlang::expr(on.exit(
    context_env[[!!name]] <- !!old,
    add = TRUE,
    after = TRUE
  ))
  rlang::eval_bare(expr, frame)
  invisible()
}

context_active <- function(name) {
  rlang::is_true(context_env[[name]])
}

context_assert <- function(
  names, 
  error_message,
  error_call = rlang::caller_env()
) {
  if (!any(map_lgl(names, context_active))) {
    abort_bad_input(error_message, error_call = error_call)
  }
  invisible()
}

context_abort <- function(error_message, error_call = rlang::caller_env()) {
  abort_bad_input(error_message, error_call = error_call)
}

#nocov end
