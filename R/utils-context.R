# TODO: We'll need a robust way to find where we are, look at how {dplyr} does
# it's masking. We can probably depend on {withr} if needed.

# Borrowed from {dplyr} with much gratitude:
# https://github.com/tidyverse/dplyr/blob/main/R/context.R

context_env <- rlang::new_environment()

context_poke <- function(name, value) {
  old <- context_env[[name]]
  context_env[[name]] <- value
  old
}

context_peek_bare <- function(name) {
  context_env[[name]]
}

# TODO: Update to use our cli_abort-based error system
context_peek <- function(name, location, call = rlang::caller_env()) {
  context_peek_bare(name) %||%
    rlang::abort(
      glue::glue("Must only be used inside {location}."),
      call = call
    )
}

context_local <- function(name, value, frame = rlang::caller_env()) {
  old <- context_poke(name, value)
  expr <- rlang::expr(on.exit(
    context_poke(!!name, !!old),
    add = TRUE,
    after = TRUE
  ))
  rlang::eval_bare(expr, frame)

  value
}
