#' @export
inlined_abort_if_mistyped <- function(
  arg_name,
  arg_validation,
  error_call = rlang::caller_env()
) {
  if (is.null(arg_validation)) {
    return(NULL)
  }
  abort_mistyped(
    c(
      format_styled("Argument {.arg {arg_name}} is mistyped."),
      arg_validation
    ),
    error_call = error_call,
    error_subclass = "type_error_mistyped_arg"
  )
}

#' @export
inlined_lazy_validate <- function(...) {
  for (i in rlang::seq2(1, ...length())) {
    if (!is.null(...elt(i))) return(...elt(i))
  }
}
