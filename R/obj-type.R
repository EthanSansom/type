# obj_is_type ------------------------------------------------------------------

obj_is_type <- function(obj, type) {
  type_test(type, obj)
}

# obj_assert_type --------------------------------------------------------------

# TODO: Very minimal prototype, improve later
obj_assert_type <- function(
  obj,
  type,
  obj_name = rlang::caller_arg(obj),
  error_call = rlang::caller_env(),
  error_class = character()
) {
  mistyped_message <- type_absent_message(type, obj, obj_name)
  if (is.null(mistyped_message)) {
    return(obj)
  }

  obj_name <- cli_escape(obj_name)
  rlang::abort(
    message = c(
      format_styled("Object {.arg {obj_name}} is mistyped."),
      mistyped_message
    ),
    class = c(error_class, "type_mistyped_error"),
    call = error_call
  )
}

# dots_assert_type -------------------------------------------------------------

# Special type assertion used only on dots
dots_assert_type <- function(
  ...,
  .type,
  .error_call = rlang::caller_env(),
  .error_class = character()
) {
  for (i in rlang::seq2(1, ...length())) {
    obj_assert_type(
      ...elt(i),
      type = .type,
      obj_name = paste0("..", i),
      error_call = .error_call,
      error_class = .error_class
    )
  }
}

# Internal helper, used for dots typed with `t_dots` which are also quoted
# or enexpred. In this case, we can't forward the dots via `...` so we have
# to iterate over the defused `...` expressions instead.
defused_dots_assert_type <- function(
  defused_dots,
  type,
  error_call = rlang::caller_env()
) {
  for (i in seq_along(defused_dots)) {
    obj_assert_type(
      defused_dots[[i]],
      type = type,
      obj_name = paste0("..", i),
      error_call = error_call
    )
  }
}
