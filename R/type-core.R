# TODO: Document all exported types
#' @export
t_any <- NULL

#' @export
t_null <- NULL

#' @export
t_lgl <- NULL

#' @export
t_bool <- NULL

#' @export
t_int <- NULL

#' @export
t_chr <- NULL

#' @export
t_dots <- NULL

on_load_core_types <- function() {
  t_any <<- type()

  t_null <<- t_any |> bare_typed("NULL")

  t_lgl <<- t_any |> bare_typed("logical")

  t_bool <<- t_lgl |> sized(1L)

  t_int <<- t_any |> bare_typed("integer")

  t_chr <<- t_any |> bare_typed("character")

  t_dots <<- t_any |> endotted()
}
