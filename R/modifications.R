# modifiers --------------------------------------------------------------------

#' Modify a type
#'
#' @description
#' Type modifiers adjust how a type is enforced in a given context:
#'
#' - `const(type)`: For use in `%:%`. Declares a constant.
#' - `optional(type)`: For use in [typed()]. Allows an argument to be missing.
#' - `maybe(type)`: For use in [typed()]. Allows an argument to be `NULL`.
#'
#' @param type A type, e.g. [t_int].
#'
#' @return A modified copy of `type`.
#'
#' @examples
#' # const() variables can't be assigned to
#' const(t_int) %:% x(1L)
#' x
#' try(x <- 2L)
#'
#' # optional() arguments may be missing
#' f <- typed(function(x = optional(t_int)) {
#'   if (missing(x)) "absent" else x)
#' }
#' f()
#' f(1L)
#'
#' # maybe() arguments may be `NULL`
#' g <- typed(function(x = maybe(t_int)) x)
#' g(NULL)
#' g(1L)
#'
#' @name modifiers
NULL

#' @rdname modifiers
#' @export
const <- function(type) {
  context_assert("%:%", format_styled("Must be used in object typing, e.g. within {.code %:%}."))
  type |> add_modification("const")
}

#' @rdname modifiers
#' @export
optional <- function(type) {
  context_assert("typed", format_styled("Must be used in function typing, e.g. within {.fn typed}."))
  type |> add_modification("optional")
}

#' @rdname modifiers
#' @export
maybe <- function(type) {
  context_assert("typed", format_styled("Must be used in function typing, e.g. within {.fn typed}."))
  type |> add_modification("maybe")
}

# This is an internal only modification for the `...` argument of functions
endotted <- function(type) {
  type |> add_modification("endotted")
}

# helpers ----------------------------------------------------------------------

add_modification <- function(type, modification) {
  type@modifications <- c(type@modifications, modification)
  type
}

has_modifications <- function(type) {
  !rlang::is_empty(type@modifications)
}
