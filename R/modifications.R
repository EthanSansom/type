# modifiers --------------------------------------------------------------------

# TODO: Document
#' @export
const <- function(type) {
  context_assert("%:%", format_styled("Must be used in object typing, e.g. within {.code %:%}."))
  type |> add_modification("const")
}

# TODO: Document
#' @export
optional <- function(type) {
  context_assert("typed", format_styled("Must be used in function typing, e.g. within {.fn typed}."))
  type |> add_modification("optional")
}

# TODO: Document
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
