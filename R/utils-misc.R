unattr <- function(x) {
  attributes(x) <- NULL
  x
}

method_exists <- function(generic, object = NULL, class = NULL) {
  is.function(try(S7::method(generic, class = class, object = object)))
}

`%notin%` <- function(lhs, rhs) !(lhs %in% rhs)
