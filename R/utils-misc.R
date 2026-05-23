unattr <- function(x) {
  attributes(x) <- NULL
  x
}

`%notin%` <- Negate(`%in%`)
`%anyin%` <- function(lhs, rhs) any(lhs %in% rhs)
