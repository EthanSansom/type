.LGL <- logical(1)
.INT <- integer(1)
.DBL <- double(1)
.CHR <- character(1)

map <- function(.x, .f, ...) lapply(.x, .f, ...)
map_lgl <- function(.x, .f, ...) vapply(.x, .f, .LGL, ...)
map_int <- function(.x, .f, ...) vapply(.x, .f, .INT, ...)
map_dbl <- function(.x, .f, ...) vapply(.x, .f, .DBL, ...)
map_chr <- function(.x, .f, ...) vapply(.x, .f, .CHR, ...)

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}
map2_lgl <- function(.x, .y, .f, ...) {
  vapply(seq_along(.x), function(i) .f(.x[[i]], .y[[i]], ...), .LGL)
}
map2_int <- function(.x, .y, .f, ...) {
  vapply(seq_along(.x), function(i) .f(.x[[i]], .y[[i]], ...), .INT)
}
map2_dbl <- function(.x, .y, .f, ...) {
  vapply(seq_along(.x), function(i) .f(.x[[i]], .y[[i]], ...), .DBL)
}
map2_chr <- function(.x, .y, .f, ...) {
  vapply(seq_along(.x), function(i) .f(.x[[i]], .y[[i]], ...), .CHR)
}

pmap <- function(.l, .f, ...) .mapply(.f, .l, MoreArgs = list(...))

map_if <- function(.x, .p, .f, ...) {
  matches <- map_lgl(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}

keep <- function(.x, .p, ...) {
  .x[map_lgl(.x, .p, ...)]
}

keep_at <- function(x, at) {
  if (is.function(at)) x[at(x)] else x[at]
}

drop <- function(.x, .p, ...) {
  .x[!map_lgl(.x, .p, ...)]
}

drop_at <- function(x, at) {
  if (is.function(at)) x[!at(x)] else x[!at]
}

list_flatten <- function(x) {
  do.call(c, x)
}