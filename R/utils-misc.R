`%notin%` <- Negate(`%in%`)

# Borrowed with thanks from {rlang}:
# https://github.com/r-lib/rlang/blob/41144247f88f75ca50b9dde0431bbe54fde791fa/R/standalone-obj-type.R#L275
obj_oo_type <- function(obj) {
  if (!is.object(obj)) {
    return("bare")
  }
  class <- inherits(obj, c("R6", "S7_object"), which = TRUE)
  if (class[[1]]) {
    "R6"
  } else if (class[[2]]) {
    "S7"
  } else if (isS4(obj)) {
    "S4"
  } else {
    "S3"
  }
}
