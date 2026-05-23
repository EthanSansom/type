# TODO: We'll need to revise the context in which these are allowed to run.
#       Also, if we don't allow parameters, we can just store these as strings.
#
# E.g. `optional(t_bool) %:% x(TRUE)` should fail, as `optional()` is allowed
# only in a function typing context.

const <- function(type) {
  new_refined_type(type, modifications = "const")
}

unsafe <- function(type) {
  new_refined_type(type, modifications = "unsafe")
}

optional <- function(type) {
  new_refined_type(type, modifications = "optional")
}

maybe <- function(type) {
  new_refined_type(type, modifications = "maybe")
}

expred <- function(type) {
  new_refined_type(type, modifications = "expred")
}

quoted <- function(type) {
  new_refined_type(type, modifications = "quoted")
}

has_modifications <- function(type) {
  is_refined_type(type) && !rlang::is_empty(type@modifications)
}
