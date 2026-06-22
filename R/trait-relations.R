# todos ------------------------------------------------------------------------

# TODO:
# - exclusive(..., .missing) -> All but one of objects must be identical to `.missing`
#   - `.missing` may be "na", "null", or "missing", defaults to "null" for objects
#     and `missing` for arguments
#
# - recyclable(...) -> Like `same_sized()`, but for recycling

# relation ---------------------------------------------------------------------

is_relation <- function(x) {
  inherits(x, "type_relation")
}

new_elms_relation <- function(trait) {
  structure(
    list(trait = trait),
    class = c("type_elms_relation", "type_relation")
  )
}

new_args_relation <- function(call, args, description) {
  structure(
    list(call = call, args = args, description = description),
    class = c("type_args_relation", "type_relation")
  )
}

# TODO: Document
#' @export
has_relation <- function(type, relation) {
  context_local("has_relation")
  assert_is_type(type)
  assert_is_relation(relation)

  type |> add_trait(relation$trait)
}

# same_classed -----------------------------------------------------------------

# TODO: Document
#' @export
same_classed <- function(...) {
  if (context_active("has_relation")) {
    return(same_classed_elms(...))
  }
  if (context_active("typed")) {
    return(same_classed_args(...))
  }
  context_abort(format_styled(
    "Must be used while defining a relation, ",
    "e.g. in {.fn has_relation} or {.fn typed}."
  ))
}

same_classed_elms <- function(..., error_call = rlang::caller_env()) {
  context_local("relation")
  selectors <- flatten_selectors(list(...), error_call = error_call)
  if (length(selectors) == 0L) {
    abort_bad_input(
      "Must supply at least one selector to {.arg ...}.",
      error_call = error_call
    )
  }
  new_elms_relation(same_classed_trait(selectors = selectors))
}

same_classed_args <- function(..., error_call = caller_env()) {
  dots <- rlang::enexprs(...)
  check_relation_dots(dots, error_call = error_call)
  args <- as.character(dots)
  new_args_relation(
    call = rlang::call2("inline_assert_same_classed", !!!dots, .ns = "type"),
    args = args,
    description = format_styled("{oxford(backtick(args))} must have the same class.")
  )
}

same_classed_trait <- new_trait("same_classed", params = c("selectors"))

method(trait_test, same_classed_trait) <- function(trait, obj) {
  values <- on_obj_values(obj, trait@selectors)
  if (length(values) <= 1L) return(TRUE)

  class_1 <- class(values[[1]])
  all(map_lgl(values[-1], \(v) identical(class(v), class_1)))
}

method(trait_diagnose, same_classed_trait) <- function(
  trait,
  obj,
  obj_name
) {
  values <- on_obj_values(obj, trait@selectors)
  labels <- on_obj_labels(obj, obj_name, trait@selectors)

  is_error <- map_lgl(values, rlang::is_error)
  if (any(is_error)) {
    bad_at <- which.max(is_error)
    return(c(
      x = format_styled("{labels[[bad_at]]} must return a value, not raise an error.")
    ))
  }

  if (length(values) <= 1L) return(NULL)

  classes <- map(values, class)
  class_1 <- classes[[1]]
  wrong <- map_lgl(classes[-1], \(cls) !identical(cls, class_1))
  if (!any(wrong)) return(NULL)

  bad_at <- 1L + which.max(wrong)
  c(
    i = format_styled("{labels[[1]]} and {labels[[bad_at]]} must have the same class."),
    x = format_styled(
      "{labels[[1]]} has class {.cls {class_1}} and {labels[[bad_at]]} has class {.cls {classes[[bad_at]]}}."
    )
  )
}

method(trait_describe, same_classed_trait) <- function(trait, obj_name) {
  descriptions <- map_chr(
    trait@selectors,
    \(selector) selector@describer(obj_name, NULL)
  )
  format_styled("{str_upper1(oxford(descriptions))} have the same class.")
}

# TODO: Document
#' @export
inline_assert_same_classed <- function(..., error_call = rlang::caller_env()) {
  labels <- backtick(as.character(rlang::enexprs(...)))
  dots <- list(...)
  if (length(dots) == 0L) return(invisible())

  class_1 <- class(dots[[1]])
  same_class <- map_lgl(dots[-1], \(elm) identical(class(elm), class_1))
  if (all(same_class)) return(invisible())

  bad_at <- which.min(same_class) + 1L
  classes <- map(dots, class)
  abort_mistyped_arg(
    c(
      format_styled("{qty(labels)}Argument{?s} {oxford(labels)} must have the same class."),
      x = format_styled(
        "{labels[1]} has class {.cls {class_1}} and {labels[bad_at]} has class {.cls {classes[bad_at]}}."
      )
    ),
    error_call = error_call
  )
}

# same_sized -------------------------------------------------------------------

# TODO: Document
#' @export
same_sized <- function(...) {
  if (context_active("has_relation")) {
    return(same_sized_elms(...))
  }
  if (context_active("typed")) {
    return(same_sized_args(...))
  }
  context_abort(format_styled(
    "Must be used while defining a relation, ",
    "e.g. in {.fn has_relation} or {.fn typed}."
  ))
}

same_sized_elms <- function(..., error_call = rlang::caller_env()) {
  context_local("relation")
  selectors <- flatten_selectors(list(...), error_call = error_call)
  if (length(selectors) == 0L) {
    abort_bad_input(
      "Must supply at least one selector to {.arg ...}.",
      error_call = error_call
    )
  }
  new_elms_relation(same_sized_trait(selectors = selectors))
}

same_sized_args <- function(..., error_call = caller_env()) {
  dots <- rlang::enexprs(...)
  check_relation_dots(dots, error_call = error_call)
  args <- as.character(dots)
  new_args_relation(
    call = rlang::call2("inline_assert_same_sized", !!!dots, .ns = "type"),
    args = args,
    description = format_styled("{oxford(backtick(args))} must be the same size.")
  )
}

same_sized_trait <- new_trait("same_sized", params = c("selectors"))

method(trait_test, same_sized_trait) <- function(trait, obj) {
  values <- on_obj_values(obj, trait@selectors)
  if (!all(map_lgl(values, vctrs::obj_is_vector))) {
    return(FALSE)
  }
  
  sizes <- map_int(values, vctrs::vec_size)
  all(sizes == sizes[[1]])
}

method(trait_diagnose, same_sized_trait) <- function(
  trait,
  obj,
  obj_name
) {
  values <- on_obj_values(obj, trait@selectors)
  labels <- on_obj_labels(obj, obj_name, trait@selectors)

  is_error <- map_lgl(values, rlang::is_error)
  if (any(is_error)) {
    bad_at <- which.max(is_error)
    return(c(
      x = format_styled("{labels[[bad_at]]} must return a value, not raise an error.")
    ))
  }

  is_vector <- map_lgl(values, vctrs::obj_is_vector)
  if (!all(is_vector)) {
    bad_at <- which.min(is_vector)
    return(c(
      x = format_styled("{labels[[bad_at]]} must be a vector, not <<fmt_r_type(values[[bad_at]])>>.")
    ))
  }

  sizes <- map_int(values, vctrs::vec_size)
  bad_at <- which.max(sizes != sizes[[1]])
  c(
    i = format_styled("{labels[[1]]} and {labels[[bad_at]]} must be the same size."),
    x = format_styled(
      "{labels[[1]]} is size {sizes[[1]]} and {labels[[bad_at]]} is size {sizes[[bad_at]]}."
    )
  )
}

method(trait_describe, same_sized_trait) <- function(trait, obj_name) {
  descriptions <- map_chr(
    trait@selectors, 
    \(selector) selector@describer(obj_name, NULL)
  )
  format_styled("{str_upper1(oxford(descriptions))} are the same size.")
}

# TODO: Document
#' @export
inline_assert_same_sized <- function(..., error_call = rlang::caller_env()) {
  labels <- backtick(as.character(rlang::enexprs(...)))
  dots <- list(...)
  if (length(dots) == 0L) return(invisible())

  sizes <- map_int(dots, \(arg) {
    if (vctrs::obj_is_vector(arg)) vctrs::vec_size(arg) else NA_integer_
  })
  if (!anyNA(sizes) && (length(sizes) == 1L || all(sizes[-1] == sizes[[1]]))) {
    return(invisible())
  }

  if (anyNA(sizes)) {
    bad_at <- which.max(is.na(sizes))
    abort_mistyped_arg(
      c(
        format_styled("{qty(labels)}Argument{?s} {oxford(labels)} must be {?a vector/vectors}."),
        x = format_styled("{labels[[bad_at]]} is <<fmt_r_type(dots[[bad_at]])>>.")
      ),
      error_call = error_call
    )
  }

  bad_at <- which.max(sizes[[1]] != sizes)
  abort_mistyped_arg(
    c(
      format_styled("{qty(labels)}Argument{?s} {oxford(labels)} must be the same size."),
      x = format_styled(
        "{labels[[1]]} is size {sizes[[1]]} and {labels[[bad_at]]} is size {sizes[[bad_at]]}."
      )
    ),
    error_call = error_call
  )
}

# helpers ----------------------------------------------------------------------

on_obj_values <- function(obj, selectors) {
  vctrs::list_unchop(map(
    selectors,
    \(selector) {
      errored <- FALSE
      value <- rlang::try_fetch(
        selector@accessor(obj), 
        error = function(cnd) {
          errored <<- TRUE
          cnd
        }
      )

      # `plural` selectors return a list of values to unpack, while the return
      # of non-plural selectors should be interpretted as a single value
      if (errored || !selector@plural) {
        return(list(value))
      } else {
        value
      }
    }
  ))
}

on_obj_labels <- function(obj, obj_name, selectors) {
  unlist(map(selectors, \(selector) selector@labeller(obj_name, obj)))
}

check_relation_dots <- function(dots, error_call = caller_env()) {
  if (rlang::is_empty(dots)) {
    abort_bad_input(
      "Must supply at least one argument to {.arg ...}.",
      error_call = error_call
    )
  }
  for (i in seq_along(dots)) {
    assert_is_symbol(dots[[i]], x_name = paste0("..", i), error_call = error_call)
  }
}
