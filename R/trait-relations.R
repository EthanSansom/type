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

#' Add a between-element constraint to a type
#'
#' @description
#'
#' `has_relation()` returns a copy of `type` that requires a relationship to 
#' hold between selected parts of an object. Parts are selected using selector
#' functions (see [on]) and the relationship is expressed as a relation 
#' (see [same_sized()], [same_classed()], [exclusive()]).
#'
#' ```r
#' # Require that elements `[[1]]` and `[[2]]` are the same size
#' t_any |> has_relation(same_sized(on_elm(1L), on_elm(2L)))
#'
#' # Require that attributes "x" and "y" have the same class
#' t_any |> has_relation(same_classed(on_attr("x"), on_attr("y")))
#' 
#' # Require that only one of attributes "x" and "y" are non-NULL
#' t_any |> has_relation(exclusive(on_attrs(c("x", "y")))
#' ```
#'
#' `has()` and `has_relation()` can be composed:
#'
#' ```r
#' t_coords <- t_list |>
#'   has(on_elm("lat"), t_dbl |> bounded(-90, 90)) |>
#'   has(on_elm("lon"), t_dbl |> bounded(-180, 180)) |>
#'   has_relation(same_sized(on_elm("lat"), on_elm("lon")))
#' ```
#'
#' @param type 
#' 
#' A type.
#' 
#' @param relation 
#' 
#' A relation, e.g. the result of [same_sized()], [same_classed()], or [exclusive()].
#'
#' @returns 
#' 
#' A copy of `type` with an additional between-element constraint.
#'
#' @seealso [has()] to add per-element constraints.
#'
#' @examples
#' # Require that elements `[[1]]` and `[[2]]` are the same size
#' t <- t_any |> has_relation(same_sized(on_elm(1L), on_elm(2L)))
#' obj_inspect_type(list(1:3, 1:3), t)
#' obj_inspect_type(list(1:3, 1:2), t)
#'
#' # Require that attributes "x" and "y" have the same class
#' t <- t_any |> has_relation(same_classed(on_attr("x"), on_attr("y")))
#' good <- structure(list(), x = 1L, y = 2L)
#' bad <- structure(list(), x = 1L, y = "a")
#' obj_inspect_type(good, t)
#' obj_inspect_type(bad, t)
#' 
#' @export
has_relation <- function(type, relation) {
  context_local("has_relation")
  assert_is_type(type)
  assert_is_relation(relation)

  type |> add_trait(relation$trait)
}

# same_classed -----------------------------------------------------------------

#' Require elements to have the same class
#'
#' @description
#' 
#' `same_classed()` constrains a set of values to share the same class. Its
#' behaviour depends on context:
#'
#' - Inside [has_relation()]: requires that the selected elements or attributes of an object all have the same class.
#' - Inside [typed()]: requires that the specified function arguments all have the same class at call time.
#' 
#' @param ...
#' 
#' Inside [has_relation()], one or more selectors (e.g. [on_elm()],
#'[on_elms()], [on_attr()], [on()]).
#' 
#' Inside [typed()], one or more argument names.
#'
#' @examples
#' # Require that elements `[[1]]` and `[[2]]` share a class
#' t <- t_any |> has_relation(same_classed(on_elm(1L), on_elm(2L)))
#' obj_inspect_type(list(1L, 2L), t) 
#' obj_inspect_type(list(1L, "a"), t)
#'
#' # Require that arguments `x` and `y` share a class
#' f <- typed(same_classed(x, y), function(x, y) { NULL })
#' f(1L, 2L)
#' try(f(1L, "a"))
#' 
#' @seealso [typed()] for typed function construction, [has_relation()] for
#' adding between-element type constraints.
#'
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
      format_styled("Must supply at least one selector to {.arg ...}."),
      error_call = error_call
    )
  }
  new_elms_relation(same_classed_trait(selectors = selectors))
}

same_classed_args <- function(..., error_call = rlang::caller_env()) {
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

#' @rdname inlined-functions
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

#' Require elements to be the same size
#'
#' @description
#'
#' `same_sized()` constrains a set of values to share the same size, checked
#' via [vctrs::vec_size()]. Its behaviour depends on context:
#'
#' - Inside [has_relation()]: requires that the selected elements or attributes of an object all have the same size.
#' - Inside [typed()]: requires that the specified function arguments all have the same size at call time.
#'
#' @param ...
#'
#' Inside [has_relation()], one or more selectors (e.g. [on_elm()],
#' [on_elms()], [on_attr()], [on()]).
#'
#' Inside [typed()], one or more argument names.
#'
#' @examples
#' # Require that elements `[["a"]]` and `[["b"]]` are the same size
#' t <- t_any |> has_relation(same_sized(on_elm("a"), on_elm("b")))
#' obj_inspect_type(list(a = 1:3, b = 1:3), t, obj_name = "obj")
#' obj_inspect_type(list(a = 1:3, b = 1:2), t, obj_name = "obj")
#' obj_inspect_type(list(1:2, b = 1:2), t, obj_name = "obj")
#'
#' # Require that arguments `x` and `y` are the same size
#' f <- typed(same_sized(x, y), function(x = t_int, y = t_int) { NULL })
#' f(1:3, 1:3)
#' try(f(1:3, 1:2))
#'
#' @seealso [typed()] for typed function construction, [has_relation()] for
#' adding between-element type constraints.
#'
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
      format_styled("Must supply at least one selector to {.arg ...}."),
      error_call = error_call
    )
  }
  new_elms_relation(same_sized_trait(selectors = selectors))
}

same_sized_args <- function(..., error_call = rlang::caller_env()) {
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

method(trait_diagnose, same_sized_trait) <- function(trait, obj, obj_name) {
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

#' @rdname inlined-functions
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

# exlusive ---------------------------------------------------------------------

#' Require exactly one element or argument to be supplied
#'
#' @description
#'
#' `exclusive()` constrains a set of values so that exactly one is non-`NULL`
#' or non-missing. Its behaviour depends on context:
#'
#' - Inside [has_relation()]: requires that exactly one of the selected elements or attributes of an object is non-`NULL`.
#' - Inside [typed()]: requires that exactly one of the function arguments is supplied.
#' 
#' @param ...
#'
#' Inside [has_relation()], one or more selectors (e.g. [on_elm()],
#' [on_elms()], [on_attr()], [on()]).
#'
#' Inside [typed()], one or more argument names.
#'
#' @examples
#' # Require that exactly one of elements `[["x"]]` and `[["y"]]` is non-NULL
#' t <- t_any |> has_relation(exclusive(on_elm("x"), on_elm("y")))
#' obj_inspect_type(list(x = 1L, y = NULL), t)
#' obj_inspect_type(list(x = NULL, y = NULL), t)
#' obj_inspect_type(list(x = 1L, y = 1L), t)
#'
#' # Require that either `x` or `y` is supplied
#' f <- typed(
#'   exclusive(x, y),
#'   function(x = optional(t_any), y = optional(t_any)) { NULL }
#' )
#' f(x = 1L)
#' try(f())
#' try(f(x = 1L, y = "a"))
#'
#' @seealso [typed()] for typed function construction, [has_relation()] for adding between-element type constraints.
#' 
#' @export
exclusive <- function(...) {
  if (context_active("has_relation")) {
    return(exclusive_elms(...))
  }
  if (context_active("typed")) {
    return(exclusive_args(...))
  }
  context_abort(format_styled(
    "Must be used while defining a relation, ",
    "e.g. in {.fn has_relation} or {.fn typed}."
  ))
}

exclusive_elms <- function(..., error_call = rlang::caller_env()) {
  context_local("relation")
  selectors <- flatten_selectors(list(...), error_call = error_call)
  if (length(selectors) == 0L) {
    abort_bad_input(
      format_styled("Must supply at least one selector to {.arg ...}."),
      error_call = error_call
    )
  }
  new_elms_relation(exclusive_trait(selectors = selectors))
}

exclusive_args <- function(..., error_call = rlang::caller_env()) {
  dots <- rlang::enexprs(...)
  check_relation_dots(dots, error_call = error_call)
  args <- as.character(dots)
  new_args_relation(
    call = rlang::call2("inline_assert_exclusive", !!!dots, .ns = "type"),
    args = args,
    description = format_styled("Exactly one of {oxford(backtick(args))} must be supplied.")
  )
}

exclusive_trait <- new_trait("exclusive", params = c("selectors"))

method(trait_test, exclusive_trait) <- function(trait, obj) {
  values <- on_obj_values(obj, trait@selectors)
  sum(!map_lgl(values, is.null)) == 1L
}

method(trait_diagnose, exclusive_trait) <- function(trait, obj, obj_name) {
  values <- on_obj_values(obj, trait@selectors)
  labels <- on_obj_labels(obj, obj_name, trait@selectors)

  is_error <- map_lgl(values, rlang::is_error)
  if (any(is_error)) {
    bad_at <- which.max(is_error)
    return(c(
      x = format_styled("{labels[[bad_at]]} must return a value, not raise an error.")
    ))
  }

  non_null <- !map_lgl(values, is.null)
  n_non_null <- sum(non_null)
  header <- format_styled("Exactly one of {oxford(labels)} must be non-NULL.")

  if (n_non_null == 0L) {
    return(c(
      i = header,
      x = format_styled("Every element is NULL.")
    ))
  }
  c(
    i = header,
    x = format_styled("{oxford(labels[non_null])} are all non-NULL.")
  )
}

method(trait_describe, exclusive_trait) <- function(trait, obj_name) {
  descriptions <- map_chr(
    trait@selectors, 
    \(selector) selector@describer(obj_name, NULL)
  )
  format_styled("Exactly one of {str_upper1(oxford(descriptions))} are non-NULL.")
}

#' @rdname inlined-functions
#' @export
inline_assert_exclusive <- function(..., error_call = rlang::caller_env()) {
  parent_frame <- rlang::caller_env()
  args <- rlang::enexprs(...)
  labels <- backtick(as.character(args))

  non_missing <- map_lgl(args, \(x) rlang::inject(!base::missing(!!x), parent_frame))
  n_non_missing <- sum(non_missing)
  if (n_non_missing == 1L) return(invisible())

  abort_mistyped_arg(
    c(
      format_styled(
        "Exactly one of {qty(length(labels))}argument{?s} {oxford(labels)} must be supplied."
      ),
      x = if (n_non_missing == 0L) {
        format_styled("No arguments were supplied.")
      } else {
        format_styled("{oxford(labels[non_missing])} are all supplied.")
      }
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
      format_styled("Must supply at least one argument to {.arg ...}."),
      error_call = error_call
    )
  }
  for (i in seq_along(dots)) {
    assert_is_symbol(dots[[i]], x_name = paste0("..", i), error_call = error_call)
  }
}
