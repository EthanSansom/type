# sized ------------------------------------------------------------------------

#' Constrain the size of a type
#'
#' @description
#' 
#' `sized()` returns a copy of `type` that requires objects to be
#' size `size`, checked via [vctrs::vec_size()].
#' 
#' ```r
#' obj_is_type(1, t_any |> sized(2L))   # FALSE
#' obj_is_type(1:2, t_any |> sized(2L)) # TRUE
#' ```
#'
#' @param type A type.
#' @param size A non-negative count.
#'
#' @return A copy of `type` with an additional size constraint.
#'
#' @examples
#' t_int2 <- t_int |> sized(2L)
#' obj_inspect_type(10L, t_int2)
#' obj_inspect_type(1:2, t_int2)
#'
#' @export
sized <- function(type, size) {
  assert_is_type(type)
  assert_is_count(size)

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }
  type |>
    add_trait(sized_trait(size = size))
}

sized_trait <- new_trait("sized", params = c("size"))

method(trait_test, sized_trait) <- function(trait, obj) {
  vctrs::vec_size(obj) == trait@size
}

method(trait_diagnose, sized_trait) <- function(trait, obj, obj_name) {
  c(x = format_styled("{.arg {obj_name}} must be size {trait@size}, not size {vctrs::vec_size(obj)}."))
}

method(trait_describe, sized_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is size {trait@size}.")
}

# bare_typed -------------------------------------------------------------------

#' Constrain the `typeof()` of a type
#'
#' @description
#' 
#' `bare_typed()` returns a copy of `type` that requires objects to be a bare
#' R value with `typeof()` equal to `typeof`. Bare objects are those with no
#' class attribute, for example `1L` or `list()`, but not `data.frame()`.
#' 
#' ```r
#' t_lgl <- t_any |> bare_typed("logical")
#' obj_is_type(1, t_lgl)  # FALSE
#' obj_is_type(NA, t_lgl) # TRUE
#' ```
#'
#' @param type 
#' 
#' A type.
#' 
#' @param typeof 
#' 
#' A string. Must be a one of the valid R types returned by [typeof()]:
#' `"double"`, `"integer"`, `"logical"`, `"character"`, `"complex"`,
#' `"raw"`, `"list"`, `"NULL"`, `"environment"`, `"symbol"`, `"pairlist"`,
#' `"language"`, `"expression"`, `"S4"`, `"closure"`, `"special"`,
#' `"builtin"`, `"externalptr"`, `"weakref"`, `"promise"`, `"char"`, or `"bytecode"`.
#'
#' @return A copy of `type` with an additional `typeof` constraint.
#'
#' @examples
#' t_pairlist <- t_any |> bare_typed("pairlist")
#' obj_inspect_type(10L, t_pairlist)
#' obj_inspect_type(pairlist(), t_pairlist)
#'
#' @export
bare_typed <- function(type, typeof) {
  assert_is_type(type)
  assert_is_string(typeof)

  valid_typeof <- c(
    "double", "integer", "logical", "character", "complex", "raw", "list", "NULL",
    "environment", "symbol", "pairlist", "language", "expression", "S4", "closure",
    "special", "builtin", "externalptr", "weakref", "promise", "char", "bytecode"
  )
  if (typeof %notin% valid_typeof) {
    abort_bad_input(
      format_styled("{.arg typeof} must be a valid R type, not {.val {typeof}}.")
    )
  }

  type |>
    add_trait(bare_typed_trait(typeof = typeof))
}

bare_typed_trait <- new_trait("bare_typed", params = c("typeof"))

method(trait_test, bare_typed_trait) <- function(trait, obj) {
    !is.object(obj) &&
    switch(
      trait@typeof,
      double = is.double(obj),
      integer = is.integer(obj),
      logical = is.logical(obj),
      character = is.character(obj),
      complex = is.complex(obj),
      raw = is.raw(obj),
      list = is.list(obj),
      `NULL` = is.null(obj),
      environment = is.environment(obj),
      symbol = is.symbol(obj),
      pairlist = is.pairlist(obj),
      language = is.language(obj),
      expression = is.expression(obj),
      `S4` = typeof(obj) == "S4",
      closure = typeof(obj) == "closure",
      special = typeof(obj) == "special",
      builtin = typeof(obj) == "builtin",
      externalptr = typeof(obj) == "externalptr",
      weakref = typeof(obj) == "weakref",
      promise = typeof(obj) == "promise",
      char = typeof(obj) == "char",
      bytecode = typeof(obj) == "bytecode"
    )
}

method(trait_diagnose, bare_typed_trait) <- function(trait, obj, obj_name) {
  if (is.object(obj)) {
    c(
      i = format_styled("{.arg {obj_name}} must be a bare {.cls {trait@typeof}}."),
      x = format_styled(
      "{.arg {obj_name}} is a {obj_oo_type(obj)} object of class {.cls {class(obj)}}."
      )
    )
  } else {
    c(
      x = format_styled(
        "{.arg {obj_name}} must be a bare {.cls {trait@typeof}}, ", 
        "not a bare {.cls {typeof(obj)}}."
      )
    )
  }
}

method(trait_describe, bare_typed_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is a bare {.cls {trait@typeof}}.")
}

# bounded ----------------------------------------------------------------------

#' Constrain the upper and lower bounds of a type
#'
#' @description
#' 
#' `bounded()` returns a copy of `type` that requires objects to fall within a
#' range. Bounds are specified by `left` and `right`, and the interval type is 
#' controlled by `bounds`:
#'
#' | `bounds` | Interval     | Condition                |
#' |----------|--------------|--------------------------|
#' | `"[]"`   | Closed       | `left <= x & x <= right` |
#' | `"[)"`   | Right-open   | `left <= x & x < right`  |
#' | `"(]"`   | Left-open    | `left < x & x <= right`  |
#' | `"()"`   | Open         | `left < x & x < right`   |
#'
#' Either `left` or `right` may be omitted to leave that side unbounded:
#'
#' ```r
#' t_dbl |> bounded(0, 1)             # probabilities: [0, 1]
#' t_dbl |> bounded(0, bounds = "()") # strictly positive
#' t_chr |> bounded("m", "p")         # starts with "m", "n", "o", or "p"
#' ```
#'
#' `bounded()` ignores `NA` values when checking bounds. To require
#' non-missingness, combine with the `complete()` trait.
#'
#' @param type 
#' 
#' A type.
#' 
#' @param left,right 
#' 
#' Scalar values giving the left and right bounds. Either may be `NULL` (the default) 
#' to leave that side unbounded. At least one of `left`, `right` must be supplied.
#' 
#' @param bounds 
#' 
#' A string denoting the boundary type: `"[]"` (closed, the default), `"[)"`, `"(]"`, or `"()"` (open).
#'
#' @returns A copy of `type` with an additional bound constraint.
#'
#' @seealso [sized()] for size constraints, [complete()] for non-missingness.
#'
#' @examples
#' t_prob <- t_dbl |> bounded(0, 1)
#' t_positive <- t_dbl |> bounded(0, bounds = "()")
#' 
#' obj_inspect_type(0.5, t_prob)
#' obj_inspect_type(0, t_positive)
#' 
#' # `NA` values are considered within bounds
#' obj_is_type(c(0, NA, 0.75), t_prob)
#' 
#' @export
bounded <- function(type, left = NULL, right = NULL, bounds = "[]") {
  assert_is_type(type)
  if (!is.null(left)) {
    assert_is_simple_vector(left)
    assert_is_size(left, 1L)
  }
  if (!is.null(right)) {
    assert_is_simple_vector(right)
    assert_is_size(right, 1L)
  }
  if (is.null(left) && is.null(right)) {
    abort_bad_input(format_styled("At least one of {.arg left}, {.arg right} must be supplied."))
  }
  assert_match(bounds, c("[]", "[)", "(]", "()"))

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }

  type |>
    add_trait(
      bounded_trait(
        left = left,
        right = right, 
        left_open = substr(bounds, 1, 1) == "(", 
        right_open = substr(bounds, 2, 2) == ")"
      )
    )
}

bounded_trait <- new_trait("bounded", params = c("left", "right", "left_open", "right_open"))

method(trait_test, bounded_trait) <- function(trait, obj) {
  left <- trait@left
  right <- trait@right
  left_open <- trait@left_open
  right_open <- trait@right_open

  bounded_left <- function(x) {
    rlang::is_true(rlang::try_fetch(
      if (left_open) all(x > left, na.rm = TRUE) else all(x >= left, na.rm = TRUE),
      error = identity,
      warning = function(cnd) rlang::cnd_muffle(cnd)
    ))
  }
  bounded_right <- function(x) {
    rlang::is_true(rlang::try_fetch(
      if (right_open) all(x < right, na.rm = TRUE) else all(x <= right, na.rm = TRUE),
      error = identity,
      warning = function(cnd) rlang::cnd_muffle(cnd)
    ))
  }

  (is.null(left) || bounded_left(obj)) && (is.null(right) || bounded_right(obj))
}

method(trait_diagnose, bounded_trait) <- function(trait, obj, obj_name) {
  left <- trait@left
  right <- trait@right
  left_open <- trait@left_open
  right_open <- trait@right_open

  left_bounded <- if (is.null(left)) TRUE else rlang::try_fetch(
    if (left_open) obj > left else obj >= left,
    error = identity,
    warning = function(cnd) rlang::cnd_muffle(cnd)
  )
  right_bounded <- if (is.null(right)) TRUE else rlang::try_fetch(
    if (right_open) obj < right else obj <= right,
    error = identity,
    warning = function(cnd) rlang::cnd_muffle(cnd)
  )

  if (!is.logical(left_bounded)) {
    left_op <- if (left_open) ">" else ">="
    footer <- format_styled("Comparison {.code <<obj_name>> <<left_op>> <<left>>} raised an error.")
  } else if (!is.logical(right_bounded)) {
    right_op <- if (right_open) "<" else "<="
    footer <- format_styled("Comparison {.code <<obj_name>> <<right_op>> <<right>>} raised an error.")
  } else {
    out_of_bounds <- !(left_bounded & right_bounded)
    footer <- format_styled("{.arg {obj_name}} is out of bounds <<fmt_at_locs(out_of_bounds)>>.")
  }

  c(
    i = format_styled("{.arg {obj_name}} must be <<bounds_label(left, right, left_open, right_open)>>."),
    x = footer
  )
}

method(trait_describe, bounded_trait) <- function(trait, obj_name) {
  left <- trait@left
  right <- trait@right
  left_open <- trait@left_open
  right_open <- trait@right_open
  format_styled("{.arg {obj_name}} is <<bounds_label(left, right, left_open, right_open)>>.")
}

bounds_label <- function(left, right, left_open, right_open) {
  has_left <- !is.null(left)
  has_right <- !is.null(right)

  if (has_left && has_right) {
    l_bracket <- if (left_open) "(" else "["
    r_bracket <- if (right_open) ")" else "]"
    glue::glue("bounded by {l_bracket}{left}, {right}{r_bracket}")
  } else if (has_left) {
    if (left_open) glue::glue("above {left}") else glue::glue("equal to or above {left}")
  } else {
    if (right_open) glue::glue("below {right}") else glue::glue("equal to or below {right}")
  }
}

# vector -----------------------------------------------------------------------

vector_trait <- new_trait("vector")

method(trait_test, vector_trait) <- function(trait, obj) {
  vctrs::obj_is_vector(obj)
}

method(trait_diagnose, vector_trait) <- function(trait, obj, obj_name) {
  c(
    x = format_styled(
      "{.arg {obj_name}} must be a {.pkg vctrs} style vector, ",
      "not <<fmt_r_type(obj)>>."
    )
  )
}

method(trait_describe, vector_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is a {.pkg vctrs} style vector.")
}

# helpers ----------------------------------------------------------------------

is_bare_vector_type <- function(type) {
  vector_types <- c("double", "integer", "logical", "character", "complex", "raw", "list")
  any(map_lgl(
    type@traits, 
    \(trait) S7::S7_inherits(trait, bare_typed_trait) && trait@typeof %in% vector_types
  ))
}
