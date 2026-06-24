# sized ------------------------------------------------------------------------

#' Check size
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

# complete ---------------------------------------------------------------------

#' Check for missing values
#'
#' @description
#' 
#' `complete()` returns a copy of `type` that requires objects to contain
#' no missing (i.e. `NA`) values, checked via [vctrs::vec_any_missing()].
#'
#' @param type A type.
#'
#' @return A copy of `type` with an additional non-missingness constraint.
#'
#' @examples
#' t_real <- t_dbl |> complete()
#' obj_inspect_type(10.6, t_real)
#' obj_inspect_type(c(1, NA), t_real)
#'
#' @export
complete <- function(type) {
  assert_is_type(type)

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }
  type |>
    add_trait(complete_trait())
}

complete_trait <- new_trait("complete")

method(trait_test, complete_trait) <- function(trait, obj) {
  !vctrs::vec_any_missing(obj)
}

method(trait_diagnose, complete_trait) <- function(trait, obj, obj_name) {
  missing_at <- vctrs::vec_detect_missing(obj)
  bad <- if (!is.object(obj) && is.list(obj)) "{.val {NULL}}" else "{.val {NA}}"
  at <- fmt_at_locs(missing_at)
  c(
    i = format_styled("{.arg {obj_name}} must not contain missing elements."),
    x = format_styled("{.arg {obj_name}} is <<bad>> <<at>>.")
  )
}

method(trait_describe, complete_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} contains no missing values.")
}

# unduplicated -----------------------------------------------------------------

#' Check for duplicate values
#'
#' @description
#' 
#' `unduplicated()` returns a copy of `type` that requires objects to contain
#' no duplicate values, checked via [vctrs::vec_duplicate_any()].
#'
#' @param type A type.
#'
#' @return A copy of `type` with an additional non-duplicate constraint.
#'
#' @examples
#' t_ids <- t_chr |> unduplicated()
#' obj_inspect_type(c("a1", "a2"), t_ids)
#' obj_inspect_type(c("a1", "b1", "b1"), t_ids)
#'
#' @export
unduplicated <- function(type) {
  assert_is_type(type)

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }
  type |>
    add_trait(unduplicated_trait())
}

unduplicated_trait <- new_trait("unduplicated")

method(trait_test, unduplicated_trait) <- function(trait, obj) {
  !vctrs::vec_duplicate_any(obj)
}

method(trait_diagnose, unduplicated_trait) <- function(trait, obj, obj_name) {
  duplicated_at <- vctrs::vec_duplicate_detect(obj)
  at <- fmt_at_locs(duplicated_at)
  c(
    i = format_styled("{.arg {obj_name}} must not contain duplicate elements."),
    x = format_styled("{.arg {obj_name}} contains duplicate elements <<at>>.")
  )
}

method(trait_describe, unduplicated_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} contains no duplicated values.")
}

# bare_typed -------------------------------------------------------------------

#' Check `typeof()`
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
  if (!(typeof %in% valid_typeof)) {
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

# classed ----------------------------------------------------------------------

#' Check `class()`
#'
#' @description
#' 
#' `classed()` returns a copy of `type` that requires objects to inherit from one 
#' or more classes. The `inherits` argument controls whether the object must 
#' inherit from all supplied classes or at least one:
#'
#' ```r
#' # Must inherit from "Date"
#' t_date <- t_any |> classed("Date")
#'
#' # Must inherit from both "POSIXct" and "POSIXt"
#' t_posixct <- t_any |> classed(c("POSIXct", "POSIXt"), inherits = "all")
#'
#' # Must inherit from "Date" or "POSIXct"
#' t_datetime <- t_any |> classed(c("Date", "POSIXct"), inherits = "any")
#' ```
#'
#' @param type 
#' 
#' A type.
#' 
#' @param classes 
#' 
#' A character vector of class names.
#' 
#' @param inherits 
#' 
#' Whether the object must inherit from `"all"` supplied classes or at 
#' least `"any"` one. Defaults to `"all"`.
#'
#' @returns A copy of `type` with an additional class constraint.
#'
#' @seealso [bare_typed()] to constrain `typeof()` rather than `class()`.
#'
#' @examples
#' t_posixct <- t_any |> classed(c("POSIXct", "POSIXt"), inherits = "all")
#' obj_is_type(as.POSIXct("2020-01-01"), t_posixct)
#' obj_is_type(as.Date("2020-01-01"), t_posixct)
#' 
#' @export
classed <- function(type, classes, inherits = "all") {
  assert_is_type(type)
  assert_is_chr(classes, complete = TRUE)
  if (rlang::is_empty(classes)) {
    abort_bad_input(format_styled("{.arg classes} must be non-empty."))
  }
  assert_match(inherits, c("all", "any"))

  type |> add_trait(classed_trait(classes = unique(classes), inherits = inherits))
}

classed_trait <- new_trait("classed", params = c("classes", "inherits"))

method(trait_test, classed_trait) <- function(trait, obj) {
  if (trait@inherits == "any") {
    inherits(obj, trait@classes)
  } else {
   all(map_lgl(trait@classes, \(cls) inherits(obj, cls)))
  }
}

method(trait_diagnose, classed_trait) <- function(trait, obj, obj_name) {
  classes <- trait@classes

  if (trait@inherits == "all") {
    missing <- classes[!map_lgl(classes, \(cls) inherits(obj, cls))]
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must inherit all classes: <<oxford(backtick(classes))>>."
      ),
      x = format_styled(
        "{.arg {obj_name}} does not inherit from <<oxford(backtick(missing))>>."
      )
    ))
  }

  c(
    i = format_styled(
      "{.arg {obj_name}} must inherit from at least one of classes: ",
      "<<oxford(backtick(classes), last = ' or ')>>."
    ),
    x = format_styled(
      "{.arg {obj_name}} has class {.cls <<class(obj)>>}."
    )
  )
}

method(trait_describe, classed_trait) <- function(trait, obj_name) {
  classes <- trait@classes
  inherit_mode <- trait@inherits

  if (inherit_mode == "all" || length(classes) == 1L) {
    format_styled("{.arg {obj_name}} inherits from class <<oxford(backtick(classes))>>.")
  } else {
    format_styled(
      "{.arg {obj_name}} inherits from at least one of classes: ",
      "<<oxford(backtick(classes), last = ' or ')>>."
    )
  }
}

# bounded ----------------------------------------------------------------------

#' Check upper and lower bounds
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

# contains ---------------------------------------------------------------------

#' Check that object is a superset
#'
#' @description
#'
#' `contains()` returns a copy of `type` that requires objects to contain all
#' elements of `values`, checked via [vctrs::vec_in()].
#'
#' @param type 
#' 
#' A type.
#' 
#' @param values 
#' 
#' A non-empty, non-list vector of values that objects must contain.
#'
#' @returns 
#' 
#' A copy of `type` with an additional constraint that objects contain 
#' all elements of `values`.
#'
#' @examples
#' t_rgb <- t_chr |> contains(c("r", "g", "b"))
#' obj_inspect_type(c("r", "g", "b", "a"), t_rgb)
#' obj_inspect_type(c("r", "g"), t_rgb)
#'
#' @export
contains <- function(type, values) {
  assert_is_type(type)
  assert_is_simple_vector(values)
  if (vctrs::vec_size(values) == 0L) {
    abort_bad_input("{.arg values} must be non-empty.")
  }

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }

  type |>
    add_trait(contains_trait(values = unique(values)))
}

contains_trait <- new_trait("contains", params = c("values"))

method(trait_test, contains_trait) <- function(trait, obj) {
  test_vec_set_relation(obj, trait@values, "all_of")
}

method(trait_diagnose, contains_trait) <- function(trait, obj, obj_name) {
  diagnose_vec_set_relation(obj, trait@values, "all_of", obj_name)
}

method(trait_describe, contains_trait) <- function(trait, obj_name) {
  describe_vec_set_relation(obj_name, trait@values, "all_of")
}

# within -----------------------------------------------------------------------

#' Check that object is a subset
#'
#' @description
#'
#' `within()` returns a copy of `type` that requires every element of an object
#' to be within `values`, checked via [vctrs::vec_in()].
#'
#' @param type 
#' 
#' A type.
#' 
#' @param values 
#' 
#' A non-empty, non-list vector of values that elements of object must be within.
#'
#' @returns 
#' 
#' A copy of `type` with an additional constraint that objects are within `values`.
#'
#' @examples
#' t_weekend <- t_chr |> within(c("Sat", "Sun"))
#' obj_inspect_type("Sat", t_weekend)
#' obj_inspect_type(c("A", "B"), t_weekend)
#'
#' @export
within <- function(type, values) {
  assert_is_type(type)
  assert_is_simple_vector(values)
  if (vctrs::vec_size(values) == 0L) {
    abort_bad_input("{.arg values} must be non-empty.")
  }

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }

  type |>
    add_trait(within_trait(values = unique(values)))
}

within_trait <- new_trait("within", params = c("values"))

method(trait_test, within_trait) <- function(trait, obj) {
  test_vec_set_relation(obj, trait@values, "subset_of")
}

method(trait_diagnose, within_trait) <- function(trait, obj, obj_name) {
  diagnose_vec_set_relation(obj, trait@values, "subset_of", obj_name)
}

method(trait_describe, within_trait) <- function(trait, obj_name) {
  describe_vec_set_relation(obj_name, trait@values, "subset_of")
}

# setequal_to ------------------------------------------------------------------

#' Check that object is a given set
#'
#' @description
#'
#' `setequal_to()` returns a copy of `type` that requires an object to be
#' setequal to `values`, checked via [vctrs::vec_in()].
#'
#' @param type 
#' 
#' A type.
#' 
#' @param values 
#' 
#' A non-empty, non-list vector of values that an object must be setequal to.
#'
#' @returns 
#' 
#' A copy of `type` with an additional constraint that objects are setequal to `values`.
#'
#' @examples
#' t_three <- t_int |> setequal_to(1:3)
#' obj_inspect_type(c(1L, 2L, 3L, 1L), t_three)
#' obj_inspect_type(10L, t_three)
#'
#' @export
setequal_to <- function(type, values) {
  assert_is_type(type)
  assert_is_simple_vector(values)
  if (vctrs::vec_size(values) == 0L) {
    abort_bad_input("{.arg values} must be non-empty.")
  }

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }

  type |>
    add_trait(setequal_to_trait(values = unique(values)))
}

setequal_to_trait <- new_trait("setequal_to", params = c("values"))

method(trait_test, setequal_to_trait) <- function(trait, obj) {
  test_vec_set_relation(obj, trait@values, "setequal")
}

method(trait_diagnose, setequal_to_trait) <- function(trait, obj, obj_name) {
  diagnose_vec_set_relation(obj, trait@values, "setequal", obj_name)
}

method(trait_describe, setequal_to_trait) <- function(trait, obj_name) {
  describe_vec_set_relation(obj_name, trait@values, "setequal")
}

# disjoint_to ------------------------------------------------------------------

#' Check that object does not contain values
#'
#' @description
#'
#' `disjoint_to()` returns a copy of `type` that requires an object to be
#' not contain any of `values`, checked via [vctrs::vec_in()].
#'
#' @param type 
#' 
#' A type.
#' 
#' @param values 
#' 
#' A non-empty, non-list vector of values that an object must not contain.
#'
#' @returns 
#' 
#' A copy of `type` with an additional constraint that objects contain
#' none of `values`.
#'
#' @examples
#' t_not_three <- t_int |> setequal_to(1:3)
#' obj_inspect_type(10L, t_not_three)
#' obj_inspect_type(c(1L, 2L, 3L, 1L), t_not_three)
#'
#' @export
disjoint_to <- function(type, values) {
  assert_is_type(type)
  assert_is_simple_vector(values)
  if (vctrs::vec_size(values) == 0L) {
    abort_bad_input("{.arg values} must be non-empty.")
  }

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }

  type |>
    add_trait(disjoint_to_trait(values = unique(values)))
}

disjoint_to_trait <- new_trait("disjoint_to", params = c("values"))

method(trait_test, disjoint_to_trait) <- function(trait, obj) {
  test_vec_set_relation(obj, trait@values, "none_of")
}

method(trait_diagnose, disjoint_to_trait) <- function(trait, obj, obj_name) {
  diagnose_vec_set_relation(obj, trait@values, "none_of", obj_name)
}

method(trait_describe, disjoint_to_trait) <- function(trait, obj_name) {
  describe_vec_set_relation(obj_name, trait@values, "none_of")
}

# internal traits --------------------------------------------------------------

#nocov start

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

# Not implementing as a type union as `is.numeric()` is more lenient than `t_int || t_dbl`
numeric_trait <- new_trait("numeric")

method(trait_test, numeric_trait) <- function(trait, obj) {
  is.numeric(obj)
}

method(trait_diagnose, numeric_trait) <- function(trait, obj, obj_name) {
  c(
    x = format_styled(
      "{.arg {obj_name}} must be a numeric vector, ",
      "not <<fmt_r_type(obj)>>."
    )
  )
}

method(trait_describe, numeric_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is a numeric vector.")
}

#nocov end

# helpers ----------------------------------------------------------------------

is_bare_vector_type <- function(type) {
  vector_types <- c("double", "integer", "logical", "character", "complex", "raw", "list")
  any(map_lgl(
    type@traits, 
    \(trait) S7::S7_inherits(trait, bare_typed_trait) && trait@typeof %in% vector_types
  ))
}
