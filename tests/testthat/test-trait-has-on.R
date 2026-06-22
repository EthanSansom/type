# on ---------------------------------------------------------------------------

test_that("on() errors if input is not a simple call or symbol", {
  expect_error(
    has(t_any, on(42L), t_int),
    class = "type_error_bad_input"
  )
  expect_error(
    has(t_any, on(bar$foo(.x)), t_int),
    class = "type_error_bad_input"
  )
})

test_that("on() errors when used in invalid contexts", {
  expect_error(
    on(names(.x)),
    class = "type_error_bad_input"
  )
})

test_that("on() symbol shorthand wraps symbol as a call", {
  t <- t_any |> has(on(names), t_null)
  expect_true(obj_is_type(1:3, t))
  expect_false(obj_is_type(list(x = 1), t))
})

test_that("on() accessor errors are treated as failed type checks", {
  t <- t_any |> has(on(stop("AH")), t_int)
  expect_false(obj_is_type(1L, t))
  expect_error(obj_assert_type(1L, t), class = "type_error_mistyped_obj")
})

# on_elm -----------------------------------------------------------------------

test_that("on_elm errors when used in invalid contexts", {
  expect_error(on_elm(1L), class = "type_error_bad_input")
})

test_that("on_elm() errors on invalid index", {
  expect_error(
    has(t_any, on_elm(0L), t_int),
    class = "type_error_bad_input"
  )
  expect_error(
    has(t_any, on_elm(1.5), t_int),
    class = "type_error_bad_input"
  )
  expect_error(
    has(t_any, on_elm(NULL), t_int),
    class = "type_error_bad_input"
  )
})

test_that("on_elm() type tests and checks work as expected", {
  t1 <- t_any |> has(on_elm(1L), t_int)
  expect_true(obj_is_type(list(1L, "a"), t1))
  expect_false(obj_is_type(list("a", 1L), t1))
  expect_no_error(obj_assert_type(list(1L), t1))
  expect_error(obj_assert_type(list("a"), t1), class = "type_error_mistyped_obj")

  t2 <- t_any |> has(on_elm("x"), t_int)
  expect_true(obj_is_type(list(x = 1L, y = "a"), t2))
  expect_false(obj_is_type(list(x = "a"), t2))
  expect_error(obj_assert_type(list(x = "a"), t2), class = "type_error_mistyped_obj")
})

# on_attr ----------------------------------------------------------------------

test_that("on_attr() errors when used in invalid contexts", {
  expect_error(on_attr("dim"), class = "type_error_bad_input")
})

test_that("on_attr() errors on non-string name", {
  expect_error(
    has(t_any, on_attr(1L), t_int),
    class = "type_error_bad_input"
  )
  expect_error(
    has(t_any, on_attr(c("a", "b")), t_int),
    class = "type_error_bad_input"
  )
})

test_that("on_attr() type tests and checks work as expected", {
  t <- t_any |> has(on_attr("dim"), t_int)
 
  expect_true(obj_is_type(matrix(1:4, 2, 2), t))
  expect_false(obj_is_type(1:4, t))
  expect_no_error(obj_assert_type(matrix(1:4, 2, 2), t))
  expect_error(obj_assert_type(1:4, t), class = "type_error_mistyped_obj")
})

# has --------------------------------------------------------------------------

test_that("has() errors on invalid inputs", {
  expect_error(
    has(42L, on(length(.x)), t_int),
    class = "type_error_bad_input"
  )
  expect_error(
    has(t_any, on(length(.x)), 3L),
    class = "type_error_bad_input"
  )
  expect_error(
    has(t_any, function(x) { length(x) }, t_int),
    class = "type_error_bad_input"
  )
})

test_that("has() chained traits are both checked", {
  t <- t_any |>
    has(on_elm(1L), t_int) |>
    has(on_elm(2L), t_chr)

  expect_true(obj_is_type(list(1L, "a"), t))
  expect_false(obj_is_type(list("a", "b"), t)) # First trait fails
  expect_false(obj_is_type(list(1L, 2L), t))   # Second trait fails
})

test_that("has() description and diagnosis are as expected", {
  t1 <- t_any |> has(on_elm(1L), t_int)
  t2 <- t_any |> has(on(names), t_chr |> sized(2L))
  expect_snapshot(obj_inspect_type(list(x = 10L), t1))
  expect_snapshot(obj_inspect_type(mean, t1))
  expect_snapshot(obj_inspect_type(list(x = 1, y = 2), t2))
  expect_snapshot(obj_inspect_type(list(x = 1), t2))
})
