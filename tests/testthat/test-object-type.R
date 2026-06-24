# is_type ----------------------------------------------------------------------

test_that("is_type() works as expected", {
  expect_true(is_type(t_int))
  expect_true(is_type(t_any))
  expect_true(is_type(t_int |> sized(1L)))

  expect_false(is_type(1L))
  expect_false(is_type("a"))
  expect_false(is_type(NULL))
  expect_false(is_type(list()))
})

# obj_is_type ------------------------------------------------------------------

test_that("obj_is_type() errors when type is not a type", {
  expect_error(obj_is_type(1L, 42L), class = "type_error_bad_input")
})

test_that("obj_is_type() works as expected", {
  expect_true(obj_is_type(1L, t_int))
  expect_true(obj_is_type(1L, t_any))
  expect_true(obj_is_type(1L, t_int |> sized(1L)))

  expect_false(obj_is_type("a", t_int))
  expect_false(obj_is_type(1:2, t_int |> sized(1L)))
})

# obj_assert_type --------------------------------------------------------------

test_that("obj_assert_type() errors when type is not a type", {
  expect_error(obj_assert_type(1L, 42L), class = "type_error_bad_input")
})

test_that("obj_assert_type() works as expected", {
  expect_invisible(obj_assert_type(1L, t_int))

  expect_error(obj_assert_type("a", t_int), class = "type_error_mistyped_obj")
  expect_error(obj_assert_type(1:2, t_int |> sized(1L)), class = "type_error_mistyped_obj")
})

# obj_inspect_type() -----------------------------------------------------------

test_that("obj_inspect_type() errors when type is not a type", {
  expect_error(obj_inspect_type(1L, 42L), class = "type_error_bad_input")
})

test_that("obj_inspect_type() works as expected", {
  skip_on_covr()

  expect_snapshot(obj_inspect_type(1L, t_int))
  expect_snapshot(obj_inspect_type("a", t_int))
  expect_snapshot(obj_inspect_type(1L, t_int |> sized(2L)))
})
