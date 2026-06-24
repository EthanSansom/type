test_that("complete() errors on invalid inputs", {
  expect_error(10L |> complete(), class = "type_error_bad_input")
})

test_that("complete() type tests and checks work as expected", {
  t <- t_any |> complete()

  expect_true(obj_is_type(1:3, t))
  expect_true(obj_is_type(c("a", "b"), t))
  expect_true(obj_is_type(integer(), t))

  expect_false(obj_is_type(c(1L, NA_integer_), t))
  expect_false(obj_is_type(NA, t))
  expect_false(obj_is_type(mean, t))

  expect_no_error(obj_assert_type(c(1L, 2L, 3L), t))
  expect_error(obj_assert_type(c(1L, NA_integer_), t), class = "type_error_mistyped_obj")
})

test_that("complete() description and diagnosis are as expected", {
  skip_on_covr()

  t <- t_any |> complete()

  expect_snapshot(obj_inspect_type(c(1L, 2L, 3L), t))
  expect_snapshot(obj_inspect_type(c(1L, NA_integer_, 3L, NA_integer_), t))
  expect_snapshot(obj_inspect_type(mean, t))
})
