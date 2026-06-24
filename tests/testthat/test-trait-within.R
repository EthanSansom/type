test_that("within() errors on invalid inputs", {
  expect_error(10L |> within(1L), class = "type_error_bad_input")
  expect_error(t_int |> within(list(1, 2)), class = "type_error_bad_input")
  expect_error(t_int |> within(integer()), class = "type_error_bad_input")
})

test_that("within() type tests and checks work as expected", {
  t <- t_any |> within(c(1L, 2L, 3L))

  expect_true(obj_is_type(c(1L, 2L), t))
  expect_true(obj_is_type(c(3L), t))
  expect_true(obj_is_type(integer(), t))

  expect_false(obj_is_type(c(1L, 4L), t))
  expect_false(obj_is_type(mean, t))

  expect_no_error(obj_assert_type(c(1L, 2L, 3L), t))
  expect_error(obj_assert_type(c(1L, 99L), t), class = "type_error_mistyped_obj")
})

test_that("within() description and diagnosis are as expected", {
  skip_on_covr()
  
  t <- t_any |> within(c("a", "b", "c"))

  expect_snapshot(obj_inspect_type(c("a", "b"), t))
  expect_snapshot(obj_inspect_type(c("a", "d", "e"), t))
  expect_snapshot(obj_inspect_type(mean, t))
})
