test_that("same_as() errors on invalid inputs", {
  expect_error(10L |> same_as(1L), class = "type_error_bad_input")
  expect_error(t_int |> same_as(list(1, 2)), class = "type_error_bad_input")
  expect_error(t_int |> same_as(integer()), class = "type_error_bad_input")
})

test_that("same_as() type tests and checks work as expected", {
  t <- t_any |> same_as(c(1L, 2L, 3L))

  expect_true(obj_is_type(c(1L, 2L, 3L), t))
  expect_false(obj_is_type(c(3L, 2L, 1L), t))
  expect_false(obj_is_type(c(1L, 2L), t)) 
  expect_false(obj_is_type(c(1L, 2L, 3L, 4L), t))
  expect_false(obj_is_type(c(1L, 2L, 4L), t))
  expect_false(obj_is_type(mean, t))

  expect_no_error(obj_assert_type(c(1L, 2L, 3L), t))
  expect_error(obj_assert_type(c(3L, 2L, 1L), t), class = "type_error_mistyped_obj")
})

test_that("same_as() description and diagnosis are as expected", {
  skip_on_covr()

  t <- t_any |> same_as(c("a", "b", "c"))

  expect_snapshot(obj_inspect_type(c("a", "b", "c"), t))
  expect_snapshot(obj_inspect_type(c("c", "b", "a"), t))
  expect_snapshot(obj_inspect_type(c("a", "b"), t))
  expect_snapshot(obj_inspect_type(c("a", "x", "c"), t))
  expect_snapshot(obj_inspect_type(mean, t))
})
