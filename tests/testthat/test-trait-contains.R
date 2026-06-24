test_that("contains() errors on invalid inputs", {
  expect_error(10L |> contains(1L), class = "type_error_bad_input")
  expect_error(t_int |> contains(list(1, 2)), class = "type_error_bad_input")
  expect_error(t_int |> contains(integer()), class = "type_error_bad_input")
})

test_that("contains() type tests and checks work as expected", {
  t <- t_any |> contains(c(1L, 2L, 3L))

  expect_true(obj_is_type(c(1L, 2L, 3L), t))
  expect_true(obj_is_type(c(1L, 2L, 3L, 4L), t))

  expect_false(obj_is_type(c(1L, 2L), t))
  expect_false(obj_is_type(integer(), t))
  expect_false(obj_is_type(mean, t))

  expect_no_error(obj_assert_type(1:5, t))
  expect_error(obj_assert_type(c(1L, 2L), t), class = "type_error_mistyped_obj")
})

test_that("contains() description and diagnosis are as expected", {
  t <- t_any |> contains(c("a", "b", "c"))

  expect_snapshot(obj_inspect_type(c("a", "b", "c", "d"), t))
  expect_snapshot(obj_inspect_type(c("a", "c"), t))
  expect_snapshot(obj_inspect_type(character(), t))
  expect_snapshot(obj_inspect_type(mean, t))
})
