test_that("setequal_to() errors on invalid inputs", {
  expect_error(10L |> setequal_to(1L), class = "type_error_bad_input")
  expect_error(t_int |> setequal_to(list(1, 2)), class = "type_error_bad_input")
  expect_error(t_int |> setequal_to(integer()), class = "type_error_bad_input")
})

test_that("setequal_to() type tests and checks work as expected", {
  t <- t_any |> setequal_to(c(1L, 2L, 3L))

  expect_true(obj_is_type(c(1L, 2L, 3L), t))
  expect_true(obj_is_type(c(3L, 1L, 2L), t))
  expect_true(obj_is_type(c(1L, 1L, 2L, 3L), t))

  expect_false(obj_is_type(c(1L, 2L), t))
  expect_false(obj_is_type(c(1L, 2L, 3L, 4L), t))
  expect_false(obj_is_type(mean, t))

  expect_no_error(obj_assert_type(c(3L, 2L, 1L), t))
  expect_error(obj_assert_type(c(1L, 2L), t), class = "type_error_mistyped_obj")
  expect_error(obj_assert_type(c(1L, 2L, 3L, 4L), t), class = "type_error_mistyped_obj")
})

test_that("setequal_to() description and diagnosis are as expected", {
  skip_on_covr()
  
  t <- t_any |> setequal_to(c("a", "b", "c"))

  expect_snapshot(obj_inspect_type(c("c", "b", "a"), t))
  expect_snapshot(obj_inspect_type(c("a", "b"), t))
  expect_snapshot(obj_inspect_type(c("a", "b", "c", "d"), t))
  expect_snapshot(obj_inspect_type(c("a", "b", "d"), t))
  expect_snapshot(obj_inspect_type(mean, t))
})
