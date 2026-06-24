test_that("disjoint_to() errors on invalid inputs", {
  expect_error(10L |> disjoint_to(1L), class = "type_error_bad_input")
  expect_error(t_int |> disjoint_to(list(1, 2)), class = "type_error_bad_input")
  expect_error(t_int |> disjoint_to(integer()), class = "type_error_bad_input")
})

test_that("disjoint_to() type tests and checks work as expected", {
  t <- t_any |> disjoint_to(c(1L, 2L, 3L))

  expect_true(obj_is_type(c(4L, 5L, 6L), t))
  expect_true(obj_is_type(integer(), t))

  expect_false(obj_is_type(c(1L), t))
  expect_false(obj_is_type(c(4L, 5L, 1L), t))
  expect_false(obj_is_type(mean, t))

  expect_no_error(obj_assert_type(100L, t))
  expect_error(obj_assert_type(c(1L, 4L), t), class = "type_error_mistyped_obj")
})

test_that("disjoint_to() description and diagnosis are as expected", {
  t <- t_any |> disjoint_to(c("a", "b", "c"))

  expect_snapshot(obj_inspect_type(c("d", "e"), t))
  expect_snapshot(obj_inspect_type(c("a"), t))
  expect_snapshot(obj_inspect_type(c("b", "c", "d"), t))  # multiple overlaps
  expect_snapshot(obj_inspect_type(mean, t))
})