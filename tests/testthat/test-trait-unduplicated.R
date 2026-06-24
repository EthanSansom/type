test_that("unduplicated() errors on invalid inputs", {
  expect_error(10L |> unduplicated(), class = "type_error_bad_input")
})

test_that("unduplicated() type tests and checks work as expected", {
  t <- t_any |> unduplicated()

  expect_true(obj_is_type(1:3, t))
  expect_true(obj_is_type(c("a", "b", "c"), t))
  expect_true(obj_is_type(integer(), t))

  expect_false(obj_is_type(c(1L, 1L, 2L), t))
  expect_false(obj_is_type(c("a", "a"), t))
  expect_false(obj_is_type(mean, t))

  expect_no_error(obj_assert_type(c(1L, 2L, 3L), t))
  expect_error(obj_assert_type(c(1L, 1L, 2L), t), class = "type_error_mistyped_obj")
})

test_that("unduplicated() description and diagnosis are as expected", {
  t <- t_any |> unduplicated()

  expect_snapshot(obj_inspect_type(c(1L, 2L, 3L), t))
  expect_snapshot(obj_inspect_type(c(1L, 1L, 2L, 3L, 3L), t))
  expect_snapshot(obj_inspect_type(mean, t))
})
