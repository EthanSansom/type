test_that("sized() errors on invalid inputs", {
  expect_error(
    t_int |> sized("A"),
    class = "type_error_bad_input"
  )
  expect_error(
    10 |> sized(1L),
    class = "type_error_bad_input"
  )
  expect_error(
    t_int |> sized(-1L),
    class = "type_error_bad_input"
  )
})

test_that("sized() type tests and checks work as expected", {
  t_size1 <- t_any |> sized(1L)
  t_size0 <- t_any |> sized(0L)

  expect_true(obj_is_type(1L, t_size1))
  expect_false(obj_is_type(1:2, t_size1))
  expect_false(obj_is_type(mean, t_size1))

  expect_true(obj_is_type(character(), t_size0))
  expect_false(obj_is_type(1:2, t_size0))

  expect_error(
    obj_assert_type(mean, t_size1),
    class = "type_error_mistyped_obj"
  )
  expect_no_error(
    obj_assert_type(TRUE, t_size1)
  )
})

test_that("sized() description and diagnosis are as expected", {
  t_size1 <- t_any |> sized(1L)
  expect_snapshot(obj_inspect_type(10, t_size1))
  expect_snapshot(obj_inspect_type(1:2, t_size1))
  expect_snapshot(obj_inspect_type(mean, t_size1))
})
