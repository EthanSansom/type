# list_type --------------------------------------------------------------------

test_that("list_type() errors on invalid inputs", {
  expect_error(list_type(), class = "type_error_bad_input")
  expect_error(list_type(1L, b = t_int), class = "type_error_bad_input")
  expect_error(list_type(a = t_int, a = t_chr), class = "type_error_bad_input")
  expect_error(list_type(a = 1L), class = "type_error_bad_input")
})

test_that("list_type() type tests and checks work as expected", {
  t <- list_type(x = t_int, y = t_chr)

  expect_true(obj_is_type(list(x = 1L, y = "a"), t))
  expect_false(obj_is_type(list(x = 1L, y = 2L), t))
  expect_false(obj_is_type(list(x = 1L), t))
  expect_false(obj_is_type(list(x = 1L, y = "a", z = 1L), t))
  expect_false(obj_is_type(list(y = "a", x = 1L), t))

  expect_no_error(obj_assert_type(list(x = 1L, y = "a"), t))
  expect_error(obj_assert_type(list(x = 1L, y = 2L), t), class = "type_error_mistyped_obj")
})

test_that("list_type() description and diagnosis are as expected", {
  skip_on_covr()

  t <- list_type(x = t_int, y = t_chr)
  expect_snapshot(obj_inspect_type(list(x = 1L, y = "a"), t))
  expect_snapshot(obj_inspect_type(list(x = 1L, y = 2L), t))
  expect_snapshot(obj_inspect_type(list(x = 1L), t))
})

# list_of_type -----------------------------------------------------------------

test_that("list_of_type() errors on invalid inputs", {
  expect_error(list_of_type(1L), class = "type_error_bad_input")
})

test_that("list_of_type() type tests and checks work as expected", {
  t <- list_of_type(t_int)

  expect_true(obj_is_type(list(1L, 2L, 3L), t))
  expect_true(obj_is_type(list(), t))

  expect_false(obj_is_type(list(1L, "a", 3L), t))
  expect_false(obj_is_type(1:3, t))

  expect_no_error(obj_assert_type(list(1L, 2L), t))
  expect_error(obj_assert_type(list(1L, "a"), t), class = "type_error_mistyped_obj")
})

test_that("list_of_type() description and diagnosis are as expected", {
  skip_on_covr()

  t <- list_of_type(t_int)
  expect_snapshot(obj_inspect_type(list(1L, 2L), t))
  expect_snapshot(obj_inspect_type(list(1L, "a", 3L), t))
})

# dataframe_type ---------------------------------------------------------------

test_that("dataframe_type() errors on invalid inputs", {
  expect_error(dataframe_type(), class = "type_error_bad_input")
  expect_error(dataframe_type(1L, b = t_int), class = "type_error_bad_input")
  expect_error(dataframe_type(a = t_int, a = t_chr), class = "type_error_bad_input")
  expect_error(dataframe_type(a = 1L), class = "type_error_bad_input")
})

test_that("dataframe_type() type tests and checks work as expected", {
  t <- dataframe_type(x = t_dbl, y = t_chr)

  expect_true(obj_is_type(data.frame(x = 1.5, y = "a"), t))
  expect_false(obj_is_type(data.frame(x = 1L, y = "a"), t))
  expect_false(obj_is_type(data.frame(x = 1.5), t))
  expect_false(obj_is_type(list(x = 1.5, y = "a"), t))

  expect_no_error(obj_assert_type(data.frame(x = 1.5, y = "a"), t))
  expect_error(
    obj_assert_type(data.frame(x = 1L, y = "a"), t),
    class = "type_error_mistyped_obj"
  )
})

test_that("dataframe_type() description and diagnosis are as expected", {
  skip_on_covr()

  t <- dataframe_type(x = t_dbl, y = t_chr)
  expect_snapshot(obj_inspect_type(data.frame(x = 1.5, y = "a"), t))
  expect_snapshot(obj_inspect_type(data.frame(x = 1L, y = "a"), t))
  expect_snapshot(obj_inspect_type(list(x = 1.5, y = "a"), t))
})
