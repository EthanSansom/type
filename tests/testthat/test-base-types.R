test_that("base types match expected objects", {
  # t_any
  expect_true(obj_is_type(1L, t_any))
  expect_true(obj_is_type("a", t_any))
  expect_true(obj_is_type(NULL, t_any))
  expect_true(obj_is_type(mean, t_any))

  # t_null
  expect_true(obj_is_type(NULL, t_null))
  expect_false(obj_is_type(NA, t_null))
  expect_false(obj_is_type(list(), t_null))

  # t_list
  expect_true(obj_is_type(list(), t_list))
  expect_true(obj_is_type(list(1, "a"), t_list))
  expect_false(obj_is_type(1:3, t_list))
  expect_false(obj_is_type(NULL, t_list))

  # t_env
  expect_true(obj_is_type(new.env(), t_env))
  expect_true(obj_is_type(globalenv(), t_env))
  expect_false(obj_is_type(list(), t_env))

  # t_fun
  expect_true(obj_is_type(mean, t_fun))
  expect_true(obj_is_type(\(x) x, t_fun))
  expect_true(obj_is_type(sum, t_fun))
  expect_false(obj_is_type(1L, t_fun))

  # t_vec
  expect_true(obj_is_type(1:3, t_vec))
  expect_true(obj_is_type(c("a", "b"), t_vec))
  expect_true(obj_is_type(data.frame(), t_vec))
  expect_false(obj_is_type(mean, t_vec))
  expect_false(obj_is_type(new.env(), t_vec))

  # t_num
  expect_true(obj_is_type(1L, t_num))
  expect_true(obj_is_type(1.5, t_num))
  expect_false(obj_is_type("1", t_num))
  expect_false(obj_is_type(TRUE, t_num))

  # t_lgl
  expect_true(obj_is_type(TRUE, t_lgl))
  expect_true(obj_is_type(c(TRUE, FALSE, NA), t_lgl))
  expect_false(obj_is_type(1L, t_lgl))
  expect_false(obj_is_type(factor(TRUE), t_lgl))

  # t_bool
  expect_true(obj_is_type(TRUE, t_bool))
  expect_true(obj_is_type(FALSE, t_bool))
  expect_false(obj_is_type(NA, t_bool))
  expect_false(obj_is_type(c(TRUE, FALSE), t_bool))
  expect_false(obj_is_type(1L, t_bool))

  # t_int
  expect_true(obj_is_type(1L, t_int))
  expect_true(obj_is_type(integer(), t_int))
  expect_false(obj_is_type(1.0, t_int))
  expect_false(obj_is_type(TRUE, t_int))

  # t_dbl
  expect_true(obj_is_type(1.5, t_dbl))
  expect_true(obj_is_type(double(), t_dbl))
  expect_false(obj_is_type(1L, t_dbl))
  expect_false(obj_is_type("1", t_dbl))

  # t_chr
  expect_true(obj_is_type("a", t_chr))
  expect_true(obj_is_type(character(), t_chr))
  expect_false(obj_is_type(1L, t_chr))
  expect_false(obj_is_type(factor("a"), t_chr))

  # t_string
  expect_true(obj_is_type("hello", t_string))
  expect_false(obj_is_type(NA_character_, t_string))
  expect_false(obj_is_type(c("a", "b"), t_string))
  expect_false(obj_is_type(1L, t_string))

  # t_dataframe
  expect_true(obj_is_type(data.frame(), t_dataframe))
  expect_true(obj_is_type(data.frame(x = 1:3), t_dataframe))
  expect_false(obj_is_type(list(x = 1:3), t_dataframe))
  expect_false(obj_is_type(1:3, t_dataframe))

  # t_factor
  expect_true(obj_is_type(factor("a"), t_factor))
  expect_true(obj_is_type(factor(c("a", "b")), t_factor))
  expect_false(obj_is_type("a", t_factor))
  expect_false(obj_is_type(1L, t_factor))

  # t_date
  expect_true(obj_is_type(Sys.Date(), t_date))
  expect_true(obj_is_type(as.Date("2020-01-01"), t_date))
  expect_false(obj_is_type("2020-01-01", t_date))
  expect_false(obj_is_type(1L, t_date))

  # t_posixct
  expect_true(obj_is_type(Sys.time(), t_posixct))
  expect_true(obj_is_type(as.POSIXct("2020-01-01"), t_posixct))
  expect_false(obj_is_type(as.Date("2020-01-01"), t_posixct))
  expect_false(obj_is_type("2020-01-01", t_posixct))
})
