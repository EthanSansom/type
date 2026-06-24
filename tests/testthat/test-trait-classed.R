test_that("classed() errors on invalid inputs", {
  expect_error(10L |> classed("foo"), class = "type_error_bad_input")
  expect_error(t_any |> classed(1L), class = "type_error_bad_input")
  expect_error(t_any |> classed(c("foo", NA_character_)), class = "type_error_bad_input")
  expect_error(t_any |> classed("foo", inherits = "some"), class = "type_error_bad_input")
})

test_that("classed() with inherits = 'any' works as expected", {
  t <- t_any |> classed(c("Date", "POSIXct"), inherits = "any")

  expect_true(obj_is_type(Sys.Date(), t))
  expect_true(obj_is_type(Sys.time(), t))
  expect_false(obj_is_type(1L, t))
  expect_false(obj_is_type("2024-01-01", t))

  expect_no_error(obj_assert_type(Sys.Date(), t))
  expect_error(obj_assert_type(1L, t), class = "type_error_mistyped_obj")
})

test_that("classed() with inherits = 'all' works as expected", {
  t <- t_any |> classed(c("POSIXct", "POSIXt"), inherits = "all")

  expect_true(obj_is_type(Sys.time(), t))
  expect_false(obj_is_type(Sys.Date(), t))
  expect_false(obj_is_type(1L, t))

  expect_no_error(obj_assert_type(Sys.time(), t))
  expect_error(obj_assert_type(Sys.Date(), t), class = "type_error_mistyped_obj")
})

test_that("classed() description and diagnosis are as expected", {
  skip_on_covr()

  t_any_cls <- t_any |> classed(c("Date", "POSIXct"), inherits = "any")
  t_all_cls <- t_any |> classed(c("POSIXct", "POSIXt"), inherits = "all")
  t_one_cls <- t_any |> classed("Date")

  expect_snapshot(obj_inspect_type(Sys.Date(), t_any_cls))
  expect_snapshot(obj_inspect_type(1L, t_any_cls))
  expect_snapshot(obj_inspect_type(Sys.time(), t_all_cls))
  expect_snapshot(obj_inspect_type(1L, t_all_cls))
  expect_snapshot(obj_inspect_type(Sys.Date(), t_one_cls))
  expect_snapshot(obj_inspect_type(1L, t_one_cls))
})
