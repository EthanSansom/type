# bounded() --------------------------------------------------------------------

test_that("bounded() errors on invalid inputs", {
  expect_error(10L |> bounded(0L, 10L), class = "type_error_bad_input")
  expect_error(t_int |> bounded(1:3, 10L), class = "type_error_bad_input")
  expect_error(t_int |> bounded(0L, 1:3), class = "type_error_bad_input")
  expect_error(t_int |> bounded(list(), 10L), class = "type_error_bad_input")
  expect_error(t_int |> bounded(0L, list()), class = "type_error_bad_input")
  expect_error(t_int |> bounded(0L, 10L, bounds = "{}"), class = "type_error_bad_input")
  expect_error(t_int |> bounded(0L, 10L, bounds = "["), class = "type_error_bad_input")
})

test_that("bounded() with closed bounds [] works as expected", {
  t <- t_int |> bounded(0L, 10L)

  expect_true(obj_is_type(0L, t))
  expect_true(obj_is_type(10L, t))
  expect_true(obj_is_type(c(0L, 5L, 10L), t))

  expect_false(obj_is_type(-1L, t))
  expect_false(obj_is_type(11L, t))

  expect_no_error(obj_assert_type(5L, t))
  expect_error(obj_assert_type(-1L, t), class = "type_error_mistyped_obj")
  expect_error(obj_assert_type(11L, t), class = "type_error_mistyped_obj")
})

test_that("bounded() with half-open [), (] and closed () bounds works as expected", {
  t_lo <- t_int |> bounded(0L, 10L, bounds = "[)")
  expect_true(obj_is_type(0L, t_lo))
  expect_false(obj_is_type(10L, t_lo))
  expect_false(obj_is_type(-1L, t_lo))

  t_ro <- t_int |> bounded(0L, 10L, bounds = "(]")
  expect_false(obj_is_type(0L, t_ro))
  expect_true(obj_is_type(10L, t_ro))
  expect_false(obj_is_type(11L, t_ro))

  t_cl <- t_int |> bounded(0L, 10L, bounds = "()")
  expect_false(obj_is_type(0L, t_cl))
  expect_false(obj_is_type(10L, t_cl))
  expect_true(obj_is_type(c(1L, 5L, 9L), t_cl))
})

test_that("bounded() with a single bound works as expected", {
  t_left  <- t_int |> bounded(0L)
  expect_true(obj_is_type(0L, t_left))
  expect_true(obj_is_type(10:20, t_left))
  expect_false(obj_is_type(-1L, t_left))

  t_right <- t_int |> bounded(right = 0L)
  expect_true(obj_is_type(0L, t_right))
  expect_true(obj_is_type(-1000L, t_right))
  expect_false(obj_is_type(1L, t_right))
})

test_that("bounded() ignores NA values", {
  t <- t_int |> bounded(0L, 10L)
  expect_true(obj_is_type(c(1L, NA_integer_, 5L), t))
})

test_that("bounded() returns FALSE when comparison raises an error", {
  t <- t_any |> bounded("a", "c")
  expect_false(obj_is_type(as.Date(0), t))
})

test_that("bounded() description and diagnosis are as expected", {
  skip_on_covr()

  t_closed <- t_any |> bounded(0L, 10L)
  t_half_open <- t_any |> bounded(0L, 10L, bounds = "[)")
  t_open <- t_any |> bounded(0L, 10L, bounds = "()")
  t_left <- t_any |> bounded(0L)
  t_right <- t_any |> bounded(right = 10L)

  expect_snapshot(obj_inspect_type(5L, t_closed))
  expect_snapshot(obj_inspect_type(-1L, t_closed))
  expect_snapshot(obj_inspect_type(11L, t_closed))
  expect_snapshot(obj_inspect_type(10L, t_half_open))
  expect_snapshot(obj_inspect_type(0L, t_open))
  expect_snapshot(obj_inspect_type(-c(10:15), t_left))
  expect_snapshot(obj_inspect_type(11:100, t_right))
})
