test_that("bare_typed() errors on invalid inputs", {
  expect_error(
    t_any |> bare_typed("A"),
    class = "type_error_bad_input"
  )
  expect_error(
    t_any |> bare_typed(10L),
    class = "type_error_bad_input"
  )
  expect_error(
    10 |> bare_typed(10L),
    class = "type_error_bad_input"
  )
})

test_that("bare_typed() type tests and checks work as expected", {
  t_bare_list <- t_any |> bare_typed("list")

  expect_true(obj_is_type(list(), t_bare_list))
  expect_false(obj_is_type(data.frame(), t_bare_list))

  expect_no_error(
    obj_assert_type(list(1, 2), t_bare_list)
  )
  expect_error(
    obj_assert_type(mean, t_bare_list),
    class = "type_error_mistyped_obj"
  )
})

test_that("bare_typed() description and diagnosis are as expected", {
  t_bare_list <- t_any |> bare_typed("list")
  expect_snapshot(obj_inspect_type(list(), t_bare_list))
  expect_snapshot(obj_inspect_type(mean, t_bare_list))
})
