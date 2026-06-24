test_that("last_type() works as expected", {
  # `NULL` when no assertions have been run
  expect_identical(last_type(), NULL)

  # Records last failed type assertsion
  try(obj_assert_type(10L, t_bool), silent = TRUE)
  expect_identical(last_type(), t_bool)

  # Doesn't record non-failed type assertions
  obj_assert_type(10L, t_int)
  expect_identical(last_type(), t_bool)

  # Records failed `typed()` assertions
  f <- typed(function(x = t_chr) { x })
  try(f(10L), silent = TRUE)
  expect_identical(last_type(), t_chr)
})
