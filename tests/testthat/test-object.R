# invalid declarations ---------------------------------------------------------

test_that("%:% errors on invlaid LHS inputs", {
  expect_error(42L %:% x(1L), class = "type_error_bad_input")
  expect_error("foo" %:% x(1L), class = "type_error_bad_input")
})

test_that("%:% errors on invalid RHS inputs", {
  value <- 1L

  # RHS not a simple call
  expect_error(t_int %:% value, class = "type_error_bad_input")
  expect_error(t_int %:% base::c(1L), class = "type_error_bad_input")
  expect_error(t_int %:% (function() 1L)(), class = "type_error_bad_input")

  # RHS contains no initial value or multiple initial values
  expect_error(t_int %:% x(), class = "type_error_bad_input")
  expect_error(t_int %:% x(1L, 2L), class = "type_error_bad_input")

  # RHS initial value can't be evaluated
  expect_error(t_int %:% x(stop("AH")), class = "type_error_bad_input")
})

# %:% --------------------------------------------------------------------------

test_that("%:% works as expected with an object of the correct type", {
  t_int %:% x(1L)
  expect_identical(x, 1L)
  expect_no_error(x <- 10L)
  expect_identical(x, 10L)

  t_int |> sized(2L) %:% y(1:2)
  expect_identical(y, 1:2)
  expect_no_error(y <- 3:4)
  expect_error(y <- 1:3)
  expect_error(y <- "A")

  # Initial value is returned invisibly, as in `<-`
  expect_invisible(t_int %:% x(1L))
  expect_identical(t_int %:% x(1L), 1L)
})

test_that("%:% assignment with a mistyped value errors", {
  t_int %:% x(1L)
  expect_error(x <- "A")
  expect_error(x <- TRUE)
  expect_error(x <- 1.0)
})

test_that("%:% initialization with a mistyped value errors", {
  expect_error(t_int %:% x("A"), class = "type_error_mistyped_obj")
  expect_error(t_int %:% x(TRUE), class = "type_error_mistyped_obj")
})

test_that("%:% respects its environment", {
  t_chr %:% x("A")
  f <- function() {
    t_int %:% x(10L)
  }
  f()
  expect_error(x <- 10L)
  expect_no_error(x <- "B")
})

# const ------------------------------------------------------------------------

test_that("const() errors outside %:% context", {
  expect_error(const(t_int), class = "type_error_bad_input")
})

test_that("const() creates a readable binding", {
  const(t_int) %:% x(1L)
  expect_identical(x, 1L)
})

test_that("const() creates a read-only binding", {
  const(t_int) %:% x(1L)
  expect_error(x <- 2L, class = "type_error_mistyped_obj")
  expect_error(x <- 1L, class = "type_error_mistyped_obj")
})

test_that("const() initialization with a mistyped value errors", {
  expect_error(const(t_int) %:% x("A"), class = "type_error_mistyped_obj")
})

test_that("optional() and maybe() error inside %:% context", {
  expect_error(optional(t_int) %:% x(1L), class = "type_error_bad_input")
  expect_error(maybe(t_int) %:% x(1L), class = "type_error_bad_input")
})
