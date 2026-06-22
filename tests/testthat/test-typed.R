# construction -----------------------------------------------------------------

test_that("typed() returns a <type_typed_function/function>", {
  f <- typed(function(x) { x })
  expect_s3_class(f, "type_typed_function")
  expect_s3_class(f, "function")
})

test_that("typed() requires exactly one function definition", {
  expect_error(
    typed(function(x) { x }, function(y) { y }),
    class = "type_error_bad_input"
  )
  expect_error(
    typed(t_int),
    class = "type_error_bad_input"
  )
})

test_that("typed() `...` rejects non-relation, non-function arguments", {
  expect_error(
    typed(42L, function(x) x),
    class = "type_error_bad_input"
  )
  expect_error(
    typed("hello", function(x) x),
    class = "type_error_bad_input"
  )
})
 
test_that("typed() rejects relations that reference args not in the function", {
  expect_error(
    typed(same_sized(x, z), function(x = t_int, y = t_int) { x }),
    class = "type_error_bad_input"
  )
})

test_that("typed() `returns` must be a type", {
  expect_error(
    typed(function(x = t_int) { x }, returns = 10),
    class = "type_error_bad_input"
  )
})

test_that("typed() accepts the function definition in any position in `...`", {
  f1 <- typed(function(x = t_int, y = t_int) { x }, same_sized(x, y))
  f2 <- typed(same_sized(x, y), function(x = t_int, y = t_int) { x })

  expect_no_error(f1(1L, 1L))
  expect_no_error(f2(1L, 1L))
  expect_error(f1(1L, 1:2), class = "type_error_mistyped_arg")
  expect_error(f2(1L, 1:2), class = "type_error_mistyped_arg")
})

test_that("typed() default argument values work as expected", {
  f1 <- typed(function(x = t_int %:% 10L) { x })
  f2 <- typed(function(x = t_int %:% "A") { x })

  # Uses the correct defaults
  expect_identical(f1(), 10L)
  expect_error(f2(), class = "type_error_mistyped_arg")

  # Defaults aren't evaluated
  expect_no_error(typed(function(x = t_int %:% stop("AH")) { x }))

  # Arguments without a default are evaluated and errors are forwarded
  expect_error(
    typed(function(x = stop("AH")) { x }),
    class = "type_error_bad_input"
  )

  # `...` can't have a default value
  expect_error(
    typed(function(... = t_int %:% 10L) { list(...) }), 
    class = "type_error_bad_input"
  )

  # Default value LHS must be a type 
  expect_error(
    typed(function(x = 10L %:% 10L) { x }),
    class = "type_error_bad_input"
  )
})

# argument type ----------------------------------------------------------------

test_that("Mistyped arguments raise a <type_error_mistyped_arg> error", {
  f1 <- typed(function(x = t_int) { x })
  f2 <- typed(function(... = t_int) { list(...) })

  expect_error(f1(1.0), class = "type_error_mistyped_arg")
  expect_error(f1(TRUE), class = "type_error_mistyped_arg")
  expect_error(f2(1.0), class = "type_error_mistyped_arg")
  expect_error(f2(1L, TRUE), class = "type_error_mistyped_arg")
})

test_that("Untyped arguments accept any value", {
  f1 <- typed(function(x) { x })
  f2 <- typed(function(...) { list(...) })

  expect_identical(f1(1L), 1L)
  expect_identical(f1(NULL), NULL)
  expect_identical(f2(1L, mean), list(1L, mean))
  expect_identical(f2(), list())
})

test_that("Typed arguments with a default value are checked as expected", {
  f <- typed(function(x = t_int %:% 99L) x)
  expect_no_error(f())
  expect_no_error(f(1L))
  expect_error(f("A"), class = "type_error_mistyped_arg")
})

test_that("Multiple typed arguments are checked independently", {
  f <- typed(function(x = t_int, y = t_lgl) paste(x, y))
  expect_equal(f(1L, TRUE), "1 TRUE")
  expect_error(f("A", TRUE), class = "type_error_mistyped_arg")
  expect_error(f(1L, "A"),  class = "type_error_mistyped_arg")
})

# dots type --------------------------------------------------------------------

test_that("Untyped ... accepts anything", {
  f <- typed(function(...) list(...))
  expect_equal(f(1L, "a", TRUE), list(1L, "a", TRUE))
})

test_that("Typed ... checks each element individually", {
  f <- typed(function(... = t_int) list(...))
  expect_equal(f(1L, 2L, 3L), list(1L, 2L, 3L))
  expect_error(f(1L, 2.0, 3L), class = "type_error_mistyped_arg")
})

test_that("Typed `... = t_dots` checks dots as a list", {
  f <- typed(function(... = t_dots |> sized(2L)) list(...))
  expect_equal(f(1L, 2L), list(1L, 2L))
  expect_error(f(1L), class = "type_error_mistyped_arg")
})

# return type ------------------------------------------------------------------

test_that("Mistyped return value raises a <type_error_mistyped_arg> error", {
  f <- typed(function(x = t_int) { x }, returns = t_lgl)
  expect_error(f(10L), class = "type_error_mistyped_arg")
})
 
test_that("Explicit `return()` calls are type checked", {
  f <- typed(
    function(x = t_lgl) {
      if (x) return("yes")
      return("no")
    },
    returns = t_chr
  )
  expect_equal(f(TRUE), "yes")
  expect_equal(f(FALSE), "no")
 
  g <- typed(
    function(x = t_lgl) {
      if (x) return(1L)
      return("no")
    },
    returns = t_chr
  )
  expect_error(g(TRUE), class = "type_error_mistyped_arg")
  expect_equal(g(FALSE), "no")
})

# modifications ----------------------------------------------------------------

test_that("optional() arguments can be unsupplied without error", {
  f1 <- typed(function(x = optional(t_int)) { if (missing(x)) "missing" else x })
  expect_equal(f1(), "missing")
  expect_equal(f1(1L), 1L)

  f2 <- typed(function(x = optional(t_lgl)) { TRUE })
  expect_equal(f2(), TRUE)

  f3 <- typed(function(x = optional(t_lgl) %:% rlang::missing_arg()) { x })
  expect_identical(f3(), rlang::missing_arg())
})

test_that("optional() arguments are type checked as expected", {
  f <- typed(function(x = optional(t_int)) { if (missing(x)) "missing" else x })
  expect_error(f("A"), class = "type_error_mistyped_arg")
})

test_that("maybe() arguments can be `NULL` without error", {
  f <- typed(function(x = maybe(t_int)) { x })
  expect_identical(f(NULL), NULL)
  expect_equal(f(1L), 1L)
})

test_that("maybe() arguments are type checked as expected", {
  f <- typed(function(x = maybe(t_int)) { x })
  expect_error(f("A"), class = "type_error_mistyped_arg")
})

test_that("optional() and maybe() cannot be applied to ...", {
  expect_error(
    typed(function(... = optional(t_int)) list(...)),
    class = "type_error_bad_input"
  )
  expect_error(
    typed(function(... = maybe(t_int)) list(...)),
    class = "type_error_bad_input"
  )
})

test_that("optional() and maybe() can be combined as expected", {
  f <- typed(function(x = optional(maybe(t_int))) { if (missing(x)) "missing" else x })
  expect_equal(f(), "missing")
  expect_null(f(NULL))
  expect_equal(f(1L),  1L)
  expect_error(f("A"), class = "type_error_mistyped_arg")
})

# printing ---------------------------------------------------------------------

test_that("typed() prints as expected", {
  # The {testthat} environment changes between runs, which causes the
  # print method to print a different function environment each time
  with_empty_env <- function(x) { environment(x) <- emptyenv(); x }

  expect_snapshot(with_empty_env(typed(function() {})))
  expect_snapshot(with_empty_env(typed(function() {}, returns = t_lgl)))
  expect_snapshot(with_empty_env(typed(function(x) { x })))
  expect_snapshot(with_empty_env(typed(function(x = t_int) { x })))
  expect_snapshot(with_empty_env(typed(function(x = t_bool, y = t_lgl) { x })))
  expect_snapshot(with_empty_env(typed(same_sized(x, y), function(x = t_bool, y = t_lgl) { x })))
  expect_snapshot(with_empty_env(typed(
    same_sized(x, y), 
    function(x = t_bool, y = t_lgl) { x },
    returns = t_chr
  )))
})
