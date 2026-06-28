test_that("type_union() errors on invalid inputs", {
  expect_error(type_union(), class = "type_error_bad_input")
  expect_error(type_union(1L), class = "type_error_bad_input")
  expect_error(type_union(t_int, "not_a_type"), class = "type_error_bad_input")
})

test_that("type_union() type tests work as expected", {
  t <- type_union(t_int, t_chr)

  expect_true(obj_is_type(1L, t))
  expect_true(obj_is_type("a", t))
  expect_true(obj_is_type(integer(), t))
  expect_true(obj_is_type(character(), t))

  expect_false(obj_is_type(1.5, t))
  expect_false(obj_is_type(TRUE, t))
  expect_false(obj_is_type(NULL, t))

  expect_no_error(obj_assert_type(1L, t))
  expect_no_error(obj_assert_type("a", t))
  expect_error(obj_assert_type(1.5, t), class = "type_error_mistyped_obj")
})

test_that("type_union() with three or more types works as expected", {
  t <- type_union(t_int, t_chr, t_lgl)

  expect_true(obj_is_type(1L, t))
  expect_true(obj_is_type("a", t))
  expect_true(obj_is_type(TRUE, t))

  expect_false(obj_is_type(1.5, t))
})

test_that("type_union() flattens nested unions", {
  t1 <- type_union(t_int, t_chr)
  t2 <- type_union(t1, t_lgl)

  expect_true(obj_is_type(1L, t2))
  expect_true(obj_is_type("a", t2))
  expect_true(obj_is_type(TRUE, t2))
  expect_false(obj_is_type(1.5, t2))

  # The flattened union matches a directly constructed union
  t3 <- type_union(t_int, t_chr, t_lgl)
  expect_true(identical(t2@types, t3@types))
})

test_that("type_union() deduplicates types", {
  t <- type_union(t_int, t_int)
  expect_equal(length(t@types), 1L)
})

test_that("type_union() works within typed()", {
  f <- typed(function(x = type_union(t_int, t_chr)) { x })

  expect_no_error(f(1L))
  expect_no_error(f("a"))
  expect_error(f(1.5), class = "type_error_mistyped_arg")
  expect_error(f(TRUE), class = "type_error_mistyped_arg")
})

test_that("type_union() works within %:%", {
  type_union(t_int, t_chr) %:% val("hello")

  expect_no_error(val <- 1L)
  expect_no_error(val <- "world")
  expect_error(val <- 1.5, class = "type_error_mistyped_obj")
})

test_that("type_union() works within has()", {
  t <- t_list |> has(on_elm(1L), type_union(t_int, t_chr))

  expect_true(obj_is_type(list(1L), t))
  expect_true(obj_is_type(list("a"), t))
  expect_false(obj_is_type(list(1.5), t))
})

test_that("type_union() description and diagnosis are as expected", {
  skip_on_covr()
  
  t <- type_union(t_int, t_chr |> sized(1L))

  expect_snapshot(obj_inspect_type(1L, t))
  expect_snapshot(obj_inspect_type("a", t))
  expect_snapshot(obj_inspect_type(1.5, t))
})
