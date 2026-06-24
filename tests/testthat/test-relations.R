# errors -----------------------------------------------------------------------

test_that("Relations error when used in invalid contexts", {
  expect_error(same_sized(on_each()), class = "type_error_bad_input")
  expect_error(t_any |> has(same_sized(on_each())), class = "type_error_bad_input")
  expect_error(same_classed(on_each()), class = "type_error_bad_input")
  expect_error(t_any |> has(same_classed(on_each())), class = "type_error_bad_input")
})

test_that("Relations within `has_relation()` error on invalid inputs", {
  expect_error(t_any |> has_relation(same_sized(10L)), class = "type_error_bad_input")
  expect_error(t_any |> has_relation(same_sized()), class = "type_error_bad_input")
  expect_error(t_any |> has_relation(same_classed("A")), class = "type_error_bad_input")
  expect_error(t_any |> has_relation(same_classed()), class = "type_error_bad_input")
})

test_that("Relations within `typed()` error on invalid inputs", {
  expect_error(
    typed(same_sized(), function(x) { x }), 
    class = "type_error_bad_input"
  )
  expect_error(
    typed(same_sized(y, z), function(x, y) { x }), 
    class = "type_error_bad_input"
  )
  expect_error(
    typed(same_sized(on_each()), function(x, y) { x }), 
    class = "type_error_bad_input"
  )
  expect_error(
    typed(same_classed(), function(x) { x }), 
    class = "type_error_bad_input"
  )
  expect_error(
    typed(same_classed(y, z), function(x, y) { x }), 
    class = "type_error_bad_input"
  )
  expect_error(
    typed(same_classed(on_each()), function(x, y) { x }), 
    class = "type_error_bad_input"
  )
})

# selectors --------------------------------------------------------------------

test_that("Multiple `has()` and `has_relation()` selections work as expected", {
  t_pair <- t_any |>
    has(on(names(.x)), t_chr) |>
    has(on_each(), t_int) |>
    has_relation(same_sized(on_elms(c(1L, 2L)))) |>
    has_relation(same_classed(on_attrs(c("a_attr", "b_attr"))))

  good <- structure(list(a = 1:3, b = 4:6), a_attr = "foo", b_attr = "bar")

  expect_true(obj_is_type(good, t_pair))
  expect_no_error(obj_assert_type(good, t_pair))

  # Incorrect names
  expect_false(obj_is_type(
    structure(list(1:3, 4:6), a_attr = "foo", b_attr = "bar"),
    t_pair
  ))

  # Incorrect element type
  expect_false(obj_is_type(
    structure(list(a = 1:3, b = c("x", "y", "z")), a_attr = "foo", b_attr = "bar"),
    t_pair
  ))

  # Incorrect element sizes
  expect_false(obj_is_type(
    structure(list(a = 1:3, b = 4:5), a_attr = "foo", b_attr = "bar"),
    t_pair
  ))

  # Incorrect attribute sizes
  expect_false(obj_is_type(
    structure(list(a = 1:3, b = 4:6), a_attr = "foo", b_attr = 42L),
    t_pair
  ))
})

test_that("Combinations of selectors work as expected", {
  t <- t_any |>
    has_relation(same_sized(
      on_each(),
      on_attrs(c("a_attr", "b_attr")),
      on(unique)
    ))

  good <- structure(
    list(1:2, 3:4), 
    a_attr = c("foo", "fah"), 
    b_attr = c("bar", "baz")
  )

  expect_true(obj_is_type(good, t))
  expect_no_error(obj_assert_type(good, t))

  # Size of `unique(.x)` is different
  expect_false(obj_is_type(
    structure(list(1:2, 1:2), a_attr = c("foo", "fah"), b_attr = c("bar", "baz")),
    t
  ))

  # Size of `.x[[2]]` is  different
  expect_false(obj_is_type(
    structure(list(1:2, 1:3), a_attr = c("foo", "fah"), b_attr = c("bar", "baz")),
    t
  ))

  # Size of `attr(.x, "b_attr")` is  different
  expect_false(obj_is_type(
    structure(list(1:2, 3:4), a_attr = c("foo", "fah"), b_attr = c("bar")),
    t
  ))
})

# same_sized -------------------------------------------------------------------

test_that("same_sized() works as expected within object type checks", {
  t <- t_any |> has_relation(same_sized(on_elm(1L), on_elm(2L)))

  expect_true(obj_is_type(list(1:3, 1:3), t))
  expect_true(obj_is_type(list(integer(), integer()), t))

  expect_false(obj_is_type(list(1:3, 1:2), t))
  expect_false(obj_is_type(list(mean, 1:3), t))

  expect_no_error(obj_assert_type(list(1:3, 1:3), t))
  expect_error(obj_assert_type(list(1:3, 1:2), t), class = "type_error_mistyped_obj")
  expect_error(obj_assert_type(list(mean, 1:3), t), class = "type_error_mistyped_obj")
})

test_that("same_sized() works as expected within argument type checks", {
  f <- typed(same_sized(x, y), function(x = t_int, y = t_int) x)

  expect_no_error(f(1:3, 1:3))
  expect_no_error(f(integer(), integer()))

  expect_error(f(1:3, 1:2), class = "type_error_mistyped_arg")
  expect_error(f(1:3, 1:4), class = "type_error_mistyped_arg")
})

# same_classed -----------------------------------------------------------------

test_that("same_classed() works as expected within object type checks", {
  t <- t_any |> has_relation(same_classed(on_elm(1L), on_elm(2L)))

  expect_true(obj_is_type(list(1:3, 1:3), t))
  expect_true(obj_is_type(list(factor("a"), factor("b")), t))

  expect_false(obj_is_type(list(1:3, 1.5), t))
  expect_false(obj_is_type(list(mean, 1:3), t))

  expect_no_error(obj_assert_type(list(1:3, 1:3), t))
  expect_error(obj_assert_type(list(1:3, 1.5), t), class = "type_error_mistyped_obj")
  expect_error(obj_assert_type(list(mean, 1:3), t), class = "type_error_mistyped_obj")
})

test_that("same_classed() works as expected within argument type checks", {
  f <- typed(same_classed(x, y), function(x = t_any, y = t_any) x)

  expect_no_error(f(1:3, 1:3))
  expect_no_error(f(factor("a"), factor("b")))

  expect_error(f(1:3, 1.5), class = "type_error_mistyped_arg")
  expect_error(f(factor("a"), 1:3), class = "type_error_mistyped_arg")
})

# exclusive --------------------------------------------------------------------

test_that("exclusive() works as expected within object type checks", {
  t <- t_any |> has_relation(exclusive(on_elm("x"), on_elm("y")))

  expect_true(obj_is_type(list(x = 1L, y = NULL), t))
  expect_true(obj_is_type(list(x = NULL, y = 1L), t))

  expect_false(obj_is_type(list(x = NULL, y = NULL), t))
  expect_false(obj_is_type(list(x = 1L, y = 1L), t))

  expect_no_error(obj_assert_type(list(x = 1L, y = NULL), t))
  expect_error(
    obj_assert_type(list(x = NULL, y = NULL), t),
    class = "type_error_mistyped_obj"
  )
  expect_error(
    obj_assert_type(list(x = 1L, y = 1L), t),
    class = "type_error_mistyped_obj"
  )
})

test_that("exclusive() works as expected within argument type checks", {
  f <- typed(
    exclusive(x, y),
    function(x = optional(t_any), y = optional(t_any)) { NULL }
  )

  expect_no_error(f(x = 1L))
  expect_no_error(f(y = "a"))

  expect_error(f(), class = "type_error_mistyped_arg")
  expect_error(f(x = 1L, y = "a"), class = "type_error_mistyped_arg")
})

test_that("exclusive() description and diagnosis are as expected", {
  skip_on_covr()

  t <- t_any |> has_relation(exclusive(on_elm("x"), on_elm("y")))

  expect_snapshot(obj_inspect_type(list(x = 1L, y = NULL), t))
  expect_snapshot(obj_inspect_type(list(x = NULL, y = NULL), t))
  expect_snapshot(obj_inspect_type(list(x = 1L, y = 1L), t))
})
