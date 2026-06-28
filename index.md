# type

{type} provides a lightweight type system for R, meant for users to
quickly and easily add run-time type validation to functions and
variables. The {type} package supports:

- Adding type annotations to function arguments and return values.
- Declaring typed variables and constants.
- Defining new types and type aliases.

## Installation

You can install the development version of {type} from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("EthanSansom/type")
```

## Typed Objects

{type} provides a collection of built-in types, such as `t_int`
(integer) and `t_bool` (boolean), which may be used to annotate objects,
function arguments, and return values.

``` r

t_int %:% x(10L)
```

A variable’s type may be declared using the `%:%` operator, which
constrains the values which may be assigned to it.

``` r

# Assigning another integer to `x` is okay
x <- 1:3

# But non-integer assignment raises an error
x <- "A"
#> Error:
#> ! Attempted to assign a mistyped value to `x`.
#> ✖ `<value>` must be a bare <integer>, not a bare <character>.
#> ℹ Run `last_type()` to get the expected type.
```

Every type in {type} is composed of traits, functions which add
additional restrictions to a type. For example, the `t_string` type is
built from the
[`sized()`](https://ethansansom.github.io/type/reference/sized.md) and
[`complete()`](https://ethansansom.github.io/type/reference/complete.md)
traits:

``` r

# A string is:
t_string <- t_chr |> # A character vector
  sized(1L) |>       # Of size 1
  complete()         # Which is non-missing
```

The type of any object can be checked using
[`obj_is_type()`](https://ethansansom.github.io/type/reference/obj-type.md),
[`obj_inspect_type()`](https://ethansansom.github.io/type/reference/obj-type.md),
and
[`obj_assert_type()`](https://ethansansom.github.io/type/reference/obj-type.md):

``` r

# Returns TRUE or FALSE
obj_is_type(NA_character_, t_string)
#> [1] FALSE

# Prints a diagnostic message
obj_inspect_type(NA_character_, t_string)
#> Object `NA_character_` does not have the expected type.
#> ✔ `NA_character_` is a bare <character>.
#> ✔ `NA_character_` is size 1.
#> ℹ `NA_character_` must not contain missing elements.
#> ✖ `NA_character_` is NA at location `1`.

# Raises an error (on failure) or returns `NULL` (on success)
obj_assert_type(NA_character_, t_string)
#> Error in `obj_assert_type()`:
#> ! Object `NA_character_` is mistyped.
#> ℹ `NA_character_` must not contain missing elements.
#> ✖ `NA_character_` is NA at location `1`.
#> ℹ Run `last_type()` to get the expected type.
```

To support more complex type definitions, {type} provides a small
dialect for setting type constraints on parts of an object using
[`on()`](https://ethansansom.github.io/type/reference/on.md) selector
functions,
[`has()`](https://ethansansom.github.io/type/reference/has.md), and
[`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md):

``` r

# `names()` of an object must contain "x" and "y"
t_any |> has(on(names), t_chr |> contains(c("x", "y")))
#> <type>
#> • `names(<object>)` is a bare <character>.
#> • `names(<object>)` contains elements: `c("x", "y")`.

# Element `[[1]]` of an object must be a function
t_any |> has(on_elm(1L), t_fun)
#> <type>
#> • `<object>[[1]]` is a function.

# Every element of the object must be the same size
t_list |> has_relation(same_sized(on_each()))
#> <type>
#> • `<object>` is a bare <list>.
#> • Each element of `<object>` are the same size.
```

These are useful for building complex types from a simple set of traits.

``` r

t_point <- t_list |>
  has(on(names), t_chr |> setequal_to(c("x", "y"))) |>
  has(on_elm("x"), t_int |> complete()) |>
  has(on_elm("y"), t_int |> complete()) |>
  has_relation(same_sized(on_elms(c("x", "y"))))
 
good <- list(x = 1:2, y = 3:4)
bad <- list(x = c(1L, NA), y = c(0L, 0L))
 
obj_is_type(good, t_point)
#> [1] TRUE
obj_inspect_type(bad, t_point)
#> Object `bad` does not have the expected type.
#> ✔ `bad` is a bare <list>.
#> ✔ `names(bad)` is a bare <character>.
#> ✔ `names(bad)` is setequal to: `c("x", "y")`.
#> ℹ `bad[["x"]]` must not contain missing elements.
#> ✖ `bad[["x"]]` is NA at location `2`.
```

## Typed Functions

A typed function is declared using
[`typed()`](https://ethansansom.github.io/type/reference/typed.md):

``` r

safe_log <- typed(function(x = t_num, base = t_num %:% exp(1)) { 
  base::log(x, base = base) 
})
print(safe_log)
#> <typed>
#> function (x, base = exp(1)) 
#> {
#>     base::log(x, base = base)
#> }
#> <environment: 0x107a4a518>
#> Arguments:
#> • `x` is a numeric vector.
#> • `base` is a numeric vector.
#> Returns:
#> • `<result>` is an R object.
```

Arguments without default values are annotated using a type
(e.g. `x = t_num`) and default values are provided using the `%:%`
operator.

When called, a typed function checks that its inputs are of the correct
type and raises an error otherwise.

``` r

safe_log(10)
#> [1] 2.302585
safe_log("A")
#> Error in `safe_log()`:
#> ! Argument `x` is mistyped.
#> ✖ `x` must be a numeric vector, not the string "A".
#> ℹ Run `last_type()` to get the expected type.
```

In addition to argument types, the return type of a function may also be
specified using the `returns` argument.

``` r

safe_any <- typed(
  function(... = t_lgl, na.rm = t_bool %:% FALSE) {
    base::any(..., na.rm = na.rm)
  },
  returns = t_lgl |> sized(1L)
)
print(safe_any)
#> <typed>
#> function (..., na.rm = FALSE) 
#> {
#>     base::any(..., na.rm = na.rm)
#> }
#> <environment: 0x107a4a518>
#> Arguments:
#> • Each element of `...` is a bare <logical>.
#> • `na.rm` is a bare <logical>.
#> • `na.rm` is size 1.
#> • `na.rm` contains no missing values.
#> Returns:
#> • `<result>` is a bare <logical>.
#> • `<result>` is size 1.
```

Typed functions also support between-argument constaints, using any
trait compatible with
[`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md).
For example, `na_string()` requires that arguments `x` and `na_to` are
either the same size or are length-1, using the
[`recyclable()`](https://ethansansom.github.io/type/reference/relations.md)
trait.

``` r

na_string <- typed(
  recyclable(x, na_to),
  function(x = t_chr, na_to = t_chr |> complete()) {
    x[is.na(x)] <- na_to
    x
  },
  returns = t_chr |> complete()
)

na_string(c("hi", NA, "bye"), na_to = "N/A")
#> [1] "hi"  "N/A" "bye"
na_string(c("hi", NA), na_to = character())
#> Error in `na_string()`:
#> ! Arguments `x` and `na_to` must be recyclable.
#> ✖ `x` (size 2) and `na_to` (size 0) are incompatible sizes.
```
