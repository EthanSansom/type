# Declare a typed object

`%:%` declares a typed variable in the calling environment. The
right-hand side must be a declaration of the form `name(value)`, where
`name` is the variable to create and `value` is its initial value:

    t_int %:% x(1L)
    x        # 1L
    x <- 2L  # ok
    x <- "A" # error, "A" is not an integer

A typed variable behaves like a regular object, but is type checked on
every assignment. A constant variable may be declared using
[`const()`](https://ethansansom.github.io/type/reference/modifiers.md),
in which case the variable cannot be modified.

    const(t_int) %:% x(1L)
    x <- 2L # error, can't assign to a constant

## Usage

``` r
type %:% declaration
```

## Arguments

- type:

  A type, optionally wrapped in
  [`const()`](https://ethansansom.github.io/type/reference/modifiers.md).

- declaration:

  A declaration of the form `name(value)`. `name` becomes the variable
  name and `value` is used as the variable's initial value.

## Value

The initial value, invisibly.

## See also

Modifier
[`const()`](https://ethansansom.github.io/type/reference/modifiers.md),
[`typed()`](https://ethansansom.github.io/type/reference/typed.md) for
type-checking functions.

## Examples

``` r
t_chr |> sized(1L) %:% x("hello")
const(t_bool) %:% y(TRUE)

# `x` must be a scalar character
x <- "goodbye"
try(x <- 10)
#> Error in eval(expr, envir) : Attempted to assign a mistyped value to `y`.
#> ✖ `<value>` must be a bare <character>, not a bare <double>.
#> ℹ Run `last_type()` to get the expected type.

# `y` can't be assigned to
try(y <- FALSE)
#> Error in eval(expr, envir) : Can't assign to the constant `y`.
#> ℹ Run `last_type()` to get the expected type.

# Typed variables can be re-declared as a different type
t_chr %:% y("true")
```
