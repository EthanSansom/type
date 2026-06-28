# Modify a type

Type modifiers adjust how a type is enforced in a given context:

- `const(type)`: For use in `%:%`. Declares a constant.

- `optional(type)`: For use in
  [`typed()`](https://ethansansom.github.io/type/reference/typed.md).
  Allows an argument to be missing.

- `maybe(type)`: For use in
  [`typed()`](https://ethansansom.github.io/type/reference/typed.md).
  Allows an argument to be `NULL`.

## Usage

``` r
const(type)

optional(type)

maybe(type)
```

## Arguments

- type:

  A type, e.g.
  [t_int](https://ethansansom.github.io/type/reference/base-types.md).

## Value

A modified copy of `type`.

## Examples

``` r
# const() variables can't be assigned to
const(t_int) %:% x(1L)
x
#> [1] 1
try(x <- 2L)
#> Error in eval(expr, envir) : Can't assign to the constant `x`.
#> ℹ Run `last_type()` to get the expected type.

# optional() arguments may be missing
f <- typed(function(x = optional(t_int)) {
  if (missing(x)) { "absent" } else { x }
})
f()
#> [1] "absent"
f(1L)
#> [1] 1

# maybe() arguments may be `NULL`
g <- typed(function(x = maybe(t_int)) x)
g(NULL)
#> NULL
g(1L)
#> [1] 1
```
