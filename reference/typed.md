# Declare a type-checked function

`typed()` inserts argument and (optionally) return type validation into
a function. When printed, a typed function shows the types of its
arguments and return value.

## Usage

``` r
typed(..., returns = NULL)
```

## Arguments

- ...:

  A type annotated function definition, optionally accompanied by one or
  more relation calls (e.g.
  [`same_sized()`](https://ethansansom.github.io/type/reference/relations.md)).
  Exactly one function definition must be supplied.

- returns:

  A type for the function's return value. By default `returns` is
  `NULL`, meaning the return value can have any type.

## Value

A typed function.

## Argument Typing

A typed function's arguments may be annotated with any valid type, e.g.
[t_int](https://ethansansom.github.io/type/reference/base-types.md),
including types refined with additional traits, e.g.
[`sized()`](https://ethansansom.github.io/type/reference/sized.md).

    f <- typed(function(x = t_int, y = t_chr |> sized(1L)) { paste(x, y) })
    f(1L, "a")         # ok
    f(TRUE, "a")       # error, `x` is not an integer
    f(1L, c("a", "b")) # error, `y` is not a size 1

By default, annotated function arguments have no default value. To set
one, use the syntax `arg = <type> %:% <default>`.

    f <- typed(function(x = t_int %:% 0L) { x })
    f()    # 0L
    f(1L)  # 1L

Dots (`...`) may also be type annotated. By default, each dot will be
checked against the supplied type. To treat the dots as a single
argument, e.g. as a list, use the
[t_dots](https://ethansansom.github.io/type/reference/base-types.md)
annotation.

    # Each `...` must be an integer
    f <- typed(function(... = t_int) list(...))

    # Exactly 2 dots must be supplied
    g <- typed(function(... = t_dots |> sized(2L)) list(...))

Arguments, with the exception of `...`, may be modified using
[`optional()`](https://ethansansom.github.io/type/reference/modifiers.md)
or
[`maybe()`](https://ethansansom.github.io/type/reference/modifiers.md).
[`optional()`](https://ethansansom.github.io/type/reference/modifiers.md)
arguments may by unsupplied while
[`maybe()`](https://ethansansom.github.io/type/reference/modifiers.md)
arguments may be `NULL`.

    # `x` is an integer or is unsupplied
    f <- typed(function(x = optional(t_int)) if (missing(x)) 0L else x)

    # `x` is an integer or `NULL`
    g <- typed(function(x = maybe(t_int)) x)

## Relations

Relations, e.g.
[`same_sized()`](https://ethansansom.github.io/type/reference/relations.md),
declare between-argument checks. A relation call may be placed before or
after the function definition.

    # `x` and `y` must be integers of the same size
    f <- typed(
      same_sized(x, y),
      function(x = t_int, y = t_int) x + y
    )

## Return Typing

Use the `returns` argument to enforce a type on the return value. Return
types cannot be modified using
[`optional()`](https://ethansansom.github.io/type/reference/modifiers.md)
or
[`maybe()`](https://ethansansom.github.io/type/reference/modifiers.md).

    f <- typed(
      function(x = t_bool) if (x) "yes" else "no",
      returns = t_chr
    )

## See also

[`untyped()`](https://ethansansom.github.io/type/reference/untyped.md)
for un-typing a function,
[`optional()`](https://ethansansom.github.io/type/reference/modifiers.md)
and
[`maybe()`](https://ethansansom.github.io/type/reference/modifiers.md)
for argument modifications,
[`same_sized()`](https://ethansansom.github.io/type/reference/relations.md)
and
[`same_classed()`](https://ethansansom.github.io/type/reference/relations.md)
for between-argument constraints.

## Examples

``` r
any2 <- typed(
  function(... = t_lgl, na.rm = t_bool %:% FALSE) { 
    any(..., na.rm) 
  },
  returns = t_lgl |> sized(1L)
)
print(any2)
#> <typed>
#> function (..., na.rm = FALSE) 
#> {
#>     any(..., na.rm)
#> }
#> <environment: 0x563a7018eac8>
#> Arguments:
#> • Each element of `...` is a bare <logical>.
#> • `na.rm` is a bare <logical>.
#> • `na.rm` is size 1.
#> • `na.rm` contains no missing values.
#> Returns:
#> • `<result>` is a bare <logical>.
#> • `<result>` is size 1.

# Correctly typed inputs proceed normally
any2(c(TRUE, FALSE), TRUE, na.rm = FALSE)
#> [1] TRUE

# Mistyped inputs cause an error
try(any2(TRUE, na.rm = "no"))
#> Error in any2(TRUE, na.rm = "no") : Argument `na.rm` is mistyped.
#> ✖ `na.rm` must be a bare <logical>, not a bare <character>.
#> ℹ Run `last_type()` to get the expected type.
```
