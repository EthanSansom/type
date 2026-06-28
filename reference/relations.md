# Set between-element type constraints

Relations constrain how a set of values must relate to one another. Each
relation function's behaviour depends on context:

- Inside
  [`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md):
  constrains selected elements or attributes of an object.

- Inside
  [`typed()`](https://ethansansom.github.io/type/reference/typed.md):
  constrains the specified function arguments at call time.

The following relations are provided:

- `same_sized()`: all values must share the same size, checked via
  [`vctrs::vec_size()`](https://vctrs.r-lib.org/reference/vec_size.html).

- `same_classed()`: all values must share the same class.

- `recyclable()`: all values must be size 1 or share the same size.

- `exclusive()`: exactly one value must be non-`NULL` (in
  [`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md))
  or supplied (in
  [`typed()`](https://ethansansom.github.io/type/reference/typed.md)).

## Usage

``` r
same_classed(...)

same_sized(...)

recyclable(...)

exclusive(...)
```

## Arguments

- ...:

  Inside
  [`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md),
  one or more selectors (e.g.
  [`on_elm()`](https://ethansansom.github.io/type/reference/on.md),
  [`on_elms()`](https://ethansansom.github.io/type/reference/on.md),
  [`on_attr()`](https://ethansansom.github.io/type/reference/on.md),
  [`on()`](https://ethansansom.github.io/type/reference/on.md)).

  Inside
  [`typed()`](https://ethansansom.github.io/type/reference/typed.md),
  one or more arguments.

## See also

[`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md)
to attach a relation to a type,
[`typed()`](https://ethansansom.github.io/type/reference/typed.md) for
typed function construction.

## Examples

``` r
# Require elements be the same size
t <- t_any |> has_relation(same_sized(on_elm("a"), on_elm("b")))
obj_inspect_type(list(a = 1:3, b = 1:3), t, obj_name = "obj")
#> Object `obj` has the expected type.
#> ✔ `obj[["a"]]` and `obj[["b"]]` are the same size.
obj_inspect_type(list(a = 1:3, b = 1:2), t, obj_name = "obj")
#> Object `obj` does not have the expected type.
#> ℹ `obj[["a"]]` and `obj[["b"]]` must be the same size.
#> ✖ `obj[["a"]]` is size 3 and `obj[["b"]]` is size 2.

# Require elements to share a class
t <- t_any |> has_relation(same_classed(on_elm(1L), on_elm(2L)))
obj_inspect_type(list(1L, 2L), t)
#> Object `list(1L, 2L)` has the expected type.
#> ✔ `list(1L, 2L)[[1]]` and `list(1L, 2L)[[2]]` have the same class.
obj_inspect_type(list(1L, "a"), t)
#> Object `list(1L, "a")` does not have the expected type.
#> ℹ `list(1L, "a")[[1]]` and `list(1L, "a")[[2]]` must have the same class.
#> ✖ `list(1L, "a")[[1]]` has class <integer> and `list(1L, "a")[[2]]` has class
#>   <character>.

# Require elements be size 1 or the same size
t <- t_any |> has_relation(recyclable(on_elm("x"), on_elm("y")))
obj_inspect_type(list(x = 1L, y = 1:3), t)
#> Object `list(x = 1L, y = 1:3)` has the expected type.
#> ✔ `list(x = 1L, y = 1:3)[["x"]]` and `list(x = 1L, y = 1:3)[["y"]]` are
#>   recyclable.
obj_inspect_type(list(x = 1:2, y = 1:3), t)
#> Object `list(x = 1:2, y = 1:3)` does not have the expected type.
#> ℹ `list(x = 1:2, y = 1:3)[["x"]]` and `list(x = 1:2, y = 1:3)[["y"]]` must be
#>   recyclable.
#> ✖ `list(x = 1:2, y = 1:3)[["x"]]` (size 2) and `list(x = 1:2, y = 1:3)[["y"]]`
#>   (size 3) have incompatible sizes.

# Require that exactly one element must be non-NULL or supplied
t <- t_any |> has_relation(exclusive(on_elm("x"), on_elm("y")))
obj_inspect_type(list(x = 1L, y = NULL), t)
#> Object `list(x = 1L, y = NULL)` has the expected type.
#> ✔ Exactly one of `list(x = 1L, y = NULL)[["x"]]` and `list(x = 1L, y =
#>   NULL)[["y"]]` are non-NULL.
obj_inspect_type(list(x = 1L, y = 1L), t)
#> Object `list(x = 1L, y = 1L)` does not have the expected type.
#> ℹ Exactly one of `list(x = 1L, y = 1L)[["x"]]` and `list(x = 1L, y =
#>   1L)[["y"]]` must be non-NULL.
#> ✖ `list(x = 1L, y = 1L)[["x"]]` and `list(x = 1L, y = 1L)[["y"]]` are all
#>   non-NULL.

f <- typed(
  exclusive(x, y),
  function(x = optional(t_any), y = optional(t_any)) NULL
)
f(x = 1L)
#> NULL
try(f(x = 1L, y = "a"))
#> Error in f(x = 1L, y = "a") : 
#>   Exactly one of arguments `x` and `y` must be supplied.
#> ✖ `x` and `y` are all supplied.
```
