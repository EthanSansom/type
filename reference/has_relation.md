# Add a between-element constraint to a type

`has_relation()` returns a copy of `type` that requires a relationship
to hold between selected parts of an object. Parts are selected using
selector functions (see
[on](https://ethansansom.github.io/type/reference/on.md)) and the
relationship is expressed as a relation (see
[`same_sized()`](https://ethansansom.github.io/type/reference/relations.md),
[`same_classed()`](https://ethansansom.github.io/type/reference/relations.md),
[`exclusive()`](https://ethansansom.github.io/type/reference/relations.md)).

    # Require that elements `[[1]]` and `[[2]]` are the same size
    t_any |> has_relation(same_sized(on_elm(1L), on_elm(2L)))

    # Require that attributes "x" and "y" have the same class
    t_any |> has_relation(same_classed(on_attr("x"), on_attr("y")))

    # Require that only one of attributes "x" and "y" are non-NULL
    t_any |> has_relation(exclusive(on_attrs(c("x", "y")))

[`has()`](https://ethansansom.github.io/type/reference/has.md) and
`has_relation()` can be composed:

    t_coords <- t_list |>
      has(on_elm("lat"), t_dbl |> bounded(-90, 90)) |>
      has(on_elm("lon"), t_dbl |> bounded(-180, 180)) |>
      has_relation(same_sized(on_elm("lat"), on_elm("lon")))

## Usage

``` r
has_relation(type, relation)
```

## Arguments

- type:

  A type.

- relation:

  A relation, e.g. the result of
  [`same_sized()`](https://ethansansom.github.io/type/reference/relations.md),
  [`same_classed()`](https://ethansansom.github.io/type/reference/relations.md),
  or
  [`exclusive()`](https://ethansansom.github.io/type/reference/relations.md).

## Value

A copy of `type` with an additional between-element constraint.

## See also

[`has()`](https://ethansansom.github.io/type/reference/has.md) to add
per-element constraints.

## Examples

``` r
# Require that elements `[[1]]` and `[[2]]` are the same size
t <- t_any |> has_relation(same_sized(on_elm(1L), on_elm(2L)))
obj_inspect_type(list(1:3, 1:3), t)
#> Object `list(1:3, 1:3)` has the expected type.
#> ✔ `list(1:3, 1:3)[[1]]` and `list(1:3, 1:3)[[2]]` are the same size.
obj_inspect_type(list(1:3, 1:2), t)
#> Object `list(1:3, 1:2)` does not have the expected type.
#> ℹ `list(1:3, 1:2)[[1]]` and `list(1:3, 1:2)[[2]]` must be the same size.
#> ✖ `list(1:3, 1:2)[[1]]` is size 3 and `list(1:3, 1:2)[[2]]` is size 2.

# Require that attributes "x" and "y" have the same class
t <- t_any |> has_relation(same_classed(on_attr("x"), on_attr("y")))
good <- structure(list(), x = 1L, y = 2L)
bad <- structure(list(), x = 1L, y = "a")
obj_inspect_type(good, t)
#> Object `good` has the expected type.
#> ✔ `attr(good, "x")` and `attr(good, "y")` have the same class.
obj_inspect_type(bad, t)
#> Object `bad` does not have the expected type.
#> ℹ `attr(bad, "x")` and `attr(bad, "y")` must have the same class.
#> ✖ `attr(bad, "x")` has class <integer> and `attr(bad, "y")` has class
#>   <character>.
```
