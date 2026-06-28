# Add an element or attribute constraint to a type

`has()` returns a copy of `type` that requires a selected part of an
object to have type `on_type`. The part is identified by a selector (see
[`on()`](https://ethansansom.github.io/type/reference/on.md)).

    # Require that element `[[1]]` is an integer
    t_any |> has(on_elm(1L), t_int)

    # Require that the "dim" attribute is an integer vector of size 2
    t_any |> has(on_attr("dim"), t_int |> sized(2L))

    # Require that names contains "x"
    t_any |> has(on(names), t_chr |> contains("x"))

Constraints can be composed with the pipe operator `|>`:

    t_coords <- t_list |>
      has(on_elm("lat"), t_dbl |> bounded(-90, 90)) |>
      has(on_elm("lon"), t_dbl |> bounded(-180, 180))

## Usage

``` r
has(type, selector, on_type)
```

## Arguments

- type:

  A type.

- selector:

  A selector, e.g. the result of
  [`on()`](https://ethansansom.github.io/type/reference/on.md),
  [`on_elm()`](https://ethansansom.github.io/type/reference/on.md), or
  [`on_attr()`](https://ethansansom.github.io/type/reference/on.md).

- on_type:

  A type to check the selected value against.

## Value

A copy of `type` with an additional element or attributeconstraint.

## See also

[`on()`](https://ethansansom.github.io/type/reference/on.md) for
available selectors,
[`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md)
to add between-element constraints.

## Examples

``` r
# Constrain a list element by position or name
t_pair <- t_list |>
  has(on_elm(1L), t_chr) |>
  has(on_elm("a"), t_int)

obj_is_type(list("a", a = 1L), t_pair)
#> [1] TRUE
obj_is_type(list("a", a = 1.5), t_pair)
#> [1] FALSE

# on() accepts any accessor call, using .x as a placeholder
t_short <- t_chr |> has(on(length(.x)), t_int |> bounded(1L, 5L))

obj_is_type(c("a", "b"), t_short)
#> [1] TRUE
obj_is_type(character(), t_short)
#> [1] FALSE
obj_is_type(letters, t_short)
#> [1] FALSE

# on() also accepts a bare function name as shorthand for f(.x)
t_dict <- t_list |> has(on(names), t_chr |> unduplicated())

obj_is_type(list(x = 1, y = 2), t_dict)
#> [1] TRUE
obj_is_type(list(x = 1, x = 2), t_dict)
#> [1] FALSE

# on_each() checks the type of every element
t_list_of_dbl <- t_list |> has(on_each(), t_dbl)

obj_is_type(list(1.1, 2.2, 3.3), t_list_of_dbl)
#> [1] TRUE
obj_is_type(list(1.1, 2L, 3.3), t_list_of_dbl)
#> [1] FALSE
```
