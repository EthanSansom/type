# Select an element or attribute to type

Selectors identify a part of an object for use in
[`has()`](https://ethansansom.github.io/type/reference/has.md) and
relation functions such as
[`same_sized()`](https://ethansansom.github.io/type/reference/relations.md)
and
[`same_classed()`](https://ethansansom.github.io/type/reference/relations.md).

These are useful for typing elements of a container type, such as a
list. For example, the following defines a coordinate type `t_coords`,
which is a list containing named numeric elements `"lat"` and `"lon"` of
the same size:

    t_coords <- t_list |>
      has(on_elm("lat"), t_dbl |> bounded(-90, 90)) |>
      has(on_elm("lon"), t_dbl |> bounded(-180, 180)) |>
      has_relation(same_sized(on_elm("lat"), on_elm("lon")))

`on()` specifies an `accessor` function, either a call or a function
name, applied to an object during type checking. If `accessor` is a
call, `.x` may be used as a placeholder for the object.

    # Require an object's length to be between 1 and 10 (inclusive)
    t_any |> has(on(length(.x)), t_int |> bounded(1L, 10L))

    # Require an object's names to contain "x" and "y"
    # Equivilant to `on(names(.x))`
    t_any |> has(on(names), t_chr |> contains(c("x", "y")))

`on_elm(index)` and `on_attr(name)` are convenience selectors for the
common cases of `on(.x[[index]])` and `on(attr(.x, name))`.

    has(t_any, on_elm(1L), t_int)     # same as `on(.x[[1L]])`
    has(t_any, on_attr("dim"), t_int) # same as `on(attr(.x, "dim"))`

`on_elms(indices)` and `on_attrs(names)` are the multiple selector forms
of `on_elm()` and `on_attr()`, useful for selecting multiple elements in
a relation:

    # These are equivilant
    t_any |> has_relation(same_sized(on_elms(c(1L, 2L))))
    t_any |> has_relation(same_sized(on_elm(1L), on_elm(2L)))

`on_data()` selects the underlying data of an object after it's
attributes and class has been removed (via
[`unclass()`](https://rdrr.io/r/base/class.html)).

    # POSIXct datetime vectors store time as a double
    t_posixct <- t_any |>
      classed("POSIXct") |>
      has(on_data(), t_dbl) |>
      has(on_attr("tzone"), t_chr |> sized(1L))

`on_each()` selects all elements of an object, applying the type check
to each one individually.

    # Defines a list-of-integers type
    t_list_of_int <- t_list |> has(on_each(), t_int)

## Usage

``` r
on(accessor)

on_attr(name)

on_elm(index)

on_data()

on_each()

on_elms(indices)

on_attrs(attrs)
```

## Arguments

- accessor:

  A call using `.x` as the object placeholder, or a bare symbol `f` as
  shorthand for `f(.x)`.

- name:

  For `on_attr()`, a single attribute name (character).

- index:

  For `on_elm()`, a single position (integer) or name (character).

- indices:

  For `on_elms()`, positions (integer) or names (character).

- attrs:

  For `on_attrs()`, attribute names (character).

## Value

A selector. These functions may only be used within
[`has()`](https://ethansansom.github.io/type/reference/has.md) and
relations such as
[`same_sized()`](https://ethansansom.github.io/type/reference/relations.md)
and
[`same_classed()`](https://ethansansom.github.io/type/reference/relations.md).
Outside of these contexts, the `on()` functions raise an error.

## See also

[`has()`](https://ethansansom.github.io/type/reference/has.md) to attach
a selector to a type,
[`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md)
to attatch a relation to a type.

## Examples

``` r
t_coords <- t_list |>
  has(on(names), t_chr |> setequal_to(c("lat", "lon"))) |>
  has(on_elm("lat"), t_dbl |> bounded(-90, 90)) |>
  has(on_elm("lon"), t_dbl |> bounded(-180, 180)) |>
  has_relation(same_sized(on_elms(c("lat", "lon"))))

good <- list(lat = c(70, 20, -50), lon = c(100, 0, 85))
obj_is_type(good, t_coords)
#> [1] TRUE

bad <- list(lat = c(1, 7), lon = 90)
obj_inspect_type(bad, t_coords)
#> Object `bad` does not have the expected type.
#> ✔ `bad` is a bare <list>.
#> ✔ `names(bad)` is a bare <character>.
#> ✔ `names(bad)` is setequal to: `c("lat", "lon")`.
#> ✔ `bad[["lat"]]` is a bare <double>.
#> ✔ `bad[["lat"]]` is bounded by [-90, 90].
#> ✔ `bad[["lon"]]` is a bare <double>.
#> ✔ `bad[["lon"]]` is bounded by [-180, 180].
#> ℹ `bad[["lat"]]` and `bad[["lon"]]` must be the same size.
#> ✖ `bad[["lat"]]` is size 2 and `bad[["lon"]]` is size 1.
```
