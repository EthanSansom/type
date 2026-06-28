# Check that object is a superset

`contains()` returns a copy of `type` that requires objects to contain
all elements of `values`, checked via
[`vctrs::vec_in()`](https://vctrs.r-lib.org/reference/vec_match.html).

## Usage

``` r
contains(type, values)
```

## Arguments

- type:

  A type.

- values:

  A non-empty, non-list vector of values that objects must contain.

## Value

A copy of `type` with an additional constraint that objects contain all
elements of `values`.

## Examples

``` r
t_rgb <- t_chr |> contains(c("r", "g", "b"))
obj_inspect_type(c("r", "g", "b", "a"), t_rgb)
#> Object `c("r", "g", "b", "a")` has the expected type.
#> ✔ `c("r", "g", "b", "a")` is a bare <character>.
#> ✔ `c("r", "g", "b", "a")` contains elements: `c("r", "g", "b")`.
obj_inspect_type(c("r", "g"), t_rgb)
#> Object `c("r", "g")` does not have the expected type.
#> ✔ `c("r", "g")` is a bare <character>.
#> ℹ `c("r", "g")` must contain elements: `c("r", "g", "b")`.
#> ✖ `c("r", "g")` is missing 1 element: `"b"`.
```
