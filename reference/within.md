# Check that object is a subset

`within()` returns a copy of `type` that requires every element of an
object to be within `values`, checked via
[`vctrs::vec_in()`](https://vctrs.r-lib.org/reference/vec_match.html).

## Usage

``` r
within(type, values)
```

## Arguments

- type:

  A type.

- values:

  A non-empty, non-list vector of values that elements of object must be
  within.

## Value

A copy of `type` with an additional constraint that objects are within
`values`.

## Examples

``` r
t_weekend <- t_chr |> within(c("Sat", "Sun"))
obj_inspect_type("Sat", t_weekend)
#> Object `"Sat"` has the expected type.
#> ✔ `"Sat"` is a bare <character>.
#> ✔ `"Sat"` contains only values from: `c("Sat", "Sun")`.
obj_inspect_type(c("A", "B"), t_weekend)
#> Object `c("A", "B")` does not have the expected type.
#> ✔ `c("A", "B")` is a bare <character>.
#> ℹ `c("A", "B")` may only contain values from: `c("Sat", "Sun")`.
#> ✖ `c("A", "B")` contains 2 unexpected elements: `c("A", "B")`.
```
