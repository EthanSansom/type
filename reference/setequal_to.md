# Check that object is a given set

`setequal_to()` returns a copy of `type` that requires an object to be
setequal to `values`, checked via
[`vctrs::vec_in()`](https://vctrs.r-lib.org/reference/vec_match.html).

## Usage

``` r
setequal_to(type, values)
```

## Arguments

- type:

  A type.

- values:

  A non-empty, non-list vector of values that an object must be setequal
  to.

## Value

A copy of `type` with an additional constraint that objects are setequal
to `values`.

## Examples

``` r
t_three <- t_int |> setequal_to(1:3)
obj_inspect_type(c(1L, 2L, 3L, 1L), t_three)
#> Object `c(1L, 2L, 3L, 1L)` has the expected type.
#> ✔ `c(1L, 2L, 3L, 1L)` is a bare <integer>.
#> ✔ `c(1L, 2L, 3L, 1L)` is setequal to: `c(1, 2, 3)`.
obj_inspect_type(10L, t_three)
#> Object `10L` does not have the expected type.
#> ✔ `10L` is a bare <integer>.
#> ℹ `10L` must contain exactly: `c(1, 2, 3)`.
#> ✖ `10L` is missing 3 elements: `c(1, 2, 3)`.
#> ✖ `10L` contains 1 unexpected element: `10`.
```
