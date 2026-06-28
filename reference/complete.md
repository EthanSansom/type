# Check for missing values

`complete()` returns a copy of `type` that requires objects to contain
no missing (i.e. `NA`) values, checked via
[`vctrs::vec_any_missing()`](https://vctrs.r-lib.org/reference/missing.html).

## Usage

``` r
complete(type)
```

## Arguments

- type:

  A type.

## Value

A copy of `type` with an additional non-missingness constraint.

## Examples

``` r
t_real <- t_dbl |> complete()
obj_inspect_type(10.6, t_real)
#> Object `10.6` has the expected type.
#> ✔ `10.6` is a bare <double>.
#> ✔ `10.6` contains no missing values.
obj_inspect_type(c(1, NA), t_real)
#> Object `c(1, NA)` does not have the expected type.
#> ✔ `c(1, NA)` is a bare <double>.
#> ℹ `c(1, NA)` must not contain missing elements.
#> ✖ `c(1, NA)` is NA at location `2`.
```
