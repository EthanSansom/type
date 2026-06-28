# Check for duplicate values

`unduplicated()` returns a copy of `type` that requires objects to
contain no duplicate values, checked via
[`vctrs::vec_duplicate_any()`](https://vctrs.r-lib.org/reference/vec_duplicate.html).

## Usage

``` r
unduplicated(type)
```

## Arguments

- type:

  A type.

## Value

A copy of `type` with an additional non-duplicate constraint.

## Examples

``` r
t_ids <- t_chr |> unduplicated()
obj_inspect_type(c("a1", "a2"), t_ids)
#> Object `c("a1", "a2")` has the expected type.
#> ✔ `c("a1", "a2")` is a bare <character>.
#> ✔ `c("a1", "a2")` contains no duplicated values.
obj_inspect_type(c("a1", "b1", "b1"), t_ids)
#> Object `c("a1", "b1", "b1")` does not have the expected type.
#> ✔ `c("a1", "b1", "b1")` is a bare <character>.
#> ℹ `c("a1", "b1", "b1")` must not contain duplicate elements.
#> ✖ `c("a1", "b1", "b1")` contains duplicate elements at locations `c(2, 3)`.
```
