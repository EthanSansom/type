# Check that object is identical to a vector

`same_as()` returns a copy of `type` that requires an object to be
identical to `values`, checked via
[`vctrs::vec_equal()`](https://vctrs.r-lib.org/reference/vec_equal.html).

## Usage

``` r
same_as(type, values)
```

## Arguments

- type:

  A type.

- values:

  A non-empty, non-list vector that an object must be identical to.

## Value

A copy of `type` with an additional constraint that elements of objects
are identical to `values`.

## See also

[`setequal_to()`](https://ethansansom.github.io/type/reference/setequal_to.md)
to test for equality, ignoring duplicates.

## Examples

``` r
t_abc <- t_chr |> same_as(c("a", "b", "c"))
obj_inspect_type(c("a", "b", "c"), t_abc)
#> Object `c("a", "b", "c")` has the expected type.
#> ✔ `c("a", "b", "c")` is a bare <character>.
#> ✔ `c("a", "b", "c")` is the same as: `c("a", "b", "c")`.
obj_inspect_type(c("c", "b", "a"), t_abc)
#> Object `c("c", "b", "a")` does not have the expected type.
#> ✔ `c("c", "b", "a")` is a bare <character>.
#> ℹ `c("c", "b", "a")` must be: `c("a", "b", "c")`.
#> ✖ `c("c", "b", "a")` differs at locations `c(1, 3)`:
#> • Actual: `c("c", "a")`
#> • Expected: `c("a", "c")`
```
