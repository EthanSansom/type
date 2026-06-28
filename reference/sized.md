# Check size

`sized()` returns a copy of `type` that requires objects to be size
`size`, checked via
[`vctrs::vec_size()`](https://vctrs.r-lib.org/reference/vec_size.html).

    obj_is_type(1, t_any |> sized(2L))   # FALSE
    obj_is_type(1:2, t_any |> sized(2L)) # TRUE

## Usage

``` r
sized(type, size)
```

## Arguments

- type:

  A type.

- size:

  A non-negative count.

## Value

A copy of `type` with an additional size constraint.

## Examples

``` r
t_int2 <- t_int |> sized(2L)
obj_inspect_type(10L, t_int2)
#> Object `10L` does not have the expected type.
#> ✔ `10L` is a bare <integer>.
#> ✖ `10L` must be size 2, not size 1.
obj_inspect_type(1:2, t_int2)
#> Object `1:2` has the expected type.
#> ✔ `1:2` is a bare <integer>.
#> ✔ `1:2` is size 2.
```
