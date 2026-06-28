# Test if an object is a type

Test if an object is a type

## Usage

``` r
is_type(x)

is_type_union(x)
```

## Arguments

- x:

  An object to test.

## Value

`TRUE` if `x` is a type, `FALSE` otherwise.

## Examples

``` r
is_type(10L)
#> [1] FALSE
is_type(t_int)
#> [1] TRUE
```
