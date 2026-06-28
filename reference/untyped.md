# Remove argument and return type validation from a typed function

`untyped()` removes argument and return types from a typed function
declared via
[`typed()`](https://ethansansom.github.io/type/reference/typed.md). If
`fun` is a non-typed function, then `fun` is returned as is.

## Usage

``` r
untyped(fun)
```

## Arguments

- fun:

  A function to untype.

## Value

A function, without
[`typed()`](https://ethansansom.github.io/type/reference/typed.md)
arguments or return values.

## Examples

``` r
foo <- typed(function(x = t_int) { x })
print(foo)
#> <typed>
#> function (x) 
#> {
#>     x
#> }
#> <environment: 0x55df1c4a6728>
#> Arguments:
#> • `x` is a bare <integer>.
#> Returns:
#> • `<result>` is an R object.
print(untyped(foo))
#> function (x) 
#> {
#>     x
#> }
#> <environment: 0x55df1c4a6728>
```
