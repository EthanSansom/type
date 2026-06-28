# Declare a type union

`type_union()` returns a type that requires an object to satisfy at
least one of the types supplied to `...`.

    # Must be an integer OR a double
    t_number <- type_union(t_int, t_dbl)

    # Must be a string OR NULL
    t_opt_string <- type_union(t_string, t_null)

Nested unions are flattened, so
`type_union(type_union(t_int, t_dbl), t_chr)` is equivalent to
`type_union(t_int, t_dbl, t_chr)`. Duplicate types are silently dropped.

## Usage

``` r
type_union(...)
```

## Arguments

- ...:

  Two or more types to combine. At least one must be supplied.

## Value

A type that accepts objects matching any of the supplied types.

## See also

[`is_type()`](https://ethansansom.github.io/type/reference/type-predicates.md),
[`is_type_union()`](https://ethansansom.github.io/type/reference/type-predicates.md)
to test whether an object is a any type a or type union.

## Examples

``` r
t_index <- type_union(t_int, t_chr)
obj_is_type(1L, t_index)
#> [1] TRUE
obj_is_type("a", t_index)
#> [1] TRUE
obj_is_type(10.6, t_index)
#> [1] FALSE

# Useful for nullable types
t_opt_chr <- type_union(t_chr, t_null)
obj_is_type(c("a", "b"), t_opt_chr)
#> [1] TRUE
obj_is_type(NULL, t_opt_chr)
#> [1] TRUE
obj_is_type(1L, t_opt_chr)
#> [1] FALSE
```
