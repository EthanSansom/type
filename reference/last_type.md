# Return the expected type of the last mistyped object

`last_type()` returns the expected type of the last object to fail a
type check in
[`obj_assert_type()`](https://ethansansom.github.io/type/reference/obj-type.md)
or [`typed()`](https://ethansansom.github.io/type/reference/typed.md).

## Usage

``` r
last_type()
```

## Value

The last expected type. If no type assertions have been run, returns
`NULL`.

## Examples

``` r
# Returns `NULL` if no type checks have been run
last_type()
#> <type>
#> • `<object>` is a bare <logical>.
#> • `<object>` is size 1.
#> • `<object>` contains no missing values.

# `last_type()` returns `t_bool` after failed assertion
if (FALSE) { # \dontrun{
obj_assert_type(10L, t_bool)
last_type()
} # }
```
