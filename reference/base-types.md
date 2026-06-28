# Built-in types

The `t_*` types represent common objects in R, such as functions and
atomic vectors. They may be refined using traits, such as
[`complete()`](https://ethansansom.github.io/type/reference/complete.md)
or
[`bounded()`](https://ethansansom.github.io/type/reference/bounded.md),
to add additional restrictions to the type.

Each `t_*` type is an S7 object containing a list of traits. `t_bool`
for example is constructed from
[`bare_typed()`](https://ethansansom.github.io/type/reference/bare_typed.md),
[`sized()`](https://ethansansom.github.io/type/reference/sized.md), and
[`complete()`](https://ethansansom.github.io/type/reference/complete.md):

    t_bool <- bare_typed("logical") |> sized(1L) |> complete()

The following base types are provided:

|  |  |
|----|----|
| Object | Matches |
| `t_any` | Any object |
| `t_null` | `NULL` |
| `t_list` | A list |
| `t_env` | An environment |
| `t_fun` | A function |
| `t_vec` | A [vctrs](https://vctrs.r-lib.org/reference/vctrs-package.html)-style vector |
| `t_num` | A numeric (e.g. integer or double) vector |
| `t_lgl` | A bare logical vector |
| `t_bool` | A single `TRUE` or `FALSE` |
| `t_int` | A bare integer vector |
| `t_dbl` | A bare double vector |
| `t_chr` | A bare character vector |
| `t_string` | A single non-`NA` string |
| `t_dataframe` | A data frame |
| `t_factor` | A factor |
| `t_date` | A `Date` |
| `t_posixct` | A `POSIXct` datetime |
| `t_dots` | A `...` argument |

## Usage

``` r
t_any

t_null

t_list

t_env

t_fun

t_vec

t_num

t_lgl

t_bool

t_int

t_dbl

t_chr

t_string

t_dataframe

t_factor

t_date

t_posixct

t_dots
```

## See also

[`typed()`](https://ethansansom.github.io/type/reference/typed.md) for
declaring typed functions.

## Examples

``` r
obj_is_type(1L, t_int)
#> [1] TRUE
obj_is_type(1.5, t_int)
#> [1] FALSE

obj_is_type(TRUE, t_bool)
#> [1] TRUE
obj_is_type(NA, t_bool)
#> [1] FALSE
obj_is_type(c(TRUE, FALSE), t_bool)
#> [1] FALSE

obj_is_type("hello", t_string)
#> [1] TRUE
obj_is_type(NA_character_, t_string)
#> [1] FALSE

# Add traits to enforce additional restrictions
t_prob <- t_dbl |> bounded(0, 1)
t_name <- t_string |> within(c("x", "y", "z"))
```
