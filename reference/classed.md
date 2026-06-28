# Check `class()`

`classed()` returns a copy of `type` that requires objects to inherit
from one or more classes. The `inherits` argument controls whether the
object must inherit from all supplied classes or at least one:

    # Must inherit from "Date"
    t_date <- t_any |> classed("Date")

    # Must inherit from both "POSIXct" and "POSIXt"
    t_posixct <- t_any |> classed(c("POSIXct", "POSIXt"), inherits = "all")

    # Must inherit from "Date" or "POSIXct"
    t_datetime <- t_any |> classed(c("Date", "POSIXct"), inherits = "any")

## Usage

``` r
classed(type, classes, inherits = "all")
```

## Arguments

- type:

  A type.

- classes:

  A character vector of class names.

- inherits:

  Whether the object must inherit from `"all"` supplied classes or at
  least `"any"` one. Defaults to `"all"`.

## Value

A copy of `type` with an additional class constraint.

## See also

[`bare_typed()`](https://ethansansom.github.io/type/reference/bare_typed.md)
to constrain [`typeof()`](https://rdrr.io/r/base/typeof.html) rather
than [`class()`](https://rdrr.io/r/base/class.html).

## Examples

``` r
t_posixct <- t_any |> classed(c("POSIXct", "POSIXt"), inherits = "all")
obj_is_type(as.POSIXct("2020-01-01"), t_posixct)
#> [1] TRUE
obj_is_type(as.Date("2020-01-01"), t_posixct)
#> [1] FALSE
```
