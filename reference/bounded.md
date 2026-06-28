# Check upper and lower bounds

`bounded()` returns a copy of `type` that requires objects to fall
within a range. Bounds are specified by `left` and `right`, and the
interval type is controlled by `bounds`:

|          |            |                          |
|----------|------------|--------------------------|
| `bounds` | Interval   | Condition                |
| `"[]"`   | Closed     | `left <= x & x <= right` |
| `"[)"`   | Right-open | `left <= x & x < right`  |
| `"(]"`   | Left-open  | `left < x & x <= right`  |
| `"()"`   | Open       | `left < x & x < right`   |

Either `left` or `right` may be omitted to leave that side unbounded:

    t_dbl |> bounded(0, 1)             # probabilities: [0, 1]
    t_dbl |> bounded(0, bounds = "()") # strictly positive
    t_chr |> bounded("m", "p")         # starts with "m", "n", "o", or "p"

`bounded()` ignores `NA` values when checking bounds. To require
non-missingness, combine with the
[`complete()`](https://ethansansom.github.io/type/reference/complete.md)
trait.

\[)"` | Right-open |`left \<= x & x \< right` | |`"(\]:
R:)%22%60%20%20%20%7C%20Right-open%20%20%20%7C%20%60left%20%3C=%20x%20&%20x%20%3C%20right%60%20%20%7C%0A%7C%20%60%22(
\[0, 1\]: R:0,%201

## Usage

``` r
bounded(type, left = NULL, right = NULL, bounds = "[]")
```

## Arguments

- type:

  A type.

- left, right:

  Scalar values giving the left and right bounds. Either may be `NULL`
  (the default) to leave that side unbounded. At least one of `left`,
  `right` must be supplied.

- bounds:

  A string denoting the boundary type: `"[]"` (closed, the default),
  `"[)"`, `"(]"`, or `"()"` (open).

  \[)"`, `"(\]: R:)%22%60,%20%60%22(

## Value

A copy of `type` with an additional bound constraint.

## See also

[`sized()`](https://ethansansom.github.io/type/reference/sized.md) for
size constraints,
[`complete()`](https://ethansansom.github.io/type/reference/complete.md)
for non-missingness.

## Examples

``` r
t_prob <- t_dbl |> bounded(0, 1)
t_positive <- t_dbl |> bounded(0, bounds = "()")

obj_inspect_type(0.5, t_prob)
#> Object `0.5` has the expected type.
#> ✔ `0.5` is a bare <double>.
#> ✔ `0.5` is bounded by [0, 1].
obj_inspect_type(0, t_positive)
#> Object `0` does not have the expected type.
#> ✔ `0` is a bare <double>.
#> ℹ `0` must be above 0.
#> ✖ `0` is out of bounds at location `1`.

# `NA` values are considered within bounds
obj_is_type(c(0, NA, 0.75), t_prob)
#> [1] TRUE
```
