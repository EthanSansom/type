# Check `typeof()`

`bare_typed()` returns a copy of `type` that requires objects to be a
bare R value with [`typeof()`](https://rdrr.io/r/base/typeof.html) equal
to `typeof`. Bare objects are those with no class attribute, for example
`1L` or [`list()`](https://rdrr.io/r/base/list.html), but not
[`data.frame()`](https://rdrr.io/r/base/data.frame.html).

    t_lgl <- t_any |> bare_typed("logical")
    obj_is_type(1, t_lgl)  # FALSE
    obj_is_type(NA, t_lgl) # TRUE

## Usage

``` r
bare_typed(type, typeof)
```

## Arguments

- type:

  A type.

- typeof:

  A string. Must be a one of the valid R types returned by
  [`typeof()`](https://rdrr.io/r/base/typeof.html): `"double"`,
  `"integer"`, `"logical"`, `"character"`, `"complex"`, `"raw"`,
  `"list"`, `"NULL"`, `"environment"`, `"symbol"`, `"pairlist"`,
  `"language"`, `"expression"`, `"S4"`, `"closure"`, `"special"`,
  `"builtin"`, `"externalptr"`, `"weakref"`, `"promise"`, `"char"`, or
  `"bytecode"`.

## Value

A copy of `type` with an additional `typeof` constraint.

## Examples

``` r
t_pairlist <- t_any |> bare_typed("pairlist")
obj_inspect_type(10L, t_pairlist)
#> Object `10L` does not have the expected type.
#> ✖ `10L` must be a bare <pairlist>, not a bare <integer>.
obj_inspect_type(pairlist(), t_pairlist)
#> Object `pairlist()` has the expected type.
#> ✔ `pairlist()` is a bare <pairlist>.
```
