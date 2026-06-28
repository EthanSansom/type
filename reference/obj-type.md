# Check an object against a type

These functions are used to investiage whether an object `obj` has a
type `type`.

- `obj_is_type(obj, type)` returns `TRUE` if `obj` has type `type` and
  `FALSE` otherwise.

- `obj_inspect_type(obj, type)` prints a success or failure message for
  each type check run on `obj` then returns `NULL` invisibly.

- `obj_assert_type(obj, type)` raises a `<type_error_mistyped_obj>`
  error if `obj` is mistyped and returns `NULL` invisibly otherwise.

## Usage

``` r
obj_is_type(obj, type)

obj_inspect_type(obj, type, obj_name = rlang::caller_arg(obj))

obj_assert_type(obj, type, obj_name = rlang::caller_arg(obj))
```

## Arguments

- obj:

  An object to check.

- type:

  A type to check against.

- obj_name:

  The name of `obj`, used in messages. Defaults to the expression passed
  to `obj`.

## Value

- `obj_is_type()`: `TRUE` if `obj` is the correct type, `FALSE`
  otherwise.

- `obj_inspect_type()`: `NULL` invisibly, called for its side effect.

- `obj_assert_type()`: `NULL` invisibly if `obj` is the correct type,
  otherwise raises an error.

## Examples

``` r
good <- TRUE
bad <- NA

# Test whether an object is a boolean
obj_is_type(good, t_bool)
#> [1] TRUE
obj_is_type(bad, t_bool)
#> [1] FALSE

# Print the type tests run on the object
obj_inspect_type(good, t_bool)
#> Object `good` has the expected type.
#> ✔ `good` is a bare <logical>.
#> ✔ `good` is size 1.
#> ✔ `good` contains no missing values.
obj_inspect_type(bad, t_bool)
#> Object `bad` does not have the expected type.
#> ✔ `bad` is a bare <logical>.
#> ✔ `bad` is size 1.
#> ℹ `bad` must not contain missing elements.
#> ✖ `bad` is NA at location `1`.

# Raise an error if the object is not a boolean
obj_assert_type(good, t_bool)
try(obj_assert_type(bad, t_bool))
#> Error in obj_assert_type(bad, t_bool) : Object `bad` is mistyped.
#> ℹ `bad` must not contain missing elements.
#> ✖ `bad` is NA at location `1`.
#> ℹ Run `last_type()` to get the expected type.
```
