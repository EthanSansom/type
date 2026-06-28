# Inlined helper functions

These functions are inserted into
[`typed()`](https://ethansansom.github.io/type/reference/typed.md)
functions and are not meant for external use. They are exported only to
ensure that
[`typed()`](https://ethansansom.github.io/type/reference/typed.md)
functions call the correct helper, e.g. `type::inline_assert_type()`,
and not a globally defined function, e.g. `inline_assert_type()`.

## Usage

``` r
inline_assert_same_classed(..., error_call = rlang::caller_env())

inline_assert_same_sized(..., error_call = rlang::caller_env())

inline_assert_recyclable(..., error_call = rlang::caller_env())

inline_assert_exclusive(..., error_call = rlang::caller_env())

inline_abort_mistyped(
  type,
  message,
  error_subclass = character(),
  error_call = rlang::caller_env()
)

inline_obj_type_validate(obj, obj_name, type)

inline_assert_type(
  obj,
  obj_name,
  type,
  error_header,
  error_subclass = character(),
  error_call = rlang::caller_env()
)

inline_arg_assert_type(arg, type, arg_name, error_call = rlang::caller_env())

inline_dots_assert_type(..., .type, .error_call = rlang::caller_env())

inline_dotlist_assert_type(dots, type, error_call = rlang::caller_env())
```

## Arguments

- ...:

  Dots to be checked individually.

- error_call, .error_call:

  The call to use in error messages.

- type, .type:

  A type.

- message:

  An error message.

- error_subclass:

  An error class.

- obj:

  An object to be checked.

- obj_name:

  An object name to use in error messages.

- error_header:

  An error message header.

- arg:

  An argument to be checked.

- arg_name:

  An argument name to use in error messages.

- dots:

  A list of dots to be checked.

## Examples

``` r
try(inline_dots_assert_type(10L, t_chr))
#> Error in inline_dots_assert_type(10L, t_chr) : 
#>   argument ".type" is missing, with no default
```
