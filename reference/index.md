# Package index

## Object and Function Typing

- [`typed()`](https://ethansansom.github.io/type/reference/typed.md) :
  Declare a type-checked function
- [`untyped()`](https://ethansansom.github.io/type/reference/untyped.md)
  : Remove argument and return type validation from a typed function
- [`` `%:%` ``](https://ethansansom.github.io/type/reference/grapes-colon-grapes.md)
  : Declare a typed object
- [`const()`](https://ethansansom.github.io/type/reference/modifiers.md)
  [`optional()`](https://ethansansom.github.io/type/reference/modifiers.md)
  [`maybe()`](https://ethansansom.github.io/type/reference/modifiers.md)
  : Modify a type
- [`last_type()`](https://ethansansom.github.io/type/reference/last_type.md)
  : Return the expected type of the last mistyped object

## Traits

Traits are functions which add an additional constraint to a type, for
example the expected size of an object or its expected class.

- [`bare_typed()`](https://ethansansom.github.io/type/reference/bare_typed.md)
  :

  Check [`typeof()`](https://rdrr.io/r/base/typeof.html)

- [`classed()`](https://ethansansom.github.io/type/reference/classed.md)
  :

  Check [`class()`](https://rdrr.io/r/base/class.html)

- [`sized()`](https://ethansansom.github.io/type/reference/sized.md) :
  Check size

- [`complete()`](https://ethansansom.github.io/type/reference/complete.md)
  : Check for missing values

- [`unduplicated()`](https://ethansansom.github.io/type/reference/unduplicated.md)
  : Check for duplicate values

- [`bounded()`](https://ethansansom.github.io/type/reference/bounded.md)
  : Check upper and lower bounds

- [`within()`](https://ethansansom.github.io/type/reference/within.md) :
  Check that object is a subset

- [`contains()`](https://ethansansom.github.io/type/reference/contains.md)
  : Check that object is a superset

- [`same_as()`](https://ethansansom.github.io/type/reference/same_as.md)
  : Check that object is identical to a vector

- [`disjoint_to()`](https://ethansansom.github.io/type/reference/disjoint_to.md)
  : Check that object does not contain values

- [`setequal_to()`](https://ethansansom.github.io/type/reference/setequal_to.md)
  : Check that object is a given set

## Element Typing

- [`has()`](https://ethansansom.github.io/type/reference/has.md) : Add
  an element or attribute constraint to a type
- [`on()`](https://ethansansom.github.io/type/reference/on.md)
  [`on_attr()`](https://ethansansom.github.io/type/reference/on.md)
  [`on_elm()`](https://ethansansom.github.io/type/reference/on.md)
  [`on_data()`](https://ethansansom.github.io/type/reference/on.md)
  [`on_each()`](https://ethansansom.github.io/type/reference/on.md)
  [`on_elms()`](https://ethansansom.github.io/type/reference/on.md)
  [`on_attrs()`](https://ethansansom.github.io/type/reference/on.md) :
  Select an element or attribute to type

## Relations

Relations are special traits which define between-element type
constraints, for example that all elements of an object are the same
size. These may also be used to define between-argument constraints in
typed functions.

- [`has_relation()`](https://ethansansom.github.io/type/reference/has_relation.md)
  : Add a between-element constraint to a type
- [`same_classed()`](https://ethansansom.github.io/type/reference/relations.md)
  [`same_sized()`](https://ethansansom.github.io/type/reference/relations.md)
  [`recyclable()`](https://ethansansom.github.io/type/reference/relations.md)
  [`exclusive()`](https://ethansansom.github.io/type/reference/relations.md)
  : Set between-element type constraints

## Type Construction

- [`type_union()`](https://ethansansom.github.io/type/reference/type_union.md)
  : Declare a type union
- [`list_type()`](https://ethansansom.github.io/type/reference/type-constructors.md)
  [`list_of_type()`](https://ethansansom.github.io/type/reference/type-constructors.md)
  [`dataframe_type()`](https://ethansansom.github.io/type/reference/type-constructors.md)
  : Type constructors for lists and data frames
- [`is_type()`](https://ethansansom.github.io/type/reference/type-predicates.md)
  [`is_type_union()`](https://ethansansom.github.io/type/reference/type-predicates.md)
  : Test if an object is a type

## Object Type Validation

- [`obj_is_type()`](https://ethansansom.github.io/type/reference/obj-type.md)
  [`obj_inspect_type()`](https://ethansansom.github.io/type/reference/obj-type.md)
  [`obj_assert_type()`](https://ethansansom.github.io/type/reference/obj-type.md)
  : Check an object against a type

## Base Types

- [`t_any`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_null`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_list`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_env`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_fun`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_vec`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_num`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_lgl`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_bool`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_int`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_dbl`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_chr`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_string`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_dataframe`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_factor`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_date`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_posixct`](https://ethansansom.github.io/type/reference/base-types.md)
  [`t_dots`](https://ethansansom.github.io/type/reference/base-types.md)
  : Built-in types

## Inlined Helpers

These functions are inlined into typed functions and are not meant for
interactive use.

- [`inline_assert_same_classed()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_assert_same_sized()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_assert_recyclable()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_assert_exclusive()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_abort_mistyped()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_obj_type_validate()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_assert_type()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_arg_assert_type()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_dots_assert_type()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  [`inline_dotlist_assert_type()`](https://ethansansom.github.io/type/reference/inlined-functions.md)
  : Inlined helper functions
