# Type constructors for lists and data frames

These functions construct types for common R container objects:

- `list_type()` creates a type for a named list with specific element
  types.

- `list_of_type()` creates a type for a list where every element shares
  the same type.

- `dataframe_type()` creates a type for a data frame with specific
  column types.

    # A list with an integer "id" and a string "label"
    t_record <- list_type(id = t_int, label = t_string)

    # A list where every element is a double vector
    t_dbl_list <- list_of_type(t_dbl)

    # A data frame with columns "x" (double) and "y" (character)
    t_df <- dataframe_type(x = t_dbl, y = t_chr)

## Usage

``` r
list_type(...)

list_of_type(type)

dataframe_type(...)
```

## Arguments

- ...:

  For `list_type()` and `dataframe_type()`, named types for each element
  or column in the object. Dots must be uniquely named and at least one
  dot must be supplied.

- type:

  For `list_of_type()`, a type that every element of the list must
  satisfy.

## Value

A type.

## Examples

``` r
t_person <- list_type(name = t_string, age = t_int |> bounded(0L))

good <- list(name = "Alice", age = 30L)
bad <- list(name = "Bob", age = -1L)
obj_is_type(good, t_person)
#> [1] TRUE
obj_inspect_type(bad, t_person)
#> Object `bad` does not have the expected type.
#> ✔ `bad` is a bare <list>.
#> ✔ `names(bad)` is a bare <character>.
#> ✔ `names(bad)` is the same as: `c("name", "age")`.
#> ✔ `bad[["name"]]` is a bare <character>.
#> ✔ `bad[["name"]]` is size 1.
#> ✔ `bad[["name"]]` contains no missing values.
#> ℹ `bad[["age"]]` must be equal to or above 0.
#> ✖ `bad[["age"]]` is out of bounds at location `1`.

t_scores <- list_of_type(t_dbl |> bounded(0, 100))
good_scores <- list(82.5, 91.0, 74.3)
bad_scores <- list(82.5, 110.0)
obj_is_type(good_scores, t_scores)
#> [1] TRUE
obj_inspect_type(bad_scores, t_scores)
#> Object `bad_scores` does not have the expected type.
#> ✔ `bad_scores` is a bare <list>.
#> ℹ `bad_scores[[2]]` must be bounded by [0, 100].
#> ✖ `bad_scores[[2]]` is out of bounds at location `1`.

t_coords <- dataframe_type(
  lat = t_dbl |> bounded(-90, 90),
  lon = t_dbl |> bounded(-180, 180)
)
good_df <- data.frame(lat = c(51.5, 40.7), lon = c(-0.1, -74.0))
bad_df <- data.frame(lat = c(51.5, 200.0))
obj_is_type(good_df, t_coords)
#> [1] TRUE
obj_inspect_type(bad_df, t_coords)
#> Object `bad_df` does not have the expected type.
#> ✔ `bad_df` inherits from class `data.frame`.
#> ℹ `names(bad_df)` must be: `c("lat", "lon")`.
#> ✖ `names(bad_df)` is missing 1 element: `"lon"`.
```
