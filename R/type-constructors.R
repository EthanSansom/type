#' Type constructors for lists and data frames
#'
#' @description
#'
#' These functions construct types for common R container objects:
#'
#' - `list_type()` creates a type for a named list with specific element types.
#' - `list_of_type()` creates a type for a list where every element shares the same type.
#' - `dataframe_type()` creates a type for a data frame with specific column types.
#'
#' ```r
#' # A list with an integer "id" and a string "label"
#' t_record <- list_type(id = t_int, label = t_string)
#'
#' # A list where every element is a double vector
#' t_dbl_list <- list_of_type(t_dbl)
#'
#' # A data frame with columns "x" (double) and "y" (character)
#' t_df <- dataframe_type(x = t_dbl, y = t_chr)
#' ```
#'
#' @param ...
#'
#' For `list_type()` and `dataframe_type()`, named types for each
#' element or column in the object. Dots must be uniquely named and
#' at least one dot must be supplied.
#'
#' @param type
#'
#' For `list_of_type()`, a type that every element of the list must satisfy.
#'
#' @returns A type.
#'
#' @examples
#' t_person <- list_type(name = t_string, age = t_int |> bounded(0L))
#'
#' good <- list(name = "Alice", age = 30L)
#' bad <- list(name = "Bob", age = -1L)
#' obj_is_type(good, t_person)
#' obj_inspect_type(bad, t_person)
#'
#' t_scores <- list_of_type(t_dbl |> bounded(0, 100))
#' good_scores <- list(82.5, 91.0, 74.3)
#' bad_scores <- list(82.5, 110.0)
#' obj_is_type(good_scores, t_scores)
#' obj_inspect_type(bad_scores, t_scores)
#'
#' t_coords <- dataframe_type(
#'   lat = t_dbl |> bounded(-90, 90),
#'   lon = t_dbl |> bounded(-180, 180)
#' )
#' good_df <- data.frame(lat = c(51.5, 40.7), lon = c(-0.1, -74.0))
#' bad_df <- data.frame(lat = c(51.5, 200.0))
#' obj_is_type(good_df, t_coords)
#' obj_inspect_type(bad_df, t_coords)
#'
#' @name type-constructors
NULL

#' @rdname type-constructors
#' @export
list_type <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    abort_bad_input("At least one {.arg ...} must be supplied.")
  } 
  assert_named(dots, unique = TRUE, x_name = "...")
  
  dots_names <- names(dots)
  type <- t_list |>
    has(on(names), t_chr |> same_as(dots_names))

  for (i in seq_along(dots)) {
    assert_is_type(dots[[i]], x_name = paste0("..", i))
    type <- type |> has(on_elm(dots_names[[i]]), dots[[i]])
  }
  type
}

#' @rdname type-constructors
#' @export
list_of_type <- function(type) {
  assert_is_type(type)
  t_list |> has(on_each(), type)
}

#' @rdname type-constructors
#' @export
dataframe_type <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    abort_bad_input("At least one {.arg ...} must be supplied.")
  } 
  assert_named(dots, unique = TRUE, x_name = "...")
  
  dots_names <- names(dots)
  type <- t_any |> 
    classed("data.frame") |>
    has(on(names), t_chr |> same_as(dots_names))

  for (i in seq_along(dots)) {
    assert_is_type(dots[[i]], x_name = paste0("..", i))
    type <- type |> has(on_elm(dots_names[[i]]), dots[[i]])
  }
  type
}
