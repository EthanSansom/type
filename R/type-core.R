#nocov start

#' Built-in types
#'
#' @description
#'
#' The `t_*` types represent common objects in R, such as functions
#' and atomic vectors. They may be refined using traits, such as
#' [complete()] or [bounded()], to add additional restrictions to the
#' type.
#' 
#' Each `t_*` type is an S7 object containing a list of traits. `t_bool`
#' for example is constructed from [bare_typed()], [sized()], and
#' [complete()]:
#' 
#' ```r
#' t_bool <- bare_typed("logical") |> sized(1L) |> complete()
#' ```
#'
#' The following base types are provided:
#'
#' | Object      | Matches                                          |
#' |-------------|--------------------------------------------------|
#' | `t_any`     | Any object                                       |
#' | `t_null`    | `NULL`                                           |
#' | `t_list`    | A list                                           |
#' | `t_env`     | An environment                                   |
#' | `t_fun`     | A function                                       |
#' | `t_vec`     | A [vctrs][vctrs::vctrs-package]-style vector     |
#' | `t_num`     | A numeric (e.g. integer or double) vector       |
#' | `t_lgl`     | A bare logical vector                            |
#' | `t_bool`    | A single `TRUE` or `FALSE`                       |
#' | `t_int`     | A bare integer vector                            |
#' | `t_dbl`     | A bare double vector                             |
#' | `t_chr`     | A bare character vector                          |
#' | `t_string`  | A single non-`NA` string                         |
#' | `t_dataframe` | A data frame                                   |
#' | `t_factor`  | A factor                                         |
#' | `t_date`    | A `Date`                                         |
#' | `t_posixct` | A `POSIXct` datetime                             |
#' | `t_dots`    | A `...` argument                                 |
#'
#' @seealso [typed()] for declaring typed functions.
#' 
#' @format NULL
#' 
#' @examples
#' obj_is_type(1L, t_int)
#' obj_is_type(1.5, t_int)
#'
#' obj_is_type(TRUE, t_bool)
#' obj_is_type(NA, t_bool)
#' obj_is_type(c(TRUE, FALSE), t_bool)
#'
#' obj_is_type("hello", t_string)
#' obj_is_type(NA_character_, t_string)
#'
#' # Add traits to enforce additional restrictions
#' t_prob <- t_dbl |> bounded(0, 1)
#' t_name <- t_string |> within(c("x", "y", "z"))
#'
#' @name base-types
#' @aliases t_any t_null t_list t_env t_fun t_vec t_num t_lgl t_bool t_int t_dbl t_chr t_string t_dataframe t_factor t_date t_posixct t_dots
NULL

#' @rdname base-types
#' @export
t_any <- NULL

#' @rdname base-types
#' @export
t_null <- NULL

#' @rdname base-types
#' @export
t_list <- NULL

#' @rdname base-types
#' @export
t_env <- NULL

#' @rdname base-types
#' @export
t_fun <- NULL

#' @rdname base-types
#' @export
t_vec <- NULL

#' @rdname base-types
#' @export
t_num <- NULL

#' @rdname base-types
#' @export
t_lgl <- NULL

#' @rdname base-types
#' @export
t_bool <- NULL

#' @rdname base-types
#' @export
t_int <- NULL

#' @rdname base-types
#' @export
t_dbl <- NULL

#' @rdname base-types
#' @export
t_chr <- NULL

#' @rdname base-types
#' @export
t_string <- NULL

#' @rdname base-types
#' @export
t_dataframe <- NULL

#' @rdname base-types
#' @export
t_factor <- NULL

#' @rdname base-types
#' @export
t_date <- NULL

#' @rdname base-types
#' @export
t_posixct <- NULL

#' @rdname base-types
#' @export
t_dots <- NULL

on_load_core_types <- function() {
  t_any <<- type()

  t_null <<- t_any |> bare_typed("NULL")

  t_list <<- t_any |> bare_typed("list")

  t_env <<- t_any |> bare_typed("environment")

  t_fun <<- t_any |> add_trait(function_trait())

  t_vec <<- t_any |> add_trait(vector_trait())

  t_num <<- t_any |> add_trait(numeric_trait())

  t_lgl <<- t_any |> bare_typed("logical")

  t_bool <<- t_lgl |> sized(1L) |> complete()

  t_int <<- t_any |> bare_typed("integer")

  t_dbl <<- t_any |> bare_typed("double")

  t_chr <<- t_any |> bare_typed("character")

  t_string <<- t_chr |> sized(1L) |> complete()

  t_dataframe <<- t_any |> classed("data.frame")

  t_factor <<- t_any |> classed("factor")

  t_date <<- t_any |> classed("Date")

  t_posixct <<- t_any |> classed("POSIXct")

  t_dots <<- t_any |> endotted()
}

#nocov end
