# has --------------------------------------------------------------------------

#' Add an element or attribute constraint to a type
#'
#' @description
#'
#' `has()` returns a copy of `type` that requires a selected part of an object 
#' to have type `on_type`. The part is identified by a selector (see [on()]).
#'
#' ```r
#' # Require that element `[[1]]` is an integer
#' t_any |> has(on_elm(1L), t_int)
#'
#' # Require that the "dim" attribute is an integer vector of size 2
#' t_any |> has(on_attr("dim"), t_int |> sized(2L))
#' 
#' # Require that names contains "x"
#' t_any |> has(on(names), t_chr |> contains("x"))
#' ```
#'
#' Constraints can be composed with the pipe operator `|>`:
#'
#' ```r
#' t_coords <- t_list |>
#'   has(on_elm("lat"), t_dbl |> bounded(-90, 90)) |>
#'   has(on_elm("lon"), t_dbl |> bounded(-180, 180))
#' ```
#'
#' @param type 
#' 
#' A type.
#' 
#' @param selector 
#' 
#' A selector, e.g. the result of [on()], [on_elm()], or [on_attr()].
#' 
#' @param on_type 
#' 
#' A type to check the selected value against.
#'
#' @returns A copy of `type` with an additional element or attributeconstraint.
#'
#' @seealso [on()] for available selectors, [has_relation()] to add between-element constraints.
#'
#' @examples
#' # TODO
#' 
#' @export
has <- function(type, selector, on_type) {
  context_local("has")
  assert_is_type(type)
  assert_is_selector(selector)
  assert_is_type(on_type)
  type |> add_trait(has_on_trait(selector = selector, on_type = on_type))
}

has_on_trait <- new_trait("has_on", params = c("selector", "on_type"))

method(trait_test, has_on_trait) <- function(trait, obj) {
  selector <- trait@selector
  on_type <- trait@on_type
  value <- rlang::try_fetch(selector@accessor(obj), error = identity)
  if (rlang::is_error(value)) return(FALSE)

  if (selector@plural) {
    if (length(value) == 0L) return(TRUE)
    return(all(map_lgl(value, \(v) type_test(on_type, v))))
  }
  type_test(on_type, value)
}

method(trait_diagnose, has_on_trait) <- function(trait, obj, obj_name) {
  selector <- trait@selector
  on_type <- trait@on_type
  value <- rlang::try_fetch(selector@accessor(obj), error = identity)

  if (rlang::is_error(value)) {
    label <- selector@labeller(obj_name, obj)
    return(c(
      x = format_styled("{label} must return a value, not raise an error.")
    ))
  }

  if (selector@plural) {
    labels <- selector@labeller(obj_name, obj)
    for (i in seq_along(value)) {
      if (!type_test(on_type, value[[i]])) {
        return(type_diagnose(on_type, value[[i]], untick(labels[[i]])))
      }
    }
  } else {
    type_diagnose(on_type, value, untick(selector@labeller(obj_name, obj)))
  }
}

method(trait_describe, has_on_trait) <- function(trait, obj_name) {
  selector <- trait@selector
  on_type <- trait@on_type

  if (selector@plural) {
    return(paste("Every element of", str_lower1(type_describe(trait@on_type, obj_name))))
  }
  type_describe(on_type, untick(selector@labeller(obj_name, NULL)))
}
