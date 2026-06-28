# on ---------------------------------------------------------------------------

#' Select an element or attribute to type
#'
#' @description
#' 
#' Selectors identify a part of an object for use in [has()] and relation
#' functions such as [same_sized()] and [same_classed()]. 
#' 
#' These are useful for typing elements of a container type, such as a list. For
#' example, the following defines a coordinate type `t_coords`, which is a list
#' containing named numeric elements `"lat"` and `"lon"` of the same size:
#'
#' ```r
#' t_coords <- t_list |>
#'   has(on_elm("lat"), t_dbl |> bounded(-90, 90)) |>
#'   has(on_elm("lon"), t_dbl |> bounded(-180, 180)) |>
#'   has_relation(same_sized(on_elm("lat"), on_elm("lon")))
#' ```
#'
#' `on()` specifies an `accessor` function, either a call or a function name, 
#' applied to an object during type checking. If `accessor` is a call, `.x`
#' may be used as a placeholder for the object.
#'
#' ```r
#' # Require an object's length to be between 1 and 10 (inclusive)
#' t_any |> has(on(length(.x)), t_int |> bounded(1L, 10L))
#' 
#' # Require an object's names to contain "x" and "y"
#' # Equivilant to `on(names(.x))`
#' t_any |> has(on(names), t_chr |> contains(c("x", "y")))
#' ```
#' 
#' `on_elm(index)` and `on_attr(name)` are convenience selectors for the
#' common cases of `on(.x[[index]])` and `on(attr(.x, name))`.
#'
#' ```r
#' has(t_any, on_elm(1L), t_int)     # same as `on(.x[[1L]])`
#' has(t_any, on_attr("dim"), t_int) # same as `on(attr(.x, "dim"))`
#' ```
#' 
#' `on_elms(indices)` and `on_attrs(names)` are the multiple selector forms
#' of `on_elm()` and `on_attr()`, useful for selecting multiple elements
#' in a relation:
#' 
#' ```r
#' # These are equivilant
#' t_any |> has_relation(same_sized(on_elms(c(1L, 2L))))
#' t_any |> has_relation(same_sized(on_elm(1L), on_elm(2L)))
#' ```
#' 
#' `on_data()` selects the underlying data of an object after it's attributes 
#' and class has been removed (via [unclass()]).
#' 
#' ```r
#' # POSIXct datetime vectors store time as a double
#' t_posixct <- t_any |>
#'   classed("POSIXct") |>
#'   has(on_data(), t_dbl) |>
#'   has(on_attr("tzone"), t_chr |> sized(1L))
#' ```
#'
#' `on_each()` selects all elements of an object, applying the type check to
#' each one individually.
#' 
#' ```r
#' # Defines a list-of-integers type
#' t_list_of_int <- t_list |> has(on_each(), t_int)
#' ```
#'
#' @param accessor 
#' 
#' A call using `.x` as the object placeholder, or a bare symbol `f` as 
#' shorthand for `f(.x)`.
#' 
#' @param index 
#' 
#' For `on_elm()`, a single position (integer) or name (character).
#' 
#' @param indices 
#' 
#' For `on_elms()`, positions (integer) or names (character).
#'
#' @param name
#' 
#' For `on_attr()`, a single attribute name (character).
#'
#' @param attrs 
#' 
#' For `on_attrs()`, attribute names (character).
#' 
#' @returns
#' 
#' A selector. These functions may only be used within [has()] and relations
#' such as [same_sized()] and [same_classed()]. Outside of these contexts, the
#' `on()` functions raise an error. 
#'
#' @seealso [has()] to attach a selector to a type, [has_relation()] to attatch a relation to a type.
#' 
#' @examples
#' t_coords <- t_list |>
#'   has(on(names), t_chr |> setequal_to(c("lat", "lon"))) |>
#'   has(on_elm("lat"), t_dbl |> bounded(-90, 90)) |>
#'   has(on_elm("lon"), t_dbl |> bounded(-180, 180)) |>
#'   has_relation(same_sized(on_elms(c("lat", "lon"))))
#' 
#' good <- list(lat = c(70, 20, -50), lon = c(100, 0, 85))
#' obj_is_type(good, t_coords)
#' 
#' bad <- list(lat = c(1, 7), lon = 90)
#' obj_inspect_type(bad, t_coords)
#'
#' @export
on <- function(accessor) {
  selector_context_assert()
  parent_frame <- rlang::caller_env()
  accessor_expr <- rlang::enexpr(accessor)
  if (!rlang::is_call_simple(accessor_expr) && !rlang::is_symbol(accessor_expr)) {
    abort_bad_input(
      format_styled("{.arg accessor} must be a simple call or a symbol."),
      error_call = parent_frame
    )
  }
  if (rlang::is_symbol(accessor_expr) && !identical(accessor_expr, quote(.x))) {
    accessor_expr <- rlang::call2(accessor_expr, quote(.x))
  }

  accessor_label <- rlang::as_label(accessor_expr)
  accessor <- rlang::new_function(
    args = rlang::pairlist2(.x = ),
    body = accessor_expr,
    env = parent_frame
  )

  selector(
    accessor = accessor,
    labeller = \(obj_name, value) {
      backtick(gsub(".x", obj_name, accessor_label, fixed = TRUE))
    },
    plural = FALSE
  )
}

# named selectors --------------------------------------------------------------

#' @rdname on
#' @export
on_attr <- function(name) {
  selector_context_assert()
  assert_is_string(name)
  on_attr_impl(name)
}

on_attr_impl <- function(name) {
  force(name)
  selector(
    accessor = \(obj) attr(obj, name, exact = TRUE),
    labeller = \(obj_name, obj) backtick(glue::glue("attr({obj_name}, {chr_encode(name)})")),
    plural = FALSE
  )
}

#' @rdname on
#' @export
on_elm <- function(index) {
  selector_context_assert()
  assert_is_index(index)
  on_elm_impl(index)
}

on_elm_impl <- function(index) {
  force(index)
  selector(
    accessor = \(obj) obj[[index]],
    labeller = \(obj_name, obj) {
      location <- if (is.character(index)) chr_encode(index) else index
      backtick(glue::glue("{obj_name}[[{location}]]"))
    },
    plural = FALSE
  )
}

#' @rdname on
#' @export
on_data <- function() {
  selector_context_assert()
  selector(
    accessor = \(obj) {
      attributes(obj) <- NULL
      unclass(obj)
    },
    labeller = \(obj_name, obj) backtick(glue::glue("unclass({obj_name})")),
    plural = FALSE
  )
}

#' @rdname on
#' @export
on_each <- function() {
  selector_context_assert()
  selector(
    accessor = \(obj) as.list(obj),
    labeller = \(obj_name, obj) {
      nms <- rlang::names2(as.list(obj))
      idx <- seq_along(nms)
      backtick(ifelse(
        nms == "",
        glue::glue("{obj_name}[[{idx}]]"),
        glue::glue("{obj_name}[[{chr_encode(nms)}]]")
      ))
    },
    describer = \(obj_name, obj) format_styled("each element of {.arg {obj_name}}"),
    plural = TRUE
  )
}

# multiple selectors -----------------------------------------------------------

#' @rdname on
#' @export
on_elms <- function(indices) {
  context_assert(
    "relation",
    format_styled("Must be used while defining a relation, e.g. in {.fn same_sized}.")
  )
  assert_is_index(indices, scalar = FALSE)
  map(indices, on_elm_impl)
}

#' @rdname on
#' @export
on_attrs <- function(attrs) {
  context_assert(
    "relation",
    format_styled("Must be used while defining a relation, e.g. in {.fn same_sized}.")
  )
  assert_is_chr(attrs, complete = TRUE)
  map(attrs, on_attr_impl)
}

# selector class ---------------------------------------------------------------

selector <- S7::new_class(
  "selector",
  package = "type",
  properties = list(
    accessor = S7::class_function,
    labeller = S7::class_function,
    describer = S7::class_any,
    plural = S7::class_logical
  ),
  constructor = function(accessor, labeller, describer = NULL, plural = FALSE) {
    S7::new_object(
      S7::S7_object(),
      accessor = accessor,
      labeller = labeller,
      describer = describer %||% labeller,
      plural = plural
    )
  }
)

is_selector <- function(x) {
  S7::S7_inherits(x, selector)
}

assert_is_selector <- function(x, x_name = rlang::caller_arg(x), error_call = rlang::caller_env()) {
  if (is_selector(x)) {
    return(invisible())
  }
  abort_bad_input(
    message = format_styled(
      "{.arg {x_name}} must be a selector, e.g. the result of {.fn on} or {.fn on_attr}."
    ),
    error_call = error_call
  )
}

# helpers ----------------------------------------------------------------------

selector_context_assert <- function(error_call = rlang::caller_env()) {
  context_assert(
    c("has", "relation"),
    format_styled(
      "Must be used while defining a relation or selector, ",
      "e.g. in {.fn same_sized} or {.fn has}."
    ),
    error_call = error_call
  )
}

# For use in relations, e.g. `same_sized()`
flatten_selectors <- function(dots, error_call = rlang::caller_env()) {
  out <- list()
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    if (!(is_selector(dot) || (is.list(dot) && all(map_lgl(dot, is_selector))))) {
      abort_bad_input(
        format_styled(
          "..{i} must be a selector or list of selectors, ",
          "e.g. the result of {.fn on_elm} or {.fn on_elms}."
        ),
        error_call = error_call
      )
    }
    out <- c(out, if (is_selector(dot)) list(dot) else dot)
  }
  out
}
