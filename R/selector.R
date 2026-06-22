# named selectors --------------------------------------------------------------

# TODO: Document
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

# TODO: Document
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

# TODO: Document
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

# TODO: Document
#' @export
on_elms <- function(indices) {
  context_assert(
    "relation",
    format_styled("Must be used while defining a relation, e.g. in {.fn same_sized}.")
  )
  assert_is_index(indices, scalar = FALSE)
  map(indices, on_elm_impl)
}

# TODO: Document
#' @export
on_attrs <- function(attrs) {
  context_assert(
    "relation",
    format_styled("Must be used while defining a relation, e.g. in {.fn same_sized}.")
  )
  assert_is_chr(attrs, complete = TRUE)
  map(attrs, on_attr_impl)
}

# on ---------------------------------------------------------------------------

# TODO: Document
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
flatten_selectors <- function(dots, error_call = caller_env()) {
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
