# todos ------------------------------------------------------------------------

# TODO: last_type()
# - Cache the last failed type on a failure, so user can run `last_type()` to see it.

# operators --------------------------------------------------------------------

# TODO: Document
#' @export
`%:%` <- function(type, declaration) {
  context_local("%:%")
  if (!is_type(type)) {
    abort_bad_input(format_styled(
      "The left-hand side of {.code %:%} must be a type, ",
      "not <<fmt_r_type(type)>>."
    ))
  }
  parent_frame <- rlang::caller_env()

  decl_expr <- rlang::enexpr(declaration)
  declared <- parse_declaration(decl_expr, parent_frame)
  name <- declared$name
  value <- declared$value

  if (!type_test(type, value)) {
    value_label <- rlang::as_label(decl_expr[[2]])
    rlang::abort(
      c(
        format_styled(
          "Attempted to initialize {.code {name}} with a mistyped value: {.code {value_label}}."
        ),
        type_diagnose(type, value, "<result>")
      ),
      class = c("type_error_mistyped_obj", "type_error_mistyped", "type_error"),
      call = parent_frame
    )
  }

  if ("const" %in%  type@modifications) {
    abort_call <- 
    binding_fun <- eval(rlang::expr(
      local(
        {
          VALUE <- !!value
          NAME <- !!name
          out <- function(x) {
            if (missing(x)) {
              return(VALUE)
            }
            rlang::abort(
              glue::glue("Can't assign to the constant `{NAME}`."),
              class = c("type_error_mistyped_obj", "type_error_mistyped", "type_error"),
              call = !!parent_frame
            )
          }
          class(out) <- c(
            "type_typed_const_binding",
            "type_typed_object_binding",
            "function"
          )
          attr(out, "bound_name") <- !!name
          attr(out, "bound_type") <- !!type
          out
        },
        parent_frame
      )
    ))
  } else {
    validate_expr <- rlang::call2(
      "inline_obj_type_validate",
      obj = rlang::sym("x"),
      obj_name = "<value>",
      type = type,
      .ns = "type"
    )
    binding_fun <- eval(rlang::expr(
      local(
        {
          VALUE <- !!value
          NAME <- !!name
          out <- function(x) {
            if (missing(x)) {
              return(VALUE)
            }
            validation_result <- !!validate_expr
            if (is.null(validation_result)) {
              VALUE <<- x
              return(VALUE)
            }
            rlang::abort(
              c(
                glue::glue("Attempted to assign a mistyped value to `{NAME}`."),
                validation_result
              ),
              error_call = !!parent_frame
            )
          }
          class(out) <- c("type_typed_object_binding", "function")
          attr(out, "bound_name") <- !!name
          attr(out, "bound_type") <- !!type
          out
        },
        parent_frame
      )
    ))
  }
  binding_fun <- rlang::zap_srcref(binding_fun)

  if (rlang::env_has(parent_frame, name)) {
    rlang::env_unbind(parent_frame, name)
  }
  makeActiveBinding(name, binding_fun, parent_frame)

  return(invisible(value))
}

# inlined ----------------------------------------------------------------------

# TODO: Document
#' @export
inline_obj_type_validate <- function(obj, obj_name, type) {
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      return(trait_diagnose(trait, obj, obj_name))
    }
  }
}

# helpers ----------------------------------------------------------------------

parse_declaration <- function(
  decl_expr,
  parent_frame,
  error_call = rlang::caller_env()
) {
  if (rlang::is_call_simple(decl_expr, ns = FALSE) && length(decl_expr) == 2L) {
    name <- rlang::as_name(decl_expr[[1]])
    value <- rlang::try_fetch(
      eval(decl_expr[[2]], parent_frame),
      error = function(e) {
        init <- rlang::as_label(decl_expr[[2]])
        abort_bad_input(
          message = format_styled(
            "Can't evaluate the initial value {.code {init}} of {.arg {name}}."
          ),
          error_call = error_call,
          parent = e
        )
      }
    )
    return(list(name = name, value = value))
  }

  expr_label <- rlang::as_label(decl_expr)
  if (rlang::is_symbol(decl_expr)) {
    abort_bad_input(
      message = c(
        format_styled(
          "The right-hand-side of {.code %:%} must be a declaration, not the symbol {.code {expr_label}}."
        ),
        i = format_styled(
          "Use {.code <<expr_label>>(value)} to declare the value of {.code {expr_label}}."
        )
      ),
      error_call = error_call
    )
  }

  # From this point onward, we're dealing with an invalid call
  if (!rlang::is_call_simple(decl_expr) || rlang::is_call_simple(decl_expr, ns = TRUE)) {
    is_namespaced <- rlang::is_call(decl_expr, c("::", ":::"))
    kind <- if (is_namespaced) "namespaced call" else "complex call"

    abort_bad_input(
      message = c(
        format_styled(
          "The right-hand side of {.code %:%} must be a declaration ",
          "of the form {.code name(value)}."
        ),
        x = format_styled("A <<kind>> {.code {expr_label}} was supplied.")
      ),
      error_call = error_call
    )
  }

  # From this point onward, we're dealing with an invalid simple call `foo(...)`
  if (length(decl_expr) == 1L) {
    name <- rlang::as_name(decl_expr[[1]])
    abort_bad_input(
      message = c(
        format_styled(
          "Attempted to declare {.code {name}} with no initial value."
        ),
        i = format_styled(
          "Typed object declarations require an initial value, ",
          "e.g. {.code <<name>>(<value>)}."
        )
      ),
      error_call = error_call
    )
  }
  if (length(decl_expr) > 2L) {
    name <- rlang::as_name(decl_expr[[1]])
    n <- length(decl_expr) - 1L
    abort_bad_input(
      message = c(
        format_styled(
          "Attempted to assign {n} initial values to {.code {name}}."
        ),
        i = format_styled(
          "Typed object declarations require exactly one initial value, ",
          "e.g. {.code <<name>>(<value>)}."
        )
      ),
      error_call = error_call
    )
  }

  abort_internal(
    format_styled("Invalid declaration was unhandled: {.code <<expr_label>>}.")
  )
}
