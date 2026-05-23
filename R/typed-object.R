# operators --------------------------------------------------------------------

# TODO: Give a hint for type declarations which look like constructors:
#
# Error in `t_point %:% c(x = 1L, y = 2L)`:
# ! Attempted to assign 2 initial values to `c`.
# i Typed object declarations require exactly one initial value, e.g. `c(<value>)`.

`%:%` <- function(type, declaration) {
  if (!is_type(type)) {
    type_abort_bad_input(format_styled(
      "The left-hand side of {.code %:%} must be a type, ",
      "not <<fmt_r_type(type)>>."
    ))
  }
  parent_frame <- rlang::caller_env()

  decl_expr <- rlang::enexpr(declaration)
  declared <- parse_declaration(decl_expr, parent_frame)
  name <- declared$name
  value <- declared$value

  validation_result <- type_validate(type, value, "<value>")
  if (!is.null(validation_result)) {
    value <- as_label(decl_expr[[2]])
    abort_mistyped(
      message = c(
        format_styled(
          "Attempted to initialize {.code {name}} with a mistyped value: {.code {value}}."
        ),
        validation_result
      )
    )
  }

  is_const <- FALSE
  if (has_modifications(type)) {
    mods <- type@modifications
    is_const <- "const" %in% mods
    warn_if_non_const_mods(mods, name)
  }

  if (is_const) {
    binding_fun <- eval(rlang::expr(
      local(
        {
          VALUE <- !!value
          NAME <- !!name
          out <- function(x) {
            if (missing(x)) {
              return(VALUE)
            }
            abort_mistyped(
              format_styled("Can't assign to the constant {.arg {NAME}}."),
              error_call = !!parent_frame
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
    validate_expr <- type_validate_expr(
      type = type,
      obj_sym = rlang::sym("x"),
      obj_name = "<value>",
      env = parent_frame
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
            abort_mistyped(
              c(
                format_styled(
                  "Attempted to assign a mistyped value to {.arg {NAME}}."
                ),
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
    env_unbind(parent_frame, name)
  }
  makeActiveBinding(name, binding_fun, parent_frame)

  return(invisible(value))
}

# TODO: Auto Typed Assignment
`%:<-%` <- function(name, value) {}

# TODO: Belongs in typed-function.R
# Typed Lambda
`%:>%` <- function(fun, type) {}

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
        init <- as_label(decl_expr[[2]])
        type_abort_bad_input(
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

  expr_label <- as_label(decl_expr)
  if (rlang::is_symbol(decl_expr)) {
    type_abort_bad_input(
      message = c(
        format_styled(
          "The right-hand-side of {.fn `%:%`} must be a declaration, not the symbol {.code {expr_label}}."
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

    type_abort_bad_input(
      message = c(
        format_styled(
          "The right-hand side of {.fn `%:%`} must be a declaration ",
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
    type_abort_bad_input(
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
    type_abort_bad_input(
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

  type_abort_internal(
    format_styled("Invalid declaration was unhandled: {.code <<expr_label>>}.")
  )
}

warn_if_non_const_mods <- function(mods, obj_name) {
  non_const_mods <- mods[mods != "const"]
  if (rlang::is_empty(non_const_mods)) {
    return(invisible())
  }

  non_const_mods <- backtick(paste0(non_const_mods, "()"))
  n <- length(non_const_mods)
  warn(
    message = c(
      format_styled(
        "{qty(n)}Removing the <<oxford(non_const_mods)>> modifier{?s} ",
        "from the type of {.arg {obj_name}}."
      ),
      i = format_styled(
        "{qty(n)}The <<oxford(non_const_mods)>> modifier{?s} ",
        "{?applies/apply} only in function-typing contexts."
      )
    )
  )
}
