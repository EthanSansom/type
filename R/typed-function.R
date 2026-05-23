# typed ------------------------------------------------------------------------

# TODO: Guard against missingness explicitly with a `check_required()` call
#       at the top for any typed arguments not explicitly marked as `t_any`,
#       `t_missing`, or `optional()`.
typed <- function(..., fun = NULL, returns = NULL) {
  parent_frame <- rlang::caller_env()
  fun_expr <- rlang::enexpr(fun)

  # The `function() {}` call may be provided either to `...` or via `fun`. This
  # is for future compatibility. In {type} V1, calls will typically look like:
  # `foo <- typed(function(...) {})`
  #
  # However, eventually we'll have calls like:
  # `typed(t_U = t_int, same_size(x, y), <more conditions>, function(...) {})`
  # in which case you might want to explicitly use the `fun` argument.
  dots_exprs <- parse_typed_dots(...)
  supplied_dots_fun <- dots_exprs$has_fun
  supplied_fun <- !is.null(fun)
  if (supplied_dots_fun && supplied_fun) {
    type_abort_bad_input(c(
      format_styled(
        "A function was supplied to both {.arg ...} and the {.arg fun} argument."
      ),
      i = format_styled(
        "A function may be supplied to either {.arg ...} or to {.fn fun}."
      )
    ))
  } else if (supplied_fun) {
    if (!rlang::is_call(fun_expr, "function")) {
      fun_label <- rlang::as_label(fun_expr)
      if (rlang::is_symbol(fun_expr)) {
        bad <- "the symbol {.code {fun_label}}"
      } else if (fun_label == "{...}") {
        bad <- "an expression"
      } else {
        bad <- "the expression {.code {fun_label}}"
      }
      type_abort_bad_input(c(
        format_styled("{.arg fun} must be a function definition, not <<bad>>."),
        i = format_styled("Use the form {.code fun = function(...) {{...}}}.")
      ))
    }
  } else {
    fun_expr <- dots_exprs$fun # pre-validated by parse_typed_dots()
  }

  args <- parse_typed_fun_args(fun_expr, parent_frame)
  untyped_body <- fun_expr[[3]]
  body <- untyped_body

  args_defaults <- args$defaults
  args_types <- args$types
  args_names <- names(args_defaults)
  args_syms <- rlang::syms(args_names)

  args_type_assertions <- pmap(
    list(
      arg_type = args_types,
      arg_name = args_names,
      arg_sym = args_syms
    ),
    \(arg_type, arg_name, arg_sym) {
      if (is_type_any(arg_type)) { # TODO: `is_type_any` will need to be exported
        return(NULL)
      }
      arg_assertion_expr(arg_type, arg_name, arg_sym, error_call = parent_frame)
    }
  )
  args_type_assertions <- drop(args_type_assertions, is_null)

  if (!is.null(returns)) {
    assert_is_type(returns)
    body <- insert_returns_type(body, returns)
  }

  out <- rlang::new_function(
    args = args_defaults,
    body = rlang::expr({
      {
        !!!args_type_assertions
      }
      !!body
    }),
    env = parent_frame
  )
  class(out) <- c("type_typed_function", "function")
  attr(out, "args_types") <- args_types
  attr(out, "returns_type") <- returns %||% t_any
  attr(out, "untyped_body") <- untyped_body
  out
}

# TODO:
# Typed Lambda
`%:>%` <- function(fun, type) {}

# typed function helpers -------------------------------------------------------

parse_typed_dots <- function(..., .error_call = rlang::caller_env()) {
  dots_exprs <- rlang::enexprs(...)
  fun_exprs <- keep(dots_exprs, is_call, name = "function")
  has_fun <- length(fun_exprs) == 1L

  # Currently allowing at most one dot, but this will be relaxed in the future
  # to support `typed(t_T = t_int, function(x = t_T, y = t_T) {...})` syntax.
  if (length(dots_exprs) > 1L) {
    n <- length(dots_exprs)
    type_abort_bad_input(
      format_styled(
        "{.arg ...} must contain exactly one or no arguments, but {n} were supplied."
      ),
      error_call = .error_call
    )
  }
  if (length(dots_exprs) == 1L && !has_fun) {
    expr_label <- rlang::as_label(dots_exprs[[1]])
    if (rlang::is_symbol(expr_label)) {
      bad <- "the symbol {.code {expr_label}}"
    } else if (expr_label == "{...}") {
      bad <- "an expression"
    } else {
      bad <- "the expression {.code {expr_label}}"
    }
    type_abort_bad_input(
      format_styled("{.arg ..1} must be a function declaration, not <<bad>>."),
      error_call = .error_call
    )
  }
  # if (length(fun_exprs) > 1L) {
  #   type_abort_bad_input(
  #     format_styled("{.arg ...} may contain exactly one function declaration, not {length(fun_exprs)}."),
  #     error_call = .error_call
  #   )
  # }

  list(
    has_fun = has_fun,
    fun = if (has_fun) fun_exprs[[1]]
  )
}

parse_typed_fun_args <- function(
  call_to_function,
  parent_frame,
  error_call = rlang::caller_env()
) {
  args_exprs <- call_to_function[[2]]
  args_parsed <- map2(
    args_exprs,
    names(args_exprs),
    parse_typed_arg_expr,
    parent_frame = parent_frame,
    error_call = error_call
  )

  defaults <- map(args_parsed, `[[`, "default")
  types <- map(args_parsed, `[[`, "type")

  if ("..." %in% names(defaults) && !rlang::is_missing(defaults[["..."]])) {
    default_label <- rlang::as_label(defaults[["..."]])
    message <- format_styled("Argument {.arg ...} can't have a default value.")
    if (default_label != "{...}") {
      message <- c(
        message,
        x = format_styled(
          "Attempted to set default: {.code ... = <<default_label>>}."
        )
      )
    }
    type_abort_bad_input(message, error_call = error_call)
  }

  list(
    defaults = map(args_parsed, `[[`, "default"),
    types = map(args_parsed, `[[`, "type")
  )
}

parse_typed_arg_expr <- function(
  arg_expr,
  arg_name,
  parent_frame,
  error_call = rlang::caller_env()
) {
  # E.g. foo(x)
  if (rlang::is_missing(arg_expr)) {
    return(list(type = t_any, default = rlang::missing_arg()))
  }

  # E.g. foo(x = t_int %:% 10L)
  if (rlang::is_call(arg_expr, "%:%", ns = c("", "type"))) {
    default <- arg_expr[[3]]
    type_expr <- arg_expr[[2]]

    type <- rlang::try_fetch(
      eval(type_expr, parent_frame),
      error = function(e) {
        type_abort_bad_input(
          format_styled("Can't type argument {.arg {arg_name}}."),
          parent = e,
          error_call = error_call
        )
      }
    )
    if (!is_type(type)) {
      type_abort_bad_input(
        message = c(
          format_styled("Can't type argument {.arg {arg_name}}."),
          x = format_styled(
            "The left-hand-side of {.code %:%} must be a type, not <<fmt_r_type(type)>>."
          ),
          i = format_styled(
            "Use {.code <<arg_name>> = <type> %:% <value>} to ",
            "create a typed argument with a default value."
          )
        ),
        error_call = error_call
      )
    }

    return(list(type = type, default = default))
  }

  # `arg_expr` is some call or symbol, possibly a type (e.g. `x = t_int`). The
  # default argument may raise an error (e.g. function(x = stop("AH")) {}), but
  # we want to immediately throw malformed type errors, e.g. from:
  # `typed(function(x = t_int |> sized("A")) {})`
  arg <- rlang::try_fetch(
    eval(arg_expr, parent_frame),
    type_error_malformed_type = function(e) rlang::cnd_signal(e),
    error = function(e) NULL
  )
  if (is_type(arg)) {
    list(type = arg, default = rlang::missing_arg())
  } else {
    list(type = t_any, default = arg_expr)
  }
}

arg_assertion_expr <- function(arg_type, arg_name, arg_sym, error_call) {
  if (arg_name == "...") {
    return(dots_assertion_expr(arg_type, error_call))
  }

  if (has_modifications(arg_type)) {
    arg_mods <- validate_arg_mods(
      arg_name = arg_name,
      arg_mods = arg_type@modifications,
      error_call = error_call
    )
  } else {
    arg_mods <- character()
  }

  # Available modifications:
  # - Wraps argument:  unsafe | quoted | expred
  # - Wraps assertion: optional, maybe
  if ("unsafe" %in% arg_mods) {
    modded_arg_sym <- rlang::expr(inline_try_unsafe_arg(!!arg_sym, !!arg_name))
  } else if ("quoted" %in% arg_mods) {
    modded_arg_sym <- rlang::expr(rlang::enquo(!!arg_sym))
  } else if ("expred" %in% arg_mods) {
    modded_arg_sym <- rlang::expr(rlang::enexpr(!!arg_sym))
  } else {
    modded_arg_sym <- arg_sym
  }

  validate_expr <- type_validate_expr(
    type = arg_type,
    obj_sym = arg_sym,
    obj_name = arg_name,
    env = rlang::expr(rlang::caller_env())
  )
  assertion_expr <- rlang::call2(
    "inline_abort_if_mistyped_arg",
    arg_name = arg_name,
    arg_validation_result = validate_expr,
    .ns = "type"
  )

  is_optional <- "optional" %in% arg_mods
  is_maybe <- "maybe" %in% arg_mods
  if (is_optional && is_maybe) {
    rlang::expr(
      if (!rlang::is_missing(!!arg_sym) && !is.null(!!arg_sym)) {
        !!assertion_expr
      }
    )
  } else if (is_optional) {
    rlang::expr(
      if (!rlang::is_missing(!!arg_sym)) {
        !!assertion_expr
      }
    )
  } else if (is_maybe) {
    rlang::expr(
      if (!is.null(!!arg_sym)) {
        !!assertion_expr
      }
    )
  } else {
    assertion_expr
  }
}

# TODO: Think about how we want to compile assertions on the `...` argument
dots_assertion_expr <- function(arg_type, error_call) {
  if (!has_modifications(arg_type)) {
    return(rlang::expr(dots_assert_type(..., .type = !!arg_type)))
  }

  arg_mod <- validate_arg_mods(
    arg_name = "...",
    arg_mods = arg_type@modifications,
    error_call = error_call
  )

  switch(
    arg_mod,
    # The entire `...` argument is treated as a single object to type-check
    endotted = rlang::expr(obj_assert_type(
      rlang::list2(...),
      type = !!arg_type,
      obj_name = "..."
    )),
    expred_dots = rlang::expr(obj_assert_type(
      rlang::enexprs(...),
      type = !!arg_type,
      obj_name = "..."
    )),
    quoted_dots = rlang::expr(obj_assert_type(
      rlang::enquos(...),
      type = !!arg_type,
      obj_name = "..."
    )),
    # Each element of `...` is treated as an object to type-check
    expred_dot = rlang::expr(defused_dots_assert_type(
      rlang::enexprs(...),
      type = !!arg_type
    )),
    quoted_dot = rlang::expr(defused_dots_assert_type(
      rlang::enquos(...),
      type = !!arg_type
    ))
  )
}

MOD_CONFLICTS <- list(
  c("expred", "quoted", "unsafe")
)
MOD_FORBIDDEN <- c("const")
MOD_ALLOWED <- list(
  "..." = c("expred", "quoted", "endotted"),
  other = c("expred", "quoted", "unsafe", "optional", "maybe")
)
ENDOTTED_MOD_MAP <- list(
  expred = c(single = "expred_dot", endotted = "expred_dots"),
  quoted = c(single = "quoted_dot", endotted = "quoted_dots")
)

validate_arg_mods <- function(arg_name, arg_mods, error_call) {
  if (rlang::is_empty(arg_mods)) {
    return(arg_mods)
  }
  arg_mods <- unique(arg_mods)

  abort_mod <- function(...) {
    type_abort_bad_input(c(...), call = error_call)
  }

  bad_forbidden <- intersect(MOD_FORBIDDEN, arg_mods)
  if (length(bad_forbidden)) {
    abort_mod(
      "Typed argument {.arg {arg_name}} has the {.fun {bad_forbidden[[1]]}} modifier.",
      i = "Arguments cannot be marked as {.fun {bad_forbidden[[1]]}}."
    )
  }

  if (length(arg_mods) <= 1) {
    return(arg_mods)
  }

  for (conflicting_mods in MOD_CONFLICTS) {
    bad_conflicting <- intersect(conflicting_mods, arg_mods)
    if (length(bad_conflicting) > 1) {
      bad_funs <- backtick(paste0(bad_conflicting, "()"))
      abort_mod(
        format_styled(
          "Typed argument {.arg {arg_name}} has the <<oxford(bad_funs, 'and')>> modifiers."
        ),
        i = format_styled(
          "Arguments may only use one of <<oxford(bad_funs, 'or')>>."
        )
      )
    }
  }

  allowed_mods <- MOD_ALLOWED[[if (arg_name == "...") "..." else "other"]]
  bad_mods <- setdiff(arg_mods, allowed_mods)
  if (length(bad_mods)) {
    abort_mod(
      "The {.fun {bad_mods[[1]]}} modifier is not supported for {.arg {arg_name}}."
    )
  }

  if (arg_name == "...") {
    lang_mod <- intersect(c("expred", "quoted"), arg_mods)
    if (length(lang_mod)) {
      dot_type <- if ("endotted" %in% arg_mods) "endotted" else "single"
      dot_mod <- ENDOTTED_MOD_MAP[[lang_mod]][[dot_type]]
      arg_mods <- c(arg_mods[arg_mods %notin% c(lang_mod, "endotted")], dot_mod)
    }
    if (length(arg_mods) > 1) {
      rlang::abort(
        glue::glue("`...` contains >1 modifications: {commas(arg_mods)}"),
        .internal = TRUE
      )
    }
  }

  arg_mods
}

insert_returns_type <- function(body, returns_type) {
  type_wrap_result <- function(expr, type) {
    rlang::expr(obj_assert_type(!!expr, !!type, obj_name = "<result>"))
  }
  update_returns <- function(expr, returns_type) {
    if (!rlang::is_call(expr)) {
      return(expr)
    }
    if (rlang::is_call(expr, "return")) {
      returns_expr <- expr[[2]]
      expr[[2]] <- type_wrap_result(returns_expr, returns_type)
      return(expr)
    }
    if (!rlang::is_call(expr, c("if", "for", "while", "repeat", "{"))) {
      return(expr)
    }

    # Recursively update return type within nested control-flow
    expr[] <- map(expr, update_returns, returns_type = returns_type)
    expr
  }
  body <- update_returns(body, returns_type)

  # update_returns() doesn't catch the last expression if it's not a return() call
  last_expr <- body[[length(body)]]
  if (!rlang::is_call(last_expr, "return")) {
    body[[length(body)]] <- type_wrap_result(last_expr, returns_type)
  }
  body
}

# inlined helpers --------------------------------------------------------------

# TODO: These will need to be exported

inline_try_unsafe_arg <- function(
  arg,
  arg_name,
  error_call = rlang::caller_env()
) {
  rlang::try_fetch(
    arg,
    error = function(e) {
      rlang::abort(
        "Can't evaluate argument {.arg {arg_name}}.",
        call = error_call,
        parent = e
      )
    }
  )
}

inline_abort_if_mistyped_arg <- function(
  arg_name,
  arg_validation_result,
  error_call = rlang::caller_env()
) {
  if (is.null(arg_validation_result)) {
    return(NULL)
  }
  abort_mistyped(
    c(
      format_styled("Argument {.arg {arg_name}} is mistyped."),
      arg_validation_result
    ),
    error_call = error_call,
    error_subclass = "type_error_mistyped_arg"
  )
}

# typed class ------------------------------------------------------------------

#' @export
print.type_typed_function <- function(x, ...) {
  body(x) <- attr(x, "untyped_body")

  cat("<typed>\n")
  NextMethod()
}
