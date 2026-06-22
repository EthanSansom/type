# typed ------------------------------------------------------------------------

# TODO: Document
#' @export
typed <- function(..., returns = NULL) {
  context_local("typed")
  parent_frame <- rlang::caller_env()

  # Values supplied to `...` may be a relation (e.g. `same_typed()`) or a
  # function definition (e.g. `function(x) { ... }`).
  dots <- rlang::enexprs(...)
  fun_expr <- NULL
  relations <- list()
  for (i in seq_along(dots)) {
    if (rlang::is_call(dots[[i]], "function", ns = "")) {
      if (!is.null(fun_expr)) {
        abort_bad_input("Only one function definition may be supplied to {.arg ...}.")
      }
      fun_expr <-dots[[i]]
      next
    }
    
    value <- eval(dots[[i]], parent_frame)
    if (!is_relation(value)) {
      abort_bad_input(format_styled(
        "{.arg {paste0('..', i)}} must be a function definition or a relation, ",
        "not <<fmt_r_type(value)>>."
      ))
    }
    relations <- c(relations, list(value))
  }
  if (is.null(fun_expr)) {
    abort_bad_input("A function definition must be supplied to {.arg ...}.")
  }
  
  args <- parse_typed_fun_args(fun_expr, parent_frame)
  untyped_body <- fun_expr[[3]]
  body <- untyped_body

  args_defaults <- args$defaults
  args_types <- args$types
  args_names <- names(args_defaults)
  args_syms <- rlang::syms(args_names)

  # Relations may only have arguments defined in the function
  for (relation in relations) {
    bad_args <- setdiff(relation$args, args_names)
    if (!rlang::is_empty(bad_args)) {
      abort_bad_input(
        c(
          format_styled("Relations must only include arguments supplied to the function definition."),
          x = format_styled("Arguments {backtick(oxford(bad_args))} are not in the function definition.")
        )
      )
    }
  }

  args_type_assertions <- pmap(
    list(
      arg_type = args_types,
      arg_name = args_names,
      arg_sym = args_syms
    ),
    function(arg_type, arg_name, arg_sym) {
      if (rlang::is_empty(arg_type@traits)) {
        return(NULL)
      }
      arg_assertion_expr(arg_type, arg_name, arg_sym, error_call = parent_frame)
    }
  )
  args_type_assertions <- drop(args_type_assertions, is.null)
  relation_assertions <- map(relations, `[[`, "call")

  if (!is.null(returns)) {
    assert_is_type(returns)
    body <- insert_returns_type(body, returns)
  }

  out <- rlang::new_function(
    args = args_defaults,
    body = rlang::expr({
      {
        !!!args_type_assertions
        !!!relation_assertions
      }
      !!body
    }),
    env = parent_frame
  )
  class(out) <- c("type_typed_function", "function")
  attr(out, "args_types") <- args_types
  attr(out, "returns_type") <- returns %||% type()
  attr(out, "args_relations") <- relations
  attr(out, "untyped_body") <- untyped_body
  out
}

# parse ------------------------------------------------------------------------

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
    abort_bad_input(message, error_call = error_call)
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
        abort_bad_input(
          format_styled("Can't type argument {.arg {arg_name}}."),
          parent = e,
          error_call = error_call
        )
      }
    )
    if (!is_type(type)) {
      abort_bad_input(
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
  # default argument may raise an error (e.g. function(x = stop("AH")) {}), in
  # which case we should use `t_any %:% stop("AH")` instead
  arg <- rlang::try_fetch(
    eval(arg_expr, parent_frame),
    error = function(e) {
      abort_bad_input(
        message = c(
          format_styled("Can't type argument {.arg {arg_name}}."),
          x = format_styled("Could not evaluate the default value of {.arg {arg_name}}."),
          i = format_styled(
            "Use {.code <<arg_name>> = t_any %:% <default>} to avoid ",
            "evaluation of the default value."
          )
        ),
        error_call = error_call,
        parent = e
      )
    }
  )
  if (is_type(arg)) {
    list(type = arg, default = rlang::missing_arg())
  } else {
    list(type = type(), default = arg_expr)
  }
}

arg_assertion_expr <- function(arg_type, arg_name, arg_sym, error_call) {
  if (arg_name == "...") {
    return(dots_assertion_expr(arg_type, error_call))
  }

  arg_mods <- arg_type@modifications
  assertion_expr <- rlang::call2(
    "inline_arg_assert_type",
    arg = arg_sym,
    arg_name = arg_name,
    type = arg_type,
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

dots_assertion_expr <- function(arg_type, error_call) {
  modifications <- arg_type@modifications
  bad_mods <- intersect(c("maybe", "optional"), modifications)
  if (!rlang::is_empty(bad_mods)) {
    abort_bad_input(
      "{qty(bad_mods)}Modification{?s} {.fn {bad_mods}} may not be used with the {.arg ...} argument.",
      error_call = error_call
    )
  }

  if ("endotted" %in% modifications) {
    rlang::call2(
      .fn = "inline_dotlist_assert_type",
      dots = rlang::call2("list2", quote(...), .ns = "rlang"),
      type = arg_type,
      .ns = "type"
    )
  } else {
    rlang::call2(
      .fn = "inline_dots_assert_type",
      quote(...),
      .type = arg_type,
      .ns = "type"
    )
  }
}

insert_returns_type <- function(body, returns_type) {
  type_wrap_result <- function(expr, type) {
    rlang::call2(
      "inline_result_assert_type",
      value = expr,
      type = type,
      .ns = "type"
    )
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

# assert -----------------------------------------------------------------------

# TODO: Document
#' @export
inline_arg_assert_type <- function(
  arg, 
  type, 
  arg_name, 
  error_call = rlang::caller_env()
) {
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, arg))) {
      rlang::abort(
        c(
          format_styled("Argument {.arg {arg_name}} is mistyped."),
          trait_diagnose(trait, arg, arg_name)
        ),
        class = c("type_error_mistyped_arg", "type_error_mistyped", "type_error"),
        call = error_call
      )
    }
  }
  return(invisible())
}

# TODO: Document
#' @export
inline_result_assert_type <- function(
  value, 
  type, 
  error_call = rlang::caller_env()
) {
    for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, value))) {
      rlang::abort(
        c(
          format_styled("Return value is mistyped."),
          trait_diagnose(trait, value, "<result>")
        ),
        class = c("type_error_mistyped_arg", "type_error_mistyped", "type_error"),
        call = error_call
      )
    }
  }
  value
}

# TODO: Document
#' @export
inline_dots_assert_type <- function(
  ...,
  .type,
  .error_call = rlang::caller_env()
) {
  for (i in rlang::seq2(1, ...length())) {
    inline_arg_assert_type(
      arg = ...elt(i),
      type = .type,
      arg_name = paste0("..", i), 
      error_call = .error_call
    )
  }
}

# TODO: Document
#' @export
inline_dotlist_assert_type <- function(
  dots,
  type,
  error_call = rlang::caller_env()
) {
  inline_arg_assert_type(
    arg = dots,
    type = type,
    arg_name = "list(...)", 
    error_call = error_call
  )
}

# methods ----------------------------------------------------------------------

#' @export
print.type_typed_function <- function(x, ...) {
  untyped <- unclass(x)
  body(untyped) <- attr(x, "untyped_body")
  args_types <- attr(x, "args_types")
  args_names <- names(args_types)
  args_relations <- attr(x, "args_relations")

  cat("<typed>\n")
  print(untyped)

  if (!rlang::is_empty(args_types)) {
    cat("Arguments:\n")
    for (i in seq_along(args_types)) {
      if (args_names[[i]] == "...") {
        if ("endotted" %in% args_types[[i]]@modifications) {
          description <- type_describe(args_types[[i]], "list(...)")
        } else {
          description <- paste("Each element of", str_lower1(type_describe(args_types[[i]], "...")))
        }
        cat_bullets(rlang::set_names(description, "*"))
        next
      }
      cat_bullets(rlang::set_names(type_describe(args_types[[i]], args_names[[i]]), "*"))
    }
  }
  if (!rlang::is_empty(args_relations)) {
    descriptions <- unlist(map(args_relations, `[[`, "description"))
    cat("Relations:\n")
    cat_bullets(rlang::set_names(descriptions, "*"))
  }
  cat("Returns:\n")
  cat_bullets(rlang::set_names(type_describe(attr(x, "returns_type"), "<result>"), "*"))

  return(invisible(x))
}
