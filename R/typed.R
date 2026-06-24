# typed ------------------------------------------------------------------------

#' Declare a type-checked function
#'
#' @description
#' `typed()` inserts argument and (optionally) return type validation into
#' a function. When printed, a typed function shows the types of its arguments
#' and return value.
#'
#' @section Argument Typing:
#' 
#' A typed function's arguments may be annotated with any valid type, e.g.
#' [t_int], including types refined with additional traits, e.g. [sized()].
#'
#' ```r
#' f <- typed(function(x = t_int, y = t_chr |> sized(1L)) { paste(x, y) })
#' f(1L, "a")         # ok
#' f(TRUE, "a")       # error, `x` is not an integer
#' f(1L, c("a", "b")) # error, `y` is not a size 1
#' ```
#' 
#' By default, annotated function arguments have no default value. To set one,
#' use the syntax `arg = <type> %:% <default>`.
#'
#' ```r
#' f <- typed(function(x = t_int %:% 0L) { x })
#' f()    # 0L
#' f(1L)  # 1L
#' ```
#' 
#' Dots (`...`) may also be type annotated. By default, each dot will be checked
#' against the supplied type. To treat the dots as a single argument, e.g. as
#' a list, use the [t_dots] annotation.
#' 
#' ```r
#' # Each `...` must be an integer
#' f <- typed(function(... = t_int) list(...))
#' 
#' # Exactly 2 dots must be supplied
#' g <- typed(function(... = t_dots |> sized(2L)) list(...))
#' ```
#' 
#' Arguments, with the exception of `...`, may be modified using [optional()]
#' or [maybe()]. [optional()] arguments may by unsupplied while [maybe()]
#' arguments may be `NULL`.
#' 
#' ```r
#' # `x` is an integer or is unsupplied
#' f <- typed(function(x = optional(t_int)) if (missing(x)) 0L else x)
#' 
#' # `x` is an integer or `NULL`
#' g <- typed(function(x = maybe(t_int)) x)
#' ```
#' 
#' @section Relations:
#' 
#' Relations, e.g. [same_sized()], declare between-argument checks. A relation
#' call may be placed before or after the function definition.
#'
#' ```r
#' # `x` and `y` must be integers of the same size
#' f <- typed(
#'   same_sized(x, y), 
#'   function(x = t_int, y = t_int) x + y
#' )
#' ```
#' 
#' @section Return Typing:
#'
#' Use the `returns` argument to enforce a type on the return value. Return
#' types cannot be modified using [optional()] or [maybe()].
#'
#' ```r
#' f <- typed(
#'   function(x = t_bool) if (x) "yes" else "no", 
#'   returns = t_chr
#' )
#' ```
#'
#' @param ... 
#' 
#' A type annotated function definition, optionally accompanied by one or more
#' relation calls (e.g. [same_sized()]). Exactly one function definition must
#' be supplied.
#' 
#' @param returns 
#' 
#' A type for the function's return value. By default `returns` is `NULL`,
#' meaning the return value can have any type.
#'
#' @return A typed function.
#'
#' @seealso [optional()] and [maybe()] for argument modifications, [same_sized()] and [same_classed()] for between-argument constraints.
#'
#' @examples
#' any2 <- typed(
#'   function(... = t_lgl, na.rm = t_bool %:% FALSE) { 
#'     any(..., na.rm) 
#'   },
#'   returns = t_lgl |> sized(1L)
#' )
#' print(any2)
#' 
#' # Correctly typed inputs proceed normally
#' any2(c(TRUE, FALSE), TRUE, na.rm = FALSE)
#' 
#' # Mistyped inputs cause an error
#' try(any2(TRUE, na.rm = "no"))
#' 
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

#nocov start

#' Inlined helper functions
#' 
#' @description
#' 
#' These functions are inserted into [typed()] functions and are not meant
#' for external use. They are exported only to ensure that [typed()] functions
#' call the correct helper, e.g. `type::inline_result_assert_type()`, and
#' not a globally defined function, e.g. `inline_result_assert_type()`.
#' 
#' @param type,.type A type.
#' @param value A value to be checked.
#' @param arg An argument to be checked.
#' @param arg_name An argument name to use in error messages.
#' @param obj An object to be checked.
#' @param obj_name An object name to use in error messages.
#' @param error_call,.error_call The call to use in error messages.
#' @param ... Dots to be checked individually.
#' @param dots A list of dots to be checked.
#'
#' @name inlined-functions
#' 
#' @examples
#' try(inline_result_assert_type(10L, t_chr))
NULL

#' @rdname inlined-functions
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

#' @rdname inlined-functions
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

#' @rdname inlined-functions
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

#' @rdname inlined-functions
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

#nocov end

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
