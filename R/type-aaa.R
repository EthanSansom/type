abstract_type <- S7::new_class("abstract_type", abstract = TRUE)

type <- S7::new_class(
  "type", 
  parent = abstract_type, 
  properties = list(
    traits = S7::new_property(S7::class_list, default = list()),
    modifications = S7::new_property(S7::class_character, default = character())
  )
)

union_type <- S7::new_class(
  "union_type", 
  parent = abstract_type, 
  properties = list(
    types = S7::new_property(S7::class_list, default = list()),
    modifications = S7::new_property(S7::class_character, default = character())
  )
)

#' Test if an object is a type
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a type, `FALSE` otherwise.
#' 
#' @examples
#' is_type(10L)
#' is_type(t_int)
#' 
#' @name type-predicates
NULL

#' @rdname type-predicates
#' @export
is_type <- function(x) {
  S7::S7_inherits(x, abstract_type)
}

#' @rdname type-predicates
#' @export
is_type_union <- function(x) {
  S7::S7_inherits(x, union_type)
}

#' Declare a type union
#'
#' @description
#'
#' `type_union()` returns a type that requires an object to satisfy at least
#' one of the types supplied to `...`.
#'
#' ```r
#' # Must be an integer OR a double
#' t_number <- type_union(t_int, t_dbl)
#'
#' # Must be a string OR NULL
#' t_opt_string <- type_union(t_string, t_null)
#' ```
#'
#' Nested unions are flattened, so `type_union(type_union(t_int, t_dbl), t_chr)`
#' is equivalent to `type_union(t_int, t_dbl, t_chr)`. Duplicate types are
#' silently dropped.
#'
#' @param ...
#'
#' Two or more types to combine. At least one must be supplied.
#'
#' @returns
#'
#' A type that accepts objects matching any of the supplied types.
#'
#' @seealso [is_type()], [is_type_union()] to test whether an object is a any type a or type union.
#'
#' @examples
#' t_index <- type_union(t_int, t_chr)
#' obj_is_type(1L, t_index)
#' obj_is_type("a", t_index)
#' obj_is_type(10.6, t_index)
#'
#' # Useful for nullable types
#' t_opt_chr <- type_union(t_chr, t_null)
#' obj_is_type(c("a", "b"), t_opt_chr)
#' obj_is_type(NULL, t_opt_chr)
#' obj_is_type(1L, t_opt_chr)
#'
#' @export
type_union <- function(...) {
  dots <- list(...)
  if (rlang::is_empty(dots)) {
    abort_bad_input(format_styled("At least one type must be supplied to {.arg ...}."))
  }
  for (i in seq_along(dots)) {
    assert_is_type(dots[[i]], x_name = paste0("..", i))
  }

  types <- list()
  modifications <- character()
  for (i in seq_along(dots)) {
    type <- dots[[i]]
    if (is_type_union(type)) {
      types <- c(types, type@types)
    } else {
      if (any(map_lgl(type@traits, S7::S7_inherits, has_on_trait))) {
        abort_bad_input(
          c(
            format_styled("Combining types with a {.fn has} trait is unsupported."),
            x = format_styled("{.arg {paste0('..', i)}} is a type with the {.fn has} trait.")
          )
        )
      }
      types <- c(types, list(type))
    }
    modifications <- c(modifications, type@modifications)
  }

  union_type(
    types = unique(types),
    modifications = unique(modifications)
  )
}

#' Check an object against a type
#'
#' @description
#' These functions are used to investiage whether an object `obj` has a type `type`.
#'
#' - `obj_is_type(obj, type)` returns `TRUE` if `obj` has type `type` and `FALSE` otherwise.
#' - `obj_inspect_type(obj, type)` prints a success or failure message for each type check run on `obj` then returns `NULL` invisibly.
#' - `obj_assert_type(obj, type)` raises a `<type_error_mistyped_obj>` error if `obj` is mistyped and returns `NULL` invisibly otherwise.
#'
#' @param obj
#' 
#' An object to check.
#' 
#' @param type 
#' 
#' A type to check against.
#' 
#' @param obj_name 
#' 
#' The name of `obj`, used in messages. Defaults to the expression passed to `obj`.
#'
#' @return
#' - `obj_is_type()`: `TRUE` if `obj` is the correct type, `FALSE` otherwise.
#' - `obj_inspect_type()`: `NULL` invisibly, called for its side effect.
#' - `obj_assert_type()`: `NULL` invisibly if `obj` is the correct type, otherwise raises an error.
#'
#' @examples
#' good <- TRUE
#' bad <- NA
#' 
#' # Test whether an object is a boolean
#' obj_is_type(good, t_bool)
#' obj_is_type(bad, t_bool)
#'
#' # Print the type tests run on the object
#' obj_inspect_type(good, t_bool)
#' obj_inspect_type(bad, t_bool)
#'
#' # Raise an error if the object is not a boolean
#' obj_assert_type(good, t_bool)
#' try(obj_assert_type(bad, t_bool))
#'
#' @name obj-type
NULL

#' @rdname obj-type
#' @export
obj_is_type <- function(obj, type) {
  assert_is_type(type)
  type_test(type, obj)
}

#' @rdname obj-type
#' @export
obj_inspect_type <- function(
  obj,
  type, 
  obj_name = rlang::caller_arg(obj)
) {
  assert_is_type(type)
  assert_is_string(obj_name)

  if (is_type_union(type)) {
    result <- obj_inspect_type_union(obj, obj_name, type)
  } else {
    result <- obj_inspect_type_single(obj, obj_name, type)
  }
  cat_bullets(c(result$header, result$message))
  invisible()
}

obj_inspect_type_single <- function(obj, obj_name, type) {
  result <- type_inspect(type, obj, obj_name)
  header <- if (result$success) {
    "Object {.arg {obj_name}} has the expected type."
  } else {
    "Object {.arg {obj_name}} does not have the expected type."
  }
  list(
    header = format_styled(header), 
    message = result$message,
    success = result$success
  )
}

obj_inspect_type_union <- function(obj, obj_name, type) {
  types <- type@types
  results <- map(types, type_inspect, obj, obj_name)
  successes <- map_lgl(results, `[[`, "success")
  if (any(successes)) {
    header <- format_styled("Object {.arg {obj_name}} has the expected type.")
    message <- results[[which.max(successes)]][["message"]]
  } else {
    header <- format_styled("Object {.arg {obj_name}} does not have the expected type.")
    message <- unlist(map(
      seq_along(results),
      function(i) {
        c(
          `*` = glue::glue("Type option {i} of {length(types)}:"),
          results[[i]][["message"]]
        )
      }
    ))
  }
  list(
    header = header, 
    message = message,
    success = any(successes)
  )
}

type_inspect <- function(type, obj, obj_name) {
  messages <- character()
  success <- TRUE
  for (trait in type@traits) {
    if (trait_test(trait, obj)) {
      messages <- c(messages, rlang::set_names(trait_describe(trait, obj_name), "v"))
    } else {
      header <- format_styled("Object {.arg {obj_name}} does not have the expected type.")
      messages <- c(messages, trait_diagnose(trait, obj, obj_name))
      success <- FALSE
      break
    }
  }
  list(message = messages, success = success)
}

#' @rdname obj-type
#' @export
obj_assert_type <- function(
  obj,
  type, 
  obj_name = rlang::caller_arg(obj)
) {
  assert_is_type(type)
  assert_is_string(obj_name)

  if (!is_type_union(type)) {
    for (trait in type@traits) {
      if (!trait_test(trait, obj)) {
        inline_abort_mistyped(
          type = type,
          message = c(
            format_styled("Object {.arg {obj_name}} is mistyped."),
            trait_diagnose(trait, obj, obj_name)
          ),
          error_subclass = "type_error_mistyped_obj"
        )
      }
    }
    return(invisible())
  }

  result <- obj_inspect_type_union(obj, obj_name, type)
  if (result$success) {
    return(invisible())
  }

  inline_abort_mistyped(
    type = type,
    message = c(
      format_styled("Object {.arg {obj_name}} is mistyped."),
      result$message
    ),
    error_subclass = "type_error_mistyped_obj"
  )
}

#' Return the expected type of the last mistyped object
#'
#' @description
#' 
#' `last_type()` returns the expected type of the last object to
#' fail a type check in [obj_assert_type()] or [typed()].
#' 
#' @return 
#' 
#' The last expected type. If no type assertions have been run, returns `NULL`.
#' 
#' @examples
#' # Returns `NULL` if no type checks have been run
#' last_type()
#' 
#' # `last_type()` returns `t_bool` after failed assertion
#' \dontrun{
#' obj_assert_type(10L, t_bool)
#' last_type()
#' }
#' 
#' @export
last_type <- function() {
  the$last_type
}

# generics ---------------------------------------------------------------------

method(base_print, type) <- function(x, ...) {
  cli::cat_line(glue::glue("<type>"))
  cat_bullets(rlang::set_names(type_describe(x, "<object>"), "*"))
  invisible(x)
}

method(base_print, union_type) <- function(x, ...) {
  cli::cat_line(glue::glue("<type_union>"))
  types <- x@types
  for (i in seq_along(types)) {
    cat_bullets(c(
      i = glue::glue("Type {i} of {length(types)}:"),
      rlang::set_names(type_describe(types[[i]], "<object>"), "*")
    ))
  }
  invisible(x)
}

# helpers ----------------------------------------------------------------------

type_test <- function(type, obj) {
  if (!is_type_union(type)) {
    for (trait in type@traits) {
      if (!trait_test(trait, obj)) return(FALSE)
    }
    return(TRUE)
  }

  for (type in type@types) {
    if (all(map_lgl(type@traits, trait_test, obj))) return(TRUE)
  }
  FALSE
}

type_diagnose <- function(type, obj, obj_name) {
  for (trait in type@traits) {
    if (!rlang::is_true(trait_test(trait, obj))) {
      return(trait_diagnose(trait, obj, obj_name))
    }
  }
}

type_describe <- function(type, obj_name) {
  if (rlang::is_empty(type@traits)) {
    return(format_styled("{.arg {obj_name}} is an R object."))
  }
  unlist(map(type@traits, trait_describe, obj_name))
}

#' @rdname inlined-functions
#' @export
inline_abort_mistyped <- function(
  type,
  message,
  error_subclass = character(),
  error_call = rlang::caller_env()
) {
  the$last_type <- type
  rlang::abort(
    c(
      message,
      i = format_styled("Run {.run last_type()} to get the expected type.")
    ),
    class = c(error_subclass, "type_error_mistyped", "type_error"),
    call = error_call
  )
}
