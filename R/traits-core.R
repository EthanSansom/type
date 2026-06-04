# definitions ------------------------------------------------------------------

bare_typed <- NULL

bare_typed_trait <- new_trait_class("bare_typed", params = "typeof")

sized <- NULL

sized_trait <- new_trait_class("sized", params = "size")

complete <- NULL

complete_trait <- new_trait_class("complete", params = character())

# Internal
is_vector_trait <- new_trait_class("is_vector", params = character())
is_atomic_trait <- new_trait_class("is_atomic", params = character())

# trait_name -------------------------------------------------------------------

method(trait_name, bare_typed_trait) <- function(trait) {
  paste0('typeof("', trait@typeof, '")')
}

method(trait_name, sized_trait) <- function(trait) {
  paste0("sized(", trait@size, ")")
}

method(trait_name, complete_trait) <- function(trait) {
  "complete()"
}

# trait_test -------------------------------------------------------------------

method(trait_test, bare_typed_trait) <- function(trait, obj) {
    !is.object(obj) &&
    switch(
      trait@typeof,
      double = is.double(obj),
      integer = is.integer(obj),
      logical = is.logical(obj),
      character = is.character(obj),
      complex = is.complex(obj),
      raw = is.raw(obj),
      list = is.list(obj),
      `NULL` = is.null(obj),
      environment = is.environment(obj),
      symbol = is.symbol(obj),
      pairlist = is.pairlist(obj),
      language = is.language(obj),
      expression = is.expression(obj),
      `S4` = typeof(obj) == "S4",
      closure = typeof(obj) == "closure",
      special = typeof(obj) == "special",
      builtin = typeof(obj) == "builtin",
      externalptr = typeof(obj) == "externalptr",
      weakref = typeof(obj) == "weakref",
      promise = typeof(obj) == "promise",
      char = typeof(obj) == "char",
      bytecode = typeof(obj) == "bytecode",
      any = typeof(obj) == "any",
      `...` = typeof(obj) == "...",
      `ANY` = typeof(obj) == "ANY"
    )
}

method(trait_test, sized_trait) <- function(trait, obj) {
  vctrs::vec_size(obj) == trait@size
}

method(trait_test, complete_trait) <- function(trait, obj) {
  !vctrs::vec_any_missing(obj)
}

method(trait_test, is_vector_trait) <- function(trait, obj) {
  vctrs::obj_is_vector(obj)
}

method(trait_test, is_atomic_trait) <- function(trait, obj) {
  is.atomic(obj)
}

# trait_validate/absent_message ------------------------------------------------

# method(trait_validate, bare_typed_trait) <- function(trait, obj, obj_name) {}

method(trait_absent_message, bare_typed_trait) <- function(
  trait,
  obj,
  obj_name
) {
  header <- format_styled(
    "{.arg {obj_name}} must be a bare {.cls {trait@typeof}}."
  )
  if (is.object(obj)) {
    footer <- format_styled(
      "{.arg {obj_name}} is a {obj_oo_type(obj)} object of class {.cls {class(obj)}}."
    )
  } else {
    footer <- format_styled("{.arg {obj_name}} is a bare {.cls {typeof(obj)}}.")
  }
  c(i = header, x = footer)
}

# method(trait_validate, sized_trait) <- function(trait, obj, obj_name) {}

method(trait_absent_message, sized_trait) <- function(
  trait,
  obj,
  obj_name
) {
  if (vec_vctrs_type(obj) == "bare_dataframe") {
    must <- "have {trait@size} rows"
    not <- "{vctrs::vec_size(obj)} rows"
  } else {
    must <- "be size {trait@size}"
    not <- "size {vctrs::vec_size(obj)}"
  }
  c(x = format_styled("{.arg {obj_name}} must <<must>>, not <<not>>."))
}

# method(trait_validate, complete_trait) <- function(trait, obj, obj_name) {}

method(trait_absent_message, complete_trait) <- function(
  trait,
  obj,
  obj_name
) {
  missing_at <- vctrs::vec_detect_missing(obj)
  obj_vctrs_type <- vec_vctrs_type(obj)
  if (obj_vctrs_type == "bare_dataframe") {
    rows <- fmt_locs(missing_at)
    n <- length(missing_at)
    footer <- "{qty(n)}Row{?s} <<rows>> of {.arg {obj_name}} {qty(n)}contain{?s/} only missing values."
  } else {
    bad <- if (obj_vctrs_type == "bare_list") "{.val {NULL}}" else "{.val {NA}}"
    at <- fmt_at_locs(missing_at)
    footer <- "{.arg {obj_name}} is <<bad>> <<at>>."
  }

  c(
    i = format_styled("{.arg {obj_name}} must not contain missing elements."),
    x = format_styled(footer)
  )
}

# method(trait_validate, is_vector_trait) <- function(trait, obj, obj_name) {}

method(trait_absent_message, is_vector_trait) <- function(
  trait,
  obj,
  obj_name
) {
  c(
    i = format_styled("{.arg {obj_name}} must be a {.pkg vctrs} style vector."),
    x = format_styled("{.arg {obj_name}} is <<fmt_r_type(obj)>>.")
  )
}

# method(trait_validate, is_atomic_trait) <- function(trait, obj, obj_name) {}

method(trait_absent_message, is_atomic_trait) <- function(
  trait,
  obj,
  obj_name
) {
  c(
    i = format_styled("{.arg {obj_name}} must be a bare atomic vector."),
    x = format_styled("{.arg {obj_name}} is <<fmt_r_type(obj)>>.")
  )
}

# trait_present_string ---------------------------------------------------------

method(trait_present_string, bare_typed_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is a bare {.cls {trait@typeof}}.")
}

method(trait_present_string, sized_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is size {trait@size}.")
}

method(trait_present_string, complete_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} contains no missing values.")
}

method(trait_present_string, is_vector_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is a {.pkg vctrs} style vector.")
}

method(trait_present_string, is_atomic_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is a bare atomic vector.")
}

# trait_test_inline/validate_inline --------------------------------------------

# method(trait_validate_inline, bare_typed_trait) <- function(trait, obj_sym, obj_name) {}

method(trait_test_inline, bare_typed_trait) <- function(trait, obj_sym) {
  typeof_expr <- switch(
    trait@typeof,
    double = rlang::expr(base::is.double(!!obj_sym)),
    integer = rlang::expr(base::is.integer(!!obj_sym)),
    logical = rlang::expr(base::is.logical(!!obj_sym)),
    character = rlang::expr(base::is.character(!!obj_sym)),
    complex = rlang::expr(base::is.complex(!!obj_sym)),
    raw = rlang::expr(base::is.raw(!!obj_sym)),
    list = rlang::expr(base::is.list(!!obj_sym)),
    `NULL` = rlang::expr(base::is.null(!!obj_sym)),
    environment = rlang::expr(base::is.environment(!!obj_sym)),
    symbol = rlang::expr(base::is.symbol(!!obj_sym)),
    pairlist = rlang::expr(base::is.pairlist(!!obj_sym)),
    language = rlang::expr(base::is.language(!!obj_sym)),
    expression = rlang::expr(base::is.expression(!!obj_sym)),
    `S4` = ,
    closure = ,
    special = ,
    builtin = ,
    externalptr = ,
    weakref = ,
    promise = ,
    char = ,
    bytecode = ,
    any = ,
    `...` = ,
    `ANY` = rlang::expr(base::typeof(!!obj_sym) == !!trait@typeof)
  )
    
  rlang::expr(is.object(!!obj_sym) && !!typeof_expr)
}

# method(trait_validate_inline, sized_trait) <- function(trait, obj_sym, obj_name) {}

method(trait_test_inline, sized_trait) <- function(trait, obj_sym) {
  rlang::expr(vctrs::vec_size(!!obj_sym) == !!trait@size)
}

# method(trait_validate_inline, complete_trait) <- function(trait, obj_sym, obj_name) {}

method(trait_test_inline, complete_trait) <- function(trait, obj_sym) {
  rlang::expr(!vctrs::vec_any_missing(!!obj_sym))
}

# method(trait_validate_inline, is_vector_trait) <- function(trait, obj_sym, obj_name) {}

method(trait_test_inline, is_vector_trait) <- function(trait, obj_sym) {
  rlang::expr(vctrs::obj_is_vector(!!obj_sym))
}

# method(trait_validate_inline, is_atomic_trait) <- function(trait, obj_sym, obj_name) {}

method(trait_test_inline, is_atomic_trait) <- function(trait, obj_sym) {
  rlang::expr(base::is.atomic(!!obj_sym))
}
