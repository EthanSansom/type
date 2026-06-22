# sized ------------------------------------------------------------------------

# TODO: Document
#' @export
sized <- function(type, size) {
  assert_is_type(type)
  assert_is_count(size)

  if (!is_bare_vector_type(type)) {
    type <- type |> add_trait(vector_trait())
  }
  type |>
    add_trait(sized_trait(size = size))
}

sized_trait <- new_trait("sized", params = c("size"))

method(trait_test, sized_trait) <- function(trait, obj) {
  vctrs::vec_size(obj) == trait@size
}

method(trait_diagnose, sized_trait) <- function(trait, obj, obj_name) {
  c(x = format_styled("{.arg {obj_name}} must be size {trait@size}, not size {vctrs::vec_size(obj)}."))
}

method(trait_describe, sized_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is size {trait@size}.")
}

# bare_typed -------------------------------------------------------------------

# TODO: Document
#' @export
bare_typed <- function(type, typeof) {
  assert_is_type(type)
  assert_is_string(typeof)

  valid_typeof <- c(
    "double", "integer", "logical", "character", "complex", "raw", "list", "NULL",
    "environment", "symbol", "pairlist", "language", "expression", "S4", "closure",
    "special", "builtin", "externalptr", "weakref", "promise", "char", "bytecode"
  )
  if (typeof %notin% valid_typeof) {
    supported_types <- fmt_vec_collapse(valid_typeof, n_elm_max = Inf, n_chr_max = Inf)
    abort_bad_input(
      c(
        x = format_styled("{.arg typeof} must be a valid R type, not {.val {typeof}}."),
        i = format_styled("Supported types are: <<supported_types>>.")
      )
    )
  }

  type |>
    add_trait(bare_typed_trait(typeof = typeof))
}

bare_typed_trait <- new_trait("bare_typed", params = c("typeof"))

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
      bytecode = typeof(obj) == "bytecode"
    )
}

method(trait_diagnose, bare_typed_trait) <- function(trait, obj, obj_name) {
  if (is.object(obj)) {
    c(
      i = format_styled("{.arg {obj_name}} must be a bare {.cls {trait@typeof}}."),
      x = format_styled(
      "{.arg {obj_name}} is a {obj_oo_type(obj)} object of class {.cls {class(obj)}}."
      )
    )
  } else {
    c(
      x = format_styled(
        "{.arg {obj_name}} must be a bare {.cls {trait@typeof}}, ", 
        "not a bare {.cls {typeof(obj)}}."
      )
    )
  }
}

method(trait_describe, bare_typed_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is a bare {.cls {trait@typeof}}.")
}

# vector -----------------------------------------------------------------------

vector_trait <- new_trait("vector")

method(trait_test, vector_trait) <- function(trait, obj) {
  vctrs::obj_is_vector(obj)
}

method(trait_diagnose, vector_trait) <- function(trait, obj, obj_name) {
  c(
    x = format_styled(
      "{.arg {obj_name}} must be a {.pkg vctrs} style vector, ",
      "not <<fmt_r_type(obj)>>."
    )
  )
}

method(trait_describe, vector_trait) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is a {.pkg vctrs} style vector.")
}

# helpers ----------------------------------------------------------------------

is_bare_vector_type <- function(type) {
  vector_types <- c("double", "integer", "logical", "character", "complex", "raw", "list")
  any(map_lgl(
    type@traits, 
    \(trait) S7::S7_inherits(trait, bare_typed_trait) && trait@typeof %in% vector_types
  ))
}
