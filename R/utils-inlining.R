type_validate_expr <- function(type, obj_sym, obj_name) {
  if (is_named_type(type)) {
    parent_type <- type
    parent_traits <- type@traits
    refinement_traits <- list()
  } else {
    parent_type <- type@parent_type
    parent_traits <- parent_type@traits
    refinement_traits <- type@traits
  }

  # If an inline validation expression is available for the `parent_type` as
  # a whole, we use it. Otherwise, its constituent traits are inlined instead.
  type_validate_expr <- type_validate_inline(parent_type, obj_sym, obj_name)
  if (is.null(type_validate_expr)) {
    validate_exprs <- list()
    traits_to_inline <- c(parent_traits, refinement_traits)
  } else {
    validate_exprs <- list(type_validate_expr)
    traits_to_inline <- refinement_traits
  }

  validate_exprs <- c(
    validate_exprs,
    map(
      traits_to_inline, 
      function(trait) trait_validate_inline(trait, obj_sym, obj_name)
    )
  )

  # Bypass lazy evaluation when performing a single validation
  if (length(validate_exprs) == 1L) {
    return(validate_exprs[[1L]])
  }

  rlang::call2("inlined_lazy_validate", !!!validate_exprs, .ns = "type")
}
