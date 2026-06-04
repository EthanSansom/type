# classes ----------------------------------------------------------------------

trait <- S7::new_class("trait", package = "type", abstract = TRUE)

# package constructors ---------------------------------------------------------

new_trait_class <- function(
  name,
  params,
  package = "type"
) {
  S7::new_class(
    name = name,
    parent = trait,
    package = package,
    properties = rlang::rep_named(params, list(S7::class_any))
  )
}

# package methods --------------------------------------------------------------

trait_name <- S7::new_generic(
  "trait_name", 
  c("trait")
)

# method(trait_name, trait) <- function(trait) {}

trait_test <- S7::new_generic(
  "trait_test",
  c("trait"),
  function(trait, obj, ...) {
    S7::S7_dispatch()
  }
)

# method(trait_test, trait) <- function(trait, obj) {}

trait_absent_message <- S7::new_generic(
  "trait_absent_message",
  c("trait"),
  function(trait, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

# method(trait_absent_message, trait) <- function(trait, obj, obj_name) {}

trait_present_string <- S7::new_generic(
  "trait_present_string",
  c("trait"),
  function(trait, obj_name, ...) {
    S7::S7_dispatch()
  }
)

# method(trait_present_string, trait) <- function(trait, obj_name) {}

trait_validate <- S7::new_generic(
  "trait_validate",
  c("trait"),
  function(trait, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

method(trait_validate, trait) <- function(trait, obj, obj_name) {
  if (rlang::is_true(trait_test(trait, obj))) {
    NULL
  } else {
    trait_absent_message(trait, obj, obj_name)
  }
}

trait_validate_inline <- S7::new_generic(
  "trait_validate_inline", 
  c("trait"),
  function(trait, obj_sym, obj_name, ...) {
    S7::S7_dispatch()
  }
)

trait_test_inline <- S7::new_generic(
  "trait_test_inline", 
  c("trait"),
  function(trait, obj_sym, ...) {
    S7::S7_dispatch()
  }
)

method(trait_test_inline, trait) <- function(trait, obj_sym, ...) {
  NULL
}

method(trait_validate_inline, trait) <- function(trait, obj_sym, obj_name, ...) {
  test_expr <- trait_test_inline(trait, obj_sym)
  if (rlang::is_zap(test_expr)) {
    rlang::zap() # Indicates that the trait can be dropped (i.e. is redundant) 
  } else if (is.null(test_expr)) {
    rlang::call2("trait_validate", trait, obj_sym, obj_name, .ns = "type")
  } else {
    message_expr <- rlang::call2("trait_absent_message", trait, obj_sym, obj_name, .ns = "type")
    rlang::expr(if (!(!!test_expr)) !!message_expr)
  }
}
