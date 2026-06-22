trait <- S7::new_class("trait", package = "type", abstract = TRUE)

is_trait <- function(x) {
  S7::S7_inherits(x, trait)
}

new_trait <- function(
  name,
  params = character(),
  package = "type"
) {
  S7::new_class(
    name = name,
    parent = trait,
    package = package,
    properties = rlang::rep_named(params, list(S7::class_any))
  )
}

# generics ---------------------------------------------------------------------

trait_test <- S7::new_generic(
  "trait_test",
  c("trait"),
  function(trait, obj, ...) {
    S7::S7_dispatch()
  }
)

trait_diagnose <- S7::new_generic(
  "trait_diagnose",
  c("trait"),
  function(trait, obj, obj_name, ...) {
    S7::S7_dispatch()
  }
)

trait_describe <- S7::new_generic(
  "trait_describe",
  c("trait"),
  function(trait, obj_name, ...) {
    S7::S7_dispatch()
  }
)
