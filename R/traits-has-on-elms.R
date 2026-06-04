# wrappers ---------------------------------------------------------------------

has_on_names <- function(.type = t_any, ...) {
  has_on_elms(.type = .type, ..., which_elm = "name")
}

has_on_attrs <- function(.type = t_any, ...) {
  has_on_elms(.type = .type, ..., which_elm = "attr")
}

has_on_cols <- function(.type = t_any, ...) {
  has_on_elms(
    .type = .type |> classed("data.frame"),
    ..., 
    which_elm = "col"
  )
}

# implementation ---------------------------------------------------------------

has_on_elms <- function(
  .type,
  ...,
  which_elm = NULL,
  error_call = rlang::caller_env()
) {
  # TODO:
  # 1. Parse `key = type` pairs, check that type is a <type>
  # 2. Add trait `has_names(names = keys, "all")`
  # 3. Add trait `has_on_elm(key, types[key])` for each type
}

if (FALSE) {
  trait_has_on_elm <- new_trait(
    "trait_has_on_elm",
    parameters = rlang::pairlist2(
      elm = ,
      elm_type = ,
      which_elm = ,
    )
  )

  # TODO: has_typed_attr("attr"), has_typed_col(`col`), has_typed_elm("name")
  method(trait_name, trait_class(trait_has_on_elm)) <- function(trait) {
    "has_on_elm"
  }

  # `trait_has_on_elm` is only applied after `elm` is verified to exist
  method(trait_validate, trait_class(trait_has_on_elm)) <- function(
    trait,
    obj,
    obj_name
  ) {
    target_type <- trait@elm_type
    elm <- trait@elm
    elm_value <- access_elm(obj, elm, trait@which_elm)
    if (!type_test(target_type, elm_value)) {
      elm_name <- elm_obj_name(obj_name, elm, trait@which_elm)
      return(type_absent_message(target_type, elm_value, elm_name))
    }
  }

  method(trait_test, trait_class(trait_has_on_elm)) <- function(trait, obj) {
    type_test(trait@elm_type, access_elm(obj, trait@elm, trait@which_elm))
  }

  method(trait_absent_message, trait_class(trait_has_on_elm)) <- function(
    trait,
    obj,
    obj_name
  ) {
    trait_validate(trait, obj, obj_name)
  }

  # TODO: Defer to a `type_present_string` which will likely look like:
  # `obj_name` is an <integer> with traits: sized(1L).
  method(trait_present_string, trait_class(trait_has_on_elm)) <- function(
    trait,
    obj_name
  ) {}
}
