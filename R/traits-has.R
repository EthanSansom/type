# has --------------------------------------------------------------------------

# TODO: Uses `on()` to determine which trait to defer to
has <- function(.type = t_any, on, on_type) {}

# has_names/has_columns/has_attrs ----------------------------------------------

has_names <- function(.type = t_any, names, match = "all") {
  assert_is_type(.type)
  assert_is_chr(names, complete = TRUE)
  match <- normalize_relation(match)
  trait_has_elms(.type = .type, elms = names, which_elm = "name", match = match)
}

has_attrs <- function(.type = t_any, attrs, match = "all") {
  assert_is_type(.type)
  assert_is_chr(attrs, complete = TRUE)
  match <- normalize_relation(match)
  trait_has_elms(.type = .type, elms = attrs, which_elm = "attr", match = match)
}

has_cols <- function(.type = t_any, cols, match = "all") {
  assert_is_type(.type)
  assert_is_chr(cols, complete = TRUE)
  match <- normalize_relation(match)

  trait_has_elms(
    .type = classed(.type, "data.frame"), # Enforce "data.frame" inheritance
    elms = cols,
    which_elm = "col",
    match = match
  )
}

trait_has_elms <- new_trait(
  "trait_has_elms",
  parameters = rlang::pairlist2(
    elms = ,
    match = ,
    which_elm = ,
  )
)

method(trait_name, trait_class(trait_has_elms)) <- function(trait) {
  fmt <- elms_format(trait@which_elm)
  elms_str <- fmt_asis_collapse(fmt$fmt(trait@elms))
  what <- abbr_relation(trait@match)

  # `has_all_names("x", "y")`, `has_no_columns(`bad`, `worse`)`
  glue::glue("has_{what}_{fmt$label}s({elms_str})")
}

method(trait_test, trait_class(trait_has_elms)) <- function(trait, obj) {
  test_vec_set_relation(
    obj = switch(
      trait@which_elm,
      name = ,
      col = names(obj) %||% character(),
      attr = names(attributes(obj)) %||% character()
    ),
    vec = trait@elms,
    relation = trait@match
  )
}

method(trait_validate, trait_class(trait_has_elms)) <- function(
  trait,
  obj,
  obj_name
) {
  validate_vec_set_relation(
    obj = switch(
      trait@which_elm,
      name = ,
      col = names(obj) %||% character(),
      attr = names(attributes(obj)) %||% character()
    ),
    obj_name = obj_name,
    vec = trait@elms,
    relation = trait@match,
    which_elm = trait@which_elm
  )
}

method(trait_absent_message, trait_class(trait_has_elms)) <- function(
  trait,
  obj,
  obj_name
) {
  trait_validate(trait, obj, obj_name)
}

method(trait_present_string, trait_class(trait_has_elms)) <- function(
  trait,
  obj_name
) {
  fmt <- elms_format(trait@which_elm)
  elms_str <- commas(fmt$fmt(trait@elms))

  switch(
    trait@match,
    superset_of = format_styled(
      "{.arg {obj_name}} {fmt$label}s include: <<elms_str>>."
    ),
    intersects_with = format_styled(
      "{.arg {obj_name}} {fmt$label}s include at least one of: <<elms_str>>."
    ),
    one_of = format_styled(
      "{.arg {obj_name}} {fmt$label}s include exactly one of: <<elms_str>>."
    ),
    subset_of = format_styled(
      "{.arg {obj_name}} {fmt$label}s are a subset of: <<elms_str>>."
    ),
    disjoint_to = format_styled(
      "{.arg {obj_name}} {fmt$label}s do not include any of: <<elms_str>>."
    ),
    setequal_to = format_styled(
      "{.arg {obj_name}} has all of the {fmt$label}s: <<elms_str>>."
    ),
    same_as = format_styled(
      "{.arg {obj_name}} has exactly the {fmt$label}s, in order: <<elms_str>>."
    ),
    perm_of = format_styled(
      "{.arg {obj_name}} has exactly the {fmt$label}s, in any order: <<elms_str>>."
    )
  )
}

# has_on_elms ------------------------------------------------------------------

has_on_names <- function(.type = t_any, ...) {
  has_on_elms(.type = .type, ..., which_elm = "name")
}

has_on_attrs <- function(.type = t_any, ...) {
  has_on_elms(.type = .type, ..., which_elm = "attr")
}

# TODO: Attatch a `is.data.frame` trait
has_on_cols <- function(.type = t_any, ...) {
  has_on_elms(.type = .type, ..., which_elm = "col")
}

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

# has_on_each ------------------------------------------------------------------

has_on_each <- new_trait(
  "has_on_each",
  parameters = rlang::pairlist2(what = , what_type = , )
)

# has_on_custom ----------------------------------------------------------------

has_on_custom <- new_trait("has", parameters = rlang::pairlist2(on = , on_type = , ))


# TODO:

method(trait_test, trait_class(has)) <- function(trait, obj) {
  obj_is_type(trait@on@accessor(obj), trait@on_type)
}

method(trait_absent_message, trait_class(has)) <- function(
  trait,
  obj,
  obj_name
) {
  type_absent_message(
    type = trait@on_type,
    obj = trait@on@accessor(obj),
    obj_name = trait@on@name_fun(obj, obj_name)
  )
}

method(trait_absent_string, trait_class(has)) <- function(
  trait,
  obj,
  obj_name
) {
  type_absent_string(
    type = trait@on_type,
    obj = trait@on@accessor(obj),
    obj_name = trait@on@name_fun(obj, obj_name)
  )
}

# on ---------------------------------------------------------------------------

new_on <- S7::new_class(
  "on",
  package = "type",
  properties = list(
    accessor = S7::class_any, # class_function
    name_fun = S7::class_any # class_function
  )
)

# TODO: Validation -> User facing
on <- function(accessor, name_str = NULL, name_fun = NULL) {
  if (is.null(name_fun)) {
    force(name_str)
    name_fun <- \(obj, obj_name) {
      gsub("obj_name", obj_name, name_str, fixed = TRUE)
    }
  }
  new_on(
    accessor = accessor,
    name_fun = name_fun
  )
}

# helpers ----------------------------------------------------------------------

access_elm <- function(obj, elm, which_elm) {
  switch(
    which_elm,
    name = ,
    col = if (elm %in% names(obj)) obj[[elm]],
    attr = attr(obj, elm, exact = TRUE)
  )
}

elm_obj_name <- function(obj_name, elm_name, which_elm) {
  switch(
    which_elm,
    name = ,
    col = glue::glue("{obj_name}[[{chr_encode(elm_name)}]]"),
    attr = glue::glue("attr({obj_name}, {chr_encode(elm_name)})")
  )
}
