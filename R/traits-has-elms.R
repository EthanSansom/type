# wrappers ---------------------------------------------------------------------

has_names <- function(.type = t_any, names, match = "all") {
  assert_is_type(.type)
  assert_is_chr(names, complete = TRUE)
  match <- normalize_relation(match)

  trait_has_elms(
    .type = .type, 
    elms = names, 
    which_elm = "name", 
    match = match
  )
}

has_attrs <- function(.type = t_any, attrs, match = "all") {
  assert_is_type(.type)
  assert_is_chr(attrs, complete = TRUE)
  match <- normalize_relation(match)

  trait_has_elms(
    .type = .type, 
    elms = attrs, 
    which_elm = "attr", 
    match = match
  )
}

has_cols <- function(.type = t_any, cols, match = "all") {
  assert_is_type(.type)
  assert_is_chr(cols, complete = TRUE)
  match <- normalize_relation(match)

  trait_has_elms(
    .type = .type |> classed("data.frame"),
    elms = cols,
    which_elm = "col",
    match = match
  )
}

# implementation ---------------------------------------------------------------

if (FALSE) {
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
    permutation_of = format_styled(
      "{.arg {obj_name}} has exactly the {fmt$label}s, in any order: <<elms_str>>."
    )
  )
}
}

