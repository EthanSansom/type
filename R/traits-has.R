# has --------------------------------------------------------------------------

# TODO: Uses `on()` to determine which trait to defer to
has <- function(.type = t_any, on, on_type) {}

# has_on_custom ----------------------------------------------------------------

if (FALSE) {
has_on_custom <- new_trait(
  "has", 
  parameters = rlang::pairlist2(on = , on_type = , )
)

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
