# has --------------------------------------------------------------------------

# TODO: Document
#' @export
has <- function(type, selector, on_type) {
  context_local("has")
  assert_is_type(type)
  assert_is_selector(selector)
  assert_is_type(on_type)
  type |> add_trait(has_on_trait(selector = selector, on_type = on_type))
}

has_on_trait <- new_trait("has_on", params = c("selector", "on_type"))

method(trait_test, has_on_trait) <- function(trait, obj) {
  selector <- trait@selector
  on_type <- trait@on_type
  value <- rlang::try_fetch(selector@accessor(obj), error = identity)
  if (rlang::is_error(value)) return(FALSE)

  if (selector@plural) {
    if (length(value) == 0L) return(TRUE)
    return(all(map_lgl(value, \(v) type_test(on_type, v))))
  }
  type_test(on_type, value)
}

method(trait_diagnose, has_on_trait) <- function(trait, obj, obj_name) {
  selector <- trait@selector
  on_type <- trait@on_type
  value <- rlang::try_fetch(selector@accessor(obj), error = identity)

  if (rlang::is_error(value)) {
    label <- selector@labeller(obj_name, obj)
    return(c(
      x = format_styled("{label} must return a value, not raise an error.")
    ))
  }

  if (selector@plural) {
    labels <- selector@labeller(obj_name, obj)
    for (i in seq_along(value)) {
      if (!type_test(on_type, value[[i]])) {
        return(type_diagnose(on_type, value[[i]], untick(labels[[i]])))
      }
    }
  } else {
    type_diagnose(on_type, value, untick(selector@labeller(obj_name, obj)))
  }
}

method(trait_describe, has_on_trait) <- function(trait, obj_name) {
  selector <- trait@selector
  on_type <- trait@on_type

  if (selector@plural) {
    return(paste("Every element of", str_lower1(type_describe(trait@on_type, obj_name))))
  }
  type_describe(on_type, untick(selector@labeller(obj_name, NULL)))
}
