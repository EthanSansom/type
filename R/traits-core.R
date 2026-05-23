# compilation helpers ----------------------------------------------------------

requires_test_obj_is_vector <- function(parent_type, traits) {
  inherits_type(parent_type, t_vector) ||
    has_trait_class(parent_type, traits, trait_obj_is_vector)
}

# bare_typed -------------------------------------------------------------------

# Methods:
# - trait_name:            Formatting name, used like "<trait<{trait_name}>>".
# - trait_validate:        Returns `NULL` if trait satisfied, otherwise a message, like S7 validator().
# - trait_test:            Test whether an object has this trait.
# - trait_absent_message:  Used for long error messages when an object doesn't have this trait.
# - trait_absent_string:   Used for short error messages when an object doesn't have this trait.
# - trait_present_string:  Describe an object with this trait, as a string.
# - trait_invalid_message: Called on trait generation, to check inputs.
# - trait_inline_rules:    Compiler rules for the trait.

bare_typed <- new_trait("bare_typed", parameters = rlang::pairlist2(typeof = , ))

method(trait_name, trait_class(bare_typed)) <- function(trait) {
  paste0('typeof("', trait@typeof, '")') # typeof("integer")
}

method(trait_test, trait_class(bare_typed)) <- function(trait, obj) {
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

method(trait_absent_message, trait_class(bare_typed)) <- function(
  trait,
  obj,
  obj_name
) {
  obj_name <- cli_escape(obj_name)
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

method(trait_present_string, trait_class(bare_typed)) <- function(
  trait,
  obj_name
) {
  format_styled("{.arg {obj_name}} is a bare {.cls {trait@typeof}}.")
}

# TODO:
method(trait_invalid_message, trait_class(bare_typed)) <- function(trait) {}

bare_typed_inline_rules <- list(
  new_inline_rule(
    validate_factory = custom_inline_trait_validate({
      test_expr <- switch(
        trait@typeof,
        double = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.double(!!obj_sym)
        ),
        integer = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.integer(!!obj_sym)
        ),
        logical = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.logical(!!obj_sym)
        ),
        character = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.character(!!obj_sym)
        ),
        complex = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.complex(!!obj_sym)
        ),
        raw = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.raw(!!obj_sym)
        ),
        list = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.list(!!obj_sym)
        ),
        `NULL` = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.null(!!obj_sym)
        ),
        environment = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.environment(!!obj_sym)
        ),
        symbol = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.symbol(!!obj_sym)
        ),
        pairlist = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.pairlist(!!obj_sym)
        ),
        language = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.language(!!obj_sym)
        ),
        expression = rlang::expr(
          !base::is.object(!!obj_sym) && base::is.expression(!!obj_sym)
        ),
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
        `ANY` = rlang::expr(
          !base::is.object(!!obj_sym) &&
            base::typeof(!!obj_sym) == !!trait@typeof
        )
      )
      rlang::expr(
        if (!(!!test_expr)) trait_absent_message(!!trait, !!obj_sym, !!obj_name)
      )
    }),
    requires = requires_test_obj_is_vector
  )
)

method(trait_inline_rules, trait_class(bare_typed)) <- function(trait) {
  bare_typed_inline_rules
}

# classed ----------------------------------------------------------------------

# TODO: Make `inherits` consistent with `relation`, you can allow some subset however
classed <- new_trait(
  "inherits_trait",
  parameters = rlang::pairlist2(
    classes = ,
    inherits = "all"
  )
)

method(trait_name, trait_class(classed)) <- function(trait) {
  classes <- fmt_asis_collapse(trait@classes, n_elm_max = Inf, sep = "/")
  paste0('classed("', classes, '")') # classed(POSIXct/POSIXt)
}

method(trait_test, trait_class(classed)) <- function(trait, obj) {
  switch(
    trait@inherits,
    all = inherits_all(obj, trait@classes),
    any = inherits(obj, trait@classes),
    only = setequal(class(obj), trait@classes),
    exacty = identical(class(obj), trait@classes)
  )
}

# TODO: Finish this absent message!
method(trait_absent_message, trait_class(classed)) <- function(
  trait,
  obj,
  obj_name
) {
  obj_name <- cli_escape(obj_name)
  inherits <- trait@inherits
  header <- format_styled(switch(
    # TODO: Better errors, we'll want to use vectors for some of these
    inherits,
    all = "{.arg {obj_name}} must inherit all of {.cls {trait@classes}}.",
    any = "{.arg {obj_name}} must inherit at least one of {.cls {trait@classes}}.",
    only = "{.arg {obj_name}} must inherit only classes from {.cls {trait@classes}}.",
    exacty = "{.arg {obj_name}} must have class {.cls {trait@classes}}."
  ))

  obj_classes <- class(obj)
  obj_class_name <- glue::glue("class(", obj_name, ")")
  body <- switch(
    inherits,
    all = bad_all_of_message(obj_class_name, obj_classes, trait@classes),
    any = bad_any_of_message(obj_class_name, obj_classes, trait@classes),
    only = bad_same_set_message(obj_class_name, obj_classes, trait@classes),
    exacty = bad_same_vec_message(obj_class_name, obj_classes, trait@classes)
  )

  c(i = header, body)
}

method(trait_present_string, trait_class(classed)) <- function(
  trait,
  obj_name
) {
  format_styled("{.arg {obj_name}} is a bare {.cls {trait@typeof}}.")
}

# TODO:
method(trait_invalid_message, trait_class(classed)) <- function(trait) {}

# TODO:
method(trait_inline_rules, trait_class(classed)) <- function(trait) {}

# sized ------------------------------------------------------------------------

sized <- new_trait("sized", parameters = rlang::pairlist2(size = , ))

method(trait_test, trait_class(sized)) <- function(trait, obj) {
  vctrs::obj_is_vector(obj) && vctrs::vec_size(obj) == trait@size
}

method(trait_absent_message, trait_class(sized)) <- function(
  trait,
  obj,
  obj_name
) {
  obj_name <- cli_escape(obj_name)
  if (!vctrs::obj_is_vector(obj)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must be a {.pkg vctrs} style vector of size {trait@size}."
      ),
      x = format_styled("{.arg {obj_name}} is <<fmt_r_type(obj)>>.")
    ))
  }

  if (vec_vctrs_type(obj) == "bare_dataframe") {
    must <- "have {.val {trait@size}} rows"
    not <- "{.val {vctrs::vec_size(obj)}} rows"
  } else {
    must <- "be size {.val {trait@size}}"
    not <- "size {.val {vctrs::vec_size(obj)}}"
  }
  c(x = format_styled("{.arg {obj_name}} must <<must>>, not <<not>>."))
}

method(trait_present_string, trait_class(sized)) <- function(trait, obj_name) {
  format_styled("{.arg {obj_name}} is size {trait@size}.")
}

# TODO:
method(trait_invalid_message, trait_class(sized)) <- function(trait) {}

sized_inline_rules <- list(
  new_inline_rule(
    inline_trait_validate(vctrs::vec_size(!!obj_sym) == !!trait@size),
    requires = requires_test_obj_is_vector
  ),
  new_inline_rule(
    inline_trait_validate(
      vctrs::obj_is_vector(!!obj_sym) &&
        vctrs::vec_size(!!obj_sym) == !!trait@size
    )
  )
)

method(trait_inline_rules, trait_class(sized)) <- function(trait) {
  sized_inline_rules
}

# complete ---------------------------------------------------------------------

# TODO: Set a size threshold (globally) for these kinds of location checks, e.g.
# `vctrs::vec_detect_missing(obj)`, beyond which we just report the presence of
# "bad" elements and not their locations or count.

complete <- new_trait("complete")

method(trait_test, trait_class(complete)) <- function(trait, obj) {
  vctrs::obj_is_vector(obj) && !vctrs::vec_any_missing(obj)
}

method(trait_absent_message, trait_class(complete)) <- function(
  trait,
  obj,
  obj_name
) {
  obj_name <- cli_escape(obj_name)
  if (!vctrs::obj_is_vector(obj)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must be a vector containing no missing elements."
      ),
      x = format_styled("{.arg {obj_name}} is <<fmt_r_type(obj)>>.")
    ))
  }

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

method(trait_present_string, trait_class(complete)) <- function(
  trait,
  obj_name
) {
  format_styled("{.arg {obj_name}} contains no missing values.")
}

method(trait_invalid_message, trait_class(complete)) <- function(trait) {
  NULL
}

complete_inline_rules <- list(
  new_inline_rule(
    inline_trait_validate(!vctrs::vec_any_missing(!!obj_sym)),
    requires = requires_test_obj_is_vector
  ),
  new_inline_rule(
    inline_trait_validate(
      vctrs::obj_is_vector(!!obj_sym) && !vctrs::vec_any_missing(!!obj_sym)
    )
  )
)

method(trait_inline_rules, trait_class(complete)) <- function(trait) {
  complete_inline_rules
}

# unduplicated -----------------------------------------------------------------

unduplicated <- new_trait("unduplicated")

method(trait_test, trait_class(unduplicated)) <- function(trait, obj) {
  vctrs::obj_is_vector(obj) && !vctrs::vec_duplicate_any(obj)
}

method(trait_absent_message, trait_class(unduplicated)) <- function(
  trait,
  obj,
  obj_name
) {
  obj_name <- cli_escape(obj_name)
  if (!vctrs::obj_is_vector(obj)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must be a vector with distinct elements."
      ),
      x = format_styled("{.arg {obj_name}} is <<fmt_r_type(obj)>>.")
    ))
  }

  duplicated_at <- vctrs::vec_duplicate_detect(obj)
  if (vec_vctrs_type(obj) == "bare_dataframe") {
    at <- fmt_at_locs(duplicated_at, "row")
  } else {
    at <- fmt_at_locs(duplicated_at, "location")
  }
  c(
    i = format_styled("{.arg {obj_name}} must not contain duplicate elements."),
    x = format_styled("{.arg {obj_name}} contains duplicate elements <<at>>.")
  )
}

method(trait_present_string, trait_class(unduplicated)) <- function(
  trait,
  obj_name
) {
  format_styled("{.arg {obj_name}} contains no duplicated values.")
}

method(trait_invalid_message, trait_class(unduplicated)) <- function(trait) {
  NULL
}


unduplicated_inline_rules <- list(
  new_inline_rule(
    inline_trait_validate(!vctrs::vec_duplicate_any(!!obj_sym)),
    requires = requires_test_obj_is_vector
  ),
  new_inline_rule(
    inline_trait_validate(
      vctrs::obj_is_vector(!!obj_sym) && !vctrs::vec_duplicate_any(!!obj_sym)
    )
  )
)

method(trait_inline_rules, trait_class(unduplicated)) <- function(trait) {
  unduplicated_inline_rules
}

# unexported -------------------------------------------------------------------

# These are required for some named types, but are not generally useful.

## trait_obj_is_vector ---------------------------------------------------------

trait_obj_is_vector <- new_trait("trait_obj_is_vector")

method(trait_test, trait_class(trait_obj_is_vector)) <- function(trait, obj) {
  vctrs::obj_is_vector(obj)
}

method(trait_absent_message, trait_class(trait_obj_is_vector)) <- function(
  trait,
  obj,
  obj_name
) {
  obj_name <- cli_escape(obj_name)
  c(
    i = format_styled("{.arg {obj_name}} must be a {.pkg vctrs} style vector."),
    x = format_styled("{.arg {obj_name}} is <<fmt_r_type(obj)>>.")
  )
}

method(trait_present_string, trait_class(trait_obj_is_vector)) <- function(
  trait,
  obj_name
) {
  format_styled("{.arg {obj_name}} is a {.pkg vctrs} style vector.")
}

# TODO:
method(trait_invalid_message, trait_class(trait_obj_is_vector)) <- function(
  trait
) {}

# TODO:
method(trait_inline_rules, trait_class(trait_obj_is_vector)) <- function(
  trait
) {}

## trait_obj_is_atomic ---------------------------------------------------------

trait_obj_is_atomic <- new_trait("trait_obj_is_atomic")

method(trait_test, trait_class(trait_obj_is_atomic)) <- function(trait, obj) {
  !is.object(obj) && is.atomic(obj)
}

method(trait_absent_message, trait_class(trait_obj_is_atomic)) <- function(
  trait,
  obj,
  obj_name
) {
  obj_name <- cli_escape(obj_name)
  c(
    i = format_styled("{.arg {obj_name}} must be a bare atomic vector."),
    x = format_styled("{.arg {obj_name}} is <<fmt_r_type(obj)>>.")
  )
}

method(trait_present_string, trait_class(trait_obj_is_atomic)) <- function(
  trait,
  obj_name
) {
  format_styled("{.arg {obj_name}} is a bare atomic vector.")
}

# TODO:
method(trait_invalid_message, trait_class(trait_obj_is_atomic)) <- function(
  trait
) {}

# TODO:
method(trait_inline_rules, trait_class(trait_obj_is_atomic)) <- function(
  trait
) {}
