# dispatch ---------------------------------------------------------------------

# We expect `normalize_relation()` to have been called prior to test/validating.

test_vec_set_relation <- function(obj, vec, relation) {
  switch(
    relation,
    superset_of = test_is_superset_of(obj, vec),
    intersects_with = test_intersects_with(obj, vec),
    one_of = test_contains_one_of(obj, vec),
    subset_of = test_is_subset_of(obj, vec),
    disjoint_to = test_is_disjoint_to(obj, vec),
    setequal_to = test_is_setequal_to(obj, vec),
    same_as = test_is_same_as(obj, vec),
    permutation_of = test_is_permutation_of(obj, vec),
    type_abort_internal(format_styled("Unexpected relation: {relation}."))
  )
}

validate_vec_set_relation <- function(
  obj,
  vec,
  relation,
  obj_name,
  which_elm = NULL
) {
  switch(
    relation,
    superset_of = validate_is_superset_of(obj, vec, obj_name, which_elm),
    intersects_with = validate_intersects_with(obj, vec, obj_name, which_elm),
    one_of = validate_contains_one_of(obj, vec, obj_name, which_elm),
    subset_of = validate_is_subset_of(obj, vec, obj_name, which_elm),
    disjoint_to = validate_is_disjoint_to(obj, vec, obj_name, which_elm),
    setequal_to = validate_is_setequal_to(obj, vec, obj_name, which_elm),
    same_as = validate_is_same_as(obj, vec, obj_name, which_elm),
    permutation_of = validate_is_permutation_of(obj, vec, obj_name, which_elm),
    type_abort_internal(format_styled("Unexpected relation: {relation}."))
  )
}

normalize_relation <- function(
  relation,
  relation_name = caller_arg(relation),
  error_call = rlang::caller_env()
) {
  switch(
    relation,
    all = ,
    all_of = ,
    superset_of = "superset_of",
    any = ,
    any_of = ,
    intersects_with = "intersects_with",
    one_of = "one_of",
    subset_of = "subset_of",
    none = ,
    none_of = ,
    disjoint_to = "disjoint_to",
    only = ,
    setequal_to = "setequal_to",
    exact = ,
    same_as = "same_as",
    perm_of = ,
    permutation_of = "permutation_of",
    {
      valid_relations <- c(
        '"all" / "all_of" / "superset_of"',
        '"any" / "any_of" / "intersects_with"',
        '"one_of"',
        '"subset_of"',
        '"none" / "none_of" / "disjoint_to"',
        '"only" / "setequal_to"',
        '"exact" / "same_as"',
        '"perm_of" / "permutation_of"'
      )
      type_abort_bad_input(
        c(
          format_styled("{.arg {relation_name}} must be one of:"),
          rlang::set_names(valid_relations, "*"),
          x = format_styled("{.arg {relation_name}} is {.val {relation}}.")
        ),
        error_call = error_call
      )
    }
  )
}

abbr_relation <- function(relation) {
  switch(
    relation,
    all = ,
    all_of = ,
    superset_of = "all",
    any = ,
    any_of = ,
    intersects_with = "any",
    one_of = "one_of",
    subset_of = "some",
    none = ,
    none_of = ,
    disjoint_to = "no",
    only = ,
    setequal_to = "only",
    exact = ,
    same_as = "same",
    perm_of = ,
    permutation_of = "perm_of",
    type_abort_internal(format_styled("Unexpected relation: {relation}."))
  )
}

# test -------------------------------------------------------------------------

test_is_superset_of <- function(obj, vec) {
  all(vctrs::vec_in(vec, obj))
}

test_intersects_with <- function(obj, vec) {
  any(vctrs::vec_in(vec, obj))
}

test_contains_one_of <- function(obj, vec) {
  sum(vctrs::vec_in(vec, obj)) == 1L
}

test_is_subset_of <- function(obj, vec) {
  all(vctrs::vec_in(obj, vec))
}

test_is_disjoint_to <- function(obj, vec) {
  !any(vctrs::vec_in(vec, obj))
}

test_is_setequal_to <- function(obj, vec) {
  test_is_superset_of(obj, vec) && test_is_subset_of(obj, vec)
}

test_is_same_as <- function(obj, vec) {
  vctrs::vec_size(obj) == vctrs::vec_size(vec) &&
    all(vctrs::vec_equal(obj, vec, na_equal = TRUE))
}

test_is_permutation_of <- function(obj, vec) {
  vctrs::vec_size(obj) == vctrs::vec_size(vec) && test_is_setequal_to(obj, vec)
}

# validation -------------------------------------------------------------------

validate_is_superset_of <- function(obj, vec, obj_name, which_elm = NULL) {
  in_obj <- vctrs::vec_in(vec, obj)
  if (all(in_obj)) {
    return(NULL)
  }

  bad_is_superset_of_message(
    obj = obj,
    obj_name = obj_name,
    vec = vec,
    bad_vec = vec[!in_obj],
    which_elm = which_elm
  )
}

bad_is_superset_of_message <- function(
  obj,
  vec,
  bad_vec,
  obj_name,
  which_elm = NULL
) {
  n_vec <- vctrs::vec_size(vec)
  if (is.null(which_elm)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must contain {qty(n_vec)}element{?s}: <<fmt_vec_string(vec)>>."
      ),
      x = bullet_missing_vec(obj_name, bad_vec)
    ))
  }

  fmt <- elms_format(which_elm)
  c(
    i = format_styled(
      "{.arg {obj_name}} must have {qty(n_vec)}{?the/every} <<fmt$have>> in: ",
      "<<fmt_asis_collapse(fmt$fmt(vec))>>."
    ),
    x = bullet_missing_elms(obj_name, bad_vec, which_elm)
  )
}

validate_intersects_with <- function(obj, vec, obj_name, which_elm = NULL) {
  if (any(vctrs::vec_in(vec, obj))) {
    return(NULL)
  }

  bad_intersects_with_message(
    obj = obj_name,
    obj_name = obj_name,
    vec = vec,
    which_elm = which_elm
  )
}

bad_intersects_with_message <- function(obj_name, obj, vec, which_elm = NULL) {
  if (is.null(which_elm)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must contain at least one of: <<fmt_vec_string(vec)>>."
      ),
      x = bullet_matching_vec(obj_name, NULL)
    ))
  }

  fmt <- elms_format(which_elm)
  c(
    i = format_styled(
      "{.arg {obj_name}} must have at least one <<fmt$have>> from: ",
      "<<fmt_asis_collapse(fmt$fmt(vec))>>."
    ),
    x = bullet_matching_elms(obj_name, NULL, which_elm)
  )
}

validate_contains_one_of <- function(obj, vec, obj_name, which_elm = NULL) {
  in_obj <- vctrs::vec_in(vec, obj)
  if (sum(in_obj) == 1L) {
    return(NULL)
  }

  bad_contains_n_of_message(
    obj = obj,
    obj_name = obj_name,
    n = 1L,
    vec = vec,
    bad_vec = vec[in_obj],
    which_elm = which_elm
  )
}

bad_contains_n_of_message <- function(
  obj,
  vec,
  bad_vec,
  n,
  obj_name,
  which_elm = NULL
) {
  if (is.null(which_elm)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must contain exactly {n} of: <<fmt_vec_string(vec)>>."
      ),
      x = bullet_matching_vec(obj_name, bad_vec)
    ))
  }

  fmt <- elms_format(which_elm)
  c(
    i = format_styled(
      "{.arg {obj_name}} must have exactly {n} <<fmt$have>>{?s} from: ",
      "<<fmt_asis_collapse(fmt$fmt(vec))>>."
    ),
    x = bullet_matching_elms(obj_name, bad_vec, which_elm)
  )
}

validate_is_subset_of <- function(obj, vec, obj_name, which_elm = NULL) {
  in_vec <- vctrs::vec_in(obj, vec)
  if (all(in_vec)) {
    return(NULL)
  }

  bad_subset_of_message(
    obj = obj,
    obj_name = obj_name,
    vec = vec,
    bad_vec = obj[!in_vec],
    which_elm = which_elm
  )
}

bad_subset_of_message <- function(
  obj,
  vec,
  bad_vec,
  obj_name,
  which_elm = NULL
) {
  if (is.null(which_elm)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} may only contain values from: <<fmt_vec_string(vec)>>."
      ),
      x = bullet_unexpected_vec(obj_name, bad_vec)
    ))
  }

  fmt <- elms_format(which_elm)
  c(
    i = format_styled(
      "{.arg {obj_name}} may only have <<fmt$have>>s from: ",
      "<<fmt_asis_collapse(fmt$fmt(vec))>>."
    ),
    x = bullet_unexpected_elms(obj_name, bad_vec, which_elm)
  )
}

validate_is_disjoint_to <- function(obj, vec, obj_name, which_elm = NULL) {
  in_obj <- vctrs::vec_in(vec, obj)
  if (!any(in_obj)) {
    return(NULL)
  }

  bad_none_of_message(
    obj = obj,
    obj_name = obj_name,
    vec = vec,
    bad_vec = vec[in_obj],
    which_elm = which_elm
  )
}

bad_none_of_message <- function(
  obj,
  vec,
  bad_vec,
  obj_name,
  which_elm = NULL
) {
  if (is.null(which_elm)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must not contain any of: <<fmt_vec_string(vec)>>."
      ),
      x = bullet_unexpected_vec(obj_name, bad_vec)
    ))
  }

  fmt <- elms_format(which_elm)
  c(
    i = format_styled(
      "{.arg {obj_name}} must not have any <<fmt$have>>s from: ",
      "<<fmt_asis_collapse(fmt$fmt(vec))>>."
    ),
    x = bullet_unexpected_elms(obj_name, bad_vec, which_elm)
  )
}

validate_is_setequal_to <- function(obj, vec, obj_name, which_elm = NULL) {
  in_obj <- vctrs::vec_in(vec, obj)
  in_vec <- vctrs::vec_in(obj, vec)
  if (all(in_obj) && all(in_vec)) {
    return(NULL)
  }

  bad_is_setequal_to_message(
    obj = obj,
    obj_name = obj_name,
    vec = vec,
    extra_vec = obj[!in_vec],
    missing_vec = vec[!in_obj],
    which_elm = which_elm
  )
}

bad_is_setequal_to_message <- function(
  obj,
  vec,
  missing_vec,
  extra_vec,
  obj_name,
  which_elm = NULL
) {
  n_missing <- vctrs::vec_size(missing_vec)
  n_extra <- vctrs::vec_size(extra_vec)

  if (is.null(which_elm)) {
    return(c(
      i = format_styled(
        "{.arg {obj_name}} must contain exactly: <<fmt_vec_string(vec)>>."
      ),
      x = if (n_missing > 0) bullet_missing_vec(obj_name, missing_vec),
      x = if (n_extra > 0) bullet_unexpected_vec(obj_name, extra_vec)
    ))
  }

  fmt <- elms_format(which_elm)
  c(
    i = format_styled(
      "{.arg {obj_name}} must have exactly these <<fmt$have>>s: ",
      "<<fmt_asis_collapse(fmt$fmt(vec))>>."
    ),
    x = if (n_missing > 0) {
      bullet_missing_elms(obj_name, missing_vec, which_elm)
    },
    x = if (n_extra > 0) bullet_unexpected_elms(obj_name, extra_vec, which_elm)
  )
}

validate_is_same_as <- function(obj, vec, obj_name, which_elm = NULL) {
  obj_size <- vctrs::vec_size(obj)
  vec_size <- vctrs::vec_size(vec)

  if (obj_size != vec_size) {
    return(c(
      x = bullet_unexpected_size(obj_name, obj_size, vec_size, which_elm)
    ))
  }

  is_same <- vctrs::vec_equal(obj, vec, na_equal = TRUE)
  if (all(is_same)) {
    return(NULL)
  }

  bad_is_same_as_message(
    obj = obj,
    obj_name = obj_name,
    vec = vec,
    is_same = is_same,
    which_elm = which_elm
  )
}

bad_is_same_as_message <- function(
  obj,
  vec,
  is_same,
  obj_name,
  which_elm = NULL
) {
  if (is.null(which_elm)) {
    not_same <- !is_same
    return(c(
      i = format_styled("{.arg {obj_name}} must be: <<fmt_vec_string(vec)>>."),
      x = format_styled("{.arg {obj_name}} differs <<fmt_at_locs(not_same)>>:"),
      `*` = format_styled("Actual:   <<fmt_vec_string(obj[not_same])>>"),
      `*` = format_styled("Expected: <<fmt_vec_string(vec[not_same])>>")
    ))
  }

  fmt <- elms_format(which_elm)
  not_same <- !is_same
  c(
    i = format_styled(
      "{.arg {obj_name}} must have these <<fmt$have>>s in order: ",
      "<<fmt_asis_collapse(fmt$fmt(vec))>>."
    ),
    x = format_styled(
      "{str_to_title(fmt$label)}s differ <<fmt_at_locs(not_same)>>:"
    ),
    `*` = format_styled(
      "Actual:   <<fmt_asis_collapse(fmt$fmt(obj[not_same]))>>"
    ),
    `*` = format_styled(
      "Expected: <<fmt_asis_collapse(fmt$fmt(vec[not_same]))>>"
    )
  )
}

validate_is_permutation_of <- function(obj, vec, obj_name, which_elm = NULL) {
  obj_size <- vctrs::vec_size(obj)
  vec_size <- vctrs::vec_size(vec)
  in_obj <- vctrs::vec_in(vec, obj)
  in_vec <- vctrs::vec_in(obj, vec)
  if (obj_size == vec_size && all(in_obj) && all(in_vec)) {
    return(NULL)
  }

  bad_is_permutation_of_message(
    obj = obj,
    obj_name = obj_name,
    vec = vec,
    vec_size = vec_size,
    obj_size = obj_size,
    in_obj = in_obj,
    in_vec = in_vec,
    which_elm = which_elm
  )
}

bad_is_permutation_of_message <- function(
  obj,
  vec,
  vec_size,
  obj_size,
  in_obj,
  in_vec,
  obj_name,
  which_elm = NULL
) {
  if (is.null(which_elm)) {
    header <- format_styled(
      "{.arg {obj_name}} must be a permutation of: <<fmt_vec_string(vec)>>."
    )
  } else {
    fmt <- elms_format(which_elm)
    header <- format_styled(
      "{.arg {obj_name}} must have these <<fmt$have>>s in any order: ",
      "<<fmt_asis_collapse(fmt$fmt(vec))>>."
    )
  }

  if (obj_size != vec_size) {
    return(c(
      i = header,
      x = bullet_unexpected_size(obj_name, obj_size, vec_size, which_elm)
    ))
  }

  if (is.null(which_elm)) {
    c(
      i = header,
      x = if (any(!in_obj)) bullet_missing_vec(obj_name, vec[!in_obj]),
      x = if (any(!in_vec)) bullet_unexpected_vec(obj_name, obj[!in_vec])
    )
  } else {
    c(
      i = header,
      x = if (any(!in_obj)) {
        bullet_missing_elms(obj_name, vec[!in_obj], which_elm)
      },
      x = if (any(!in_vec)) {
        bullet_unexpected_elms(obj_name, obj[!in_vec], which_elm)
      }
    )
  }
}

# helpers ----------------------------------------------------------------------

elms_format <- function(which_elm) {
  switch(
    which_elm,
    attr = list(fmt = chr_encode, label = "attribute", have = "attribute"),
    name = list(fmt = chr_encode, label = "name", have = "named element"),
    col = list(fmt = backtick, label = "column", have = "column"),
  )
}

bullet_unexpected_vec <- function(obj_name, unexpected) {
  n <- vctrs::vec_size(unexpected)
  format_styled(
    "{.arg {obj_name}} contains {n} unexpected element{?s}: <<fmt_vec_string(unexpected)>>."
  )
}

bullet_unexpected_elms <- function(obj_name, unexpected, which_elm) {
  fmt <- elms_format(which_elm)
  n <- vctrs::vec_size(unexpected)
  format_styled(
    "{.arg {obj_name}} has {n} unexpected <<fmt$label>>{?s}: ",
    "<<fmt_asis_collapse(fmt$fmt(unexpected))>>."
  )
}

bullet_missing_vec <- function(obj_name, missing) {
  n <- vctrs::vec_size(missing)
  format_styled(
    "{.arg {obj_name}} is missing {n} element{?s}: <<fmt_vec_string(missing)>>."
  )
}

bullet_missing_elms <- function(obj_name, missing, which_elm) {
  fmt <- elms_format(which_elm)
  n <- vctrs::vec_size(missing)
  format_styled(
    "{.arg {obj_name}} is missing {n} <<fmt$label>>{?s}: ",
    "<<fmt_asis_collapse(fmt$fmt(missing))>>."
  )
}

bullet_matching_vec <- function(obj_name, matching) {
  n <- vctrs::vec_size(matching)
  if (n == 0L) {
    return(format_styled("{.arg {obj_name}} contains no matching values."))
  }
  format_styled(
    "{.arg {obj_name}} contains {n} matching value{?s}: <<fmt_vec_string(matching)>>."
  )
}

bullet_matching_elms <- function(obj_name, matching, which_elm) {
  fmt <- elms_format(which_elm)
  n <- vctrs::vec_size(matching)
  if (n == 0L) {
    return(format_styled(
      "{.arg {obj_name}} has no matching <<fmt$label>>s."
    ))
  }
  format_styled(
    "{.arg {obj_name}} has {n} matching <<fmt$label>>{?s}: ",
    "<<fmt_asis_collapse(fmt$fmt(matching))>>."
  )
}

bullet_bad_n_vec <- function(obj_name, n_actual, n_expected) {
  format_styled(
    "{.arg {obj_name}} has {n_actual} element{?s}, but expected {n_expected}."
  )
}

bullet_bad_n_elms <- function(obj_name, n_actual, n_expected, which_elm) {
  fmt <- elms_format(which_elm)
  format_styled(
    "{.arg {obj_name}} has {n_actual} <<fmt$label>>{?s}, ",
    "but expected {n_expected}."
  )
}

bullet_unexpected_size <- function(
  obj_name,
  obj_size,
  vec_size,
  which_elm = NULL
) {
  if (is.null(which_elm)) {
    return(bullet_bad_n_vec(obj_name, obj_size, vec_size))
  }
  fmt <- elms_format(which_elm)
  bullet_bad_n_elms(obj_name, obj_size, vec_size, which_elm)
}
