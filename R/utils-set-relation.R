# dispatch ---------------------------------------------------------------------

test_vec_set_relation <- function(obj, vec, relation) {
  switch(
    relation,
    all_of = test_relation_all_of(obj, vec),
    any_of = test_relation_any_of(obj, vec),
    one_of = test_relation_one_of(obj, vec),
    subset_of = test_relation_subset_of(obj, vec),
    none_of = test_relation_none_of(obj, vec),
    setequal = test_relation_setequal(obj, vec),
    same = test_relation_same(obj, vec),
    perm_of = test_relation_perm_of(obj, vec),
    abort_internal(format_styled("Unexpected relation: {relation}."))
  )
}

diagnose_vec_set_relation <- function(
  obj,
  vec,
  relation,
  obj_name
) {
  switch(
    relation,
    all_of = diagnose_relation_all_of(obj, vec, obj_name),
    any_of = diagnose_relation_any_of(obj, vec, obj_name),
    one_of = diagnose_relation_one_of(obj, vec, obj_name),
    subset_of = diagnose_relation_subset_of(obj, vec, obj_name),
    none_of = diagnose_relation_none_of(obj, vec, obj_name),
    setequal = diagnose_relation_setequal(obj, vec, obj_name),
    same = diagnose_relation_same(obj, vec, obj_name),
    perm_of = diagnose_relation_perm_of(obj, vec, obj_name),
    abort_internal(format_styled("Unexpected relation: {relation}."))
  )
}

describe_vec_set_relation <- function(
  obj_name,
  vec,
  relation
) {
  switch(
    relation,
    all_of = describe_relation_all_of(obj_name, vec),
    any_of = describe_relation_any_of(obj_name, vec),
    one_of = describe_relation_one_of(obj_name, vec),
    subset_of = describe_relation_subset_of(obj_name, vec),
    none_of = describe_relation_none_of(obj_name, vec),
    setequal = describe_relation_setequal(obj_name, vec),
    same = describe_relation_same(obj_name, vec),
    perm_of = describe_relation_perm_of(obj_name, vec),
    abort_internal(format_styled("Unexpected relation: {relation}."))
  )
}

abbr_vec_set_relation <- function(relation) {
  switch(
    relation,
    all_of = "all",
    any_of = "any",
    one_of = "one_of",
    subset_of = "some",
    none_of = "no",
    setequal = "only",
    same = "same",
    perm_of = "perm_of",
    abort_internal(format_styled("Unexpected relation: {relation}."))
  )
}

# all_of -----------------------------------------------------------------------

test_relation_all_of <- function(obj, vec) {
  rlang::is_true(try_test(all(vctrs::vec_in(vec, obj))))
}

diagnose_relation_all_of <- function(obj, vec, obj_name) {
  in_obj <- try_test(vctrs::vec_in(vec, obj))
  n_vec <- vctrs::vec_size(vec)
  if (!is.logical(in_obj)) {
    footer <- format_styled("Checking for elements in {.arg {obj_name}} raised an error.")
  } else {
    footer <- bullet_missing_vec(obj_name, vec[!in_obj])
  }
  c(
    i = format_styled(
      "{.arg {obj_name}} must contain {qty(n_vec)}element{?s}: <<fmt_vec_string(vec)>>."
    ),
    x = footer
  )
}

describe_relation_all_of <- function(obj_name, vec) {
  n_vec <- vctrs::vec_size(vec)
  elements <- fmt_vec_string(vec, n_elm_max = Inf, n_chr_max = Inf)
  format_styled(
    "{.arg {obj_name}} contains {qty(n_vec)}element{?s}: <<elements>>."
  )
}

# any_of -----------------------------------------------------------------------

test_relation_any_of <- function(obj, vec) {
  rlang::is_true(try_test(any(vctrs::vec_in(vec, obj))))
}

diagnose_relation_any_of <- function(obj, vec, obj_name) {
  c(
    i = format_styled(
      "{.arg {obj_name}} must contain at least one of: <<fmt_vec_string(vec)>>."
    ),
    x = bullet_matching_vec(obj_name, NULL)
  )
}

describe_relation_any_of <- function(obj_name, vec) {
  elements <- fmt_vec_string(vec, n_elm_max = Inf, n_chr_max = Inf)
  format_styled(
    "{.arg {obj_name}} contains at least one of: <<elements>>."
  )
}

# one_of -----------------------------------------------------------------------

test_relation_one_of <- function(obj, vec) {
  rlang::is_true(try_test(sum(vctrs::vec_in(vec, obj)) == 1L))
}

diagnose_relation_one_of <- function(obj, vec, obj_name) {
  in_obj <- try_test(vctrs::vec_in(vec, obj))
  if (!is.logical(in_obj)) {
    footer <- format_styled("Checking for elements in {.arg {obj_name}} raised an error.")
  } else {
    footer <- bullet_matching_vec(obj_name, vec[in_obj])
  }
  c(
    i = format_styled(
      "{.arg {obj_name}} must contain exactly 1 of: <<fmt_vec_string(vec)>>."
    ),
    x = footer
  )
}

describe_relation_one_of <- function(obj_name, vec) {
  elements <- fmt_vec_string(vec, n_elm_max = Inf, n_chr_max = Inf)
  format_styled(
    "{.arg {obj_name}} contains exactly 1 of: <<elements>>."
  )
}

# subset_of --------------------------------------------------------------------

test_relation_subset_of <- function(obj, vec) {
  rlang::is_true(try_test(all(vctrs::vec_in(obj, vec))))
}

diagnose_relation_subset_of <- function(obj, vec, obj_name) {
  in_vec <- vctrs::vec_in(obj, vec)
  if (!is.logical(in_vec)) {
    footer <- format_styled("Checking for elements in {.arg {obj_name}} raised an error.")
  } else {
    footer <- bullet_unexpected_vec(obj_name, obj[!in_vec])
  }
  c(
    i = format_styled(
      "{.arg {obj_name}} may only contain values from: <<fmt_vec_string(vec)>>."
    ),
    x = footer
  )
}

describe_relation_subset_of <- function(obj_name, vec) {
  elements <- fmt_vec_string(vec, n_elm_max = Inf, n_chr_max = Inf)
  format_styled(
    "{.arg {obj_name}} contains only values from: <<elements>>."
  )
}

# none_of ----------------------------------------------------------------------

test_relation_none_of <- function(obj, vec) {
  rlang::is_true(try_test(!any(vctrs::vec_in(vec, obj))))
}

diagnose_relation_none_of <- function(obj, vec, obj_name) {
  in_obj <- vctrs::vec_in(vec, obj)
  if (!is.logical(in_obj)) {
    footer <- format_styled("Checking for elements in {.arg {obj_name}} raised an error.")
  } else {
    footer <- bullet_unexpected_vec(obj_name, vec[in_obj])
  }
  c(
    i = format_styled(
      "{.arg {obj_name}} must not contain any of: <<fmt_vec_string(vec)>>."
    ),
    x = footer
  )
}

describe_relation_none_of <- function(obj_name, vec) {
  elements <- fmt_vec_string(vec, n_elm_max = Inf, n_chr_max = Inf)
  format_styled(
    "{.arg {obj_name}} contains none of: <<elements>>."
  )
}

# setequal ---------------------------------------------------------------------

test_relation_setequal <- function(obj, vec) {
  test_relation_all_of(obj, vec) && test_relation_subset_of(obj, vec)
}

diagnose_relation_setequal <- function(obj, vec, obj_name) {
  in_obj <- try_test(vctrs::vec_in(vec, obj))
  in_vec <- try_test(vctrs::vec_in(obj, vec))
  if (!(is.logical(in_obj) & is.logical(in_vec))) {
    footer <- c(x = format_styled("Checking for elements in {.arg {obj_name}} raised an error."))
  } else {
    extra_vec  <- obj[!in_vec]
    missing_vec <- vec[!in_obj]
    n_missing <- vctrs::vec_size(missing_vec)
    n_extra <- vctrs::vec_size(extra_vec)
    footer <- c(
      x = if (n_missing > 0) bullet_missing_vec(obj_name, missing_vec),
      x = if (n_extra > 0)   bullet_unexpected_vec(obj_name, extra_vec)
    )
  }
  c(
    i = format_styled(
      "{.arg {obj_name}} must contain exactly: <<fmt_vec_string(vec)>>."
    ),
    footer
  )
}

describe_relation_setequal <- function(obj_name, vec) {
  elements <- fmt_vec_string(vec, n_elm_max = Inf, n_chr_max = Inf)
  format_styled(
    "{.arg {obj_name}} is setequal to: <<elements>>."
  )
}

# same -------------------------------------------------------------------------

test_relation_same <- function(obj, vec) {
  rlang::is_true(try_test(vctrs::vec_size(obj) == vctrs::vec_size(vec) &&
    all(vctrs::vec_equal(obj, vec, na_equal = TRUE))))
}

diagnose_relation_same <- function(obj, vec, obj_name) {
  obj_size <- vctrs::vec_size(obj)
  vec_size <- vctrs::vec_size(vec)

  if (obj_size != vec_size) {
    return(c(x = bullet_bad_n_vec(obj_name, obj_size, vec_size)))
  }

  not_same <- try_test(!vctrs::vec_equal(obj, vec, na_equal = TRUE))
  if (!is.logical(not_same)) {
    footer <- c(x = format_styled("Checking for elements in {.arg {obj_name}} raised an error."))
  } else {
    footer <- c(
      x = format_styled("{.arg {obj_name}} differs <<fmt_at_locs(not_same)>>:"),
      `*` = format_styled("Actual:   <<fmt_vec_string(obj[not_same])>>"),
      `*` = format_styled("Expected: <<fmt_vec_string(vec[not_same])>>")
    )
  }
  c(
    i = format_styled("{.arg {obj_name}} must be: <<fmt_vec_string(vec)>>."),
    footer
  )
}

describe_relation_same <- function(obj_name, vec) {
  elements <- fmt_vec_string(vec, n_elm_max = Inf, n_chr_max = Inf)
  format_styled(
    "{.arg {obj_name}} is the same as: <<elements>>."
  )
}

# perm_of ----------------------------------------------------------------------

test_relation_perm_of <- function(obj, vec) {
  rlang::is_true(try_test(vctrs::vec_size(obj) == vctrs::vec_size(vec) && test_relation_setequal(obj, vec)))
}

diagnose_relation_perm_of <- function(obj, vec, obj_name) {
  obj_size <- vctrs::vec_size(obj)
  vec_size <- vctrs::vec_size(vec)

  header <- format_styled(
    "{.arg {obj_name}} must be a permutation of: <<fmt_vec_string(vec)>>."
  )

  if (obj_size != vec_size) {
    return(c(
      i = header,
      x = bullet_bad_n_vec(obj_name, obj_size, vec_size)
    ))
  }

  in_obj <- try_test(vctrs::vec_in(vec, obj))
  in_vec <- try_test(vctrs::vec_in(obj, vec))
  
  if (!(is.logical(in_obj) & is.logical(in_vec))) {
    footer <- c(x = format_styled("Checking for elements in {.arg {obj_name}} raised an error."))
  } else {
    footer <- c(
      x = if (any(!in_obj)) bullet_missing_vec(obj_name, vec[!in_obj]),
      x = if (any(!in_vec)) bullet_unexpected_vec(obj_name, obj[!in_vec])
    )
  }

  c(
    i = header,
    footer
  )
}

describe_relation_perm_of <- function(obj_name, vec) {
  elements <- fmt_vec_string(vec, n_elm_max = Inf, n_chr_max = Inf)
  format_styled(
    "{.arg {obj_name}} is a permutation of: <<elements>>."
  )
}

# test_helpers -----------------------------------------------------------------

try_test <- function(expr) {
  rlang::try_fetch(
    expr,
    error = identity,
    warning = function(cnd) rlang::cnd_muffle(cnd)
  )
}

# message helpers --------------------------------------------------------------

bullet_unexpected_vec <- function(obj_name, unexpected) {
  n <- vctrs::vec_size(unexpected)
  format_styled(
    "{.arg {obj_name}} contains {n} unexpected element{?s}: <<fmt_vec_string(unexpected)>>."
  )
}

bullet_missing_vec <- function(obj_name, missing) {
  n <- vctrs::vec_size(missing)
  format_styled(
    "{.arg {obj_name}} is missing {n} element{?s}: <<fmt_vec_string(missing)>>."
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

bullet_bad_n_vec <- function(obj_name, n_actual, n_expected) {
  format_styled(
    "{.arg {obj_name}} has {n_actual} element{?s}, but expected {n_expected}."
  )
}
