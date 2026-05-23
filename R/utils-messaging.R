# misc -------------------------------------------------------------------------

commas <- function(x) {
  paste(x, collapse = ", ")
}

collapse <- function(x, sep) {
  paste(x, collapse = sep)
}

c_commas <- function(x) {
  paste0("c(", commas(x), ")")
}

backtick <- function(x) {
  paste0("`", x, "`")
}

chr_encode <- function(x, quote = '"') {
  encodeString(x, quote = quote)
}

chr_trunc <- function(x, n_max, tail = "...") {
  n_max <- n_max
  too_long <- nchar(x) > (n_max + nchar(tail))
  x[too_long] <- paste0(substr(x[too_long], 0, max(0, n_max)), tail)
  x
}

chr_remove <- function(x, remove) {
  gsub(remove, "", x)
}

str_to_title <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}

oxford <- function(x, last = "and") {
  n <- length(x)
  if (n <= 1) {
    return(x)
  }
  if (n == 2) {
    return(paste(x[[1]], last, x[[2]]))
  }
  head <- paste(x[-n], collapse = ", ")
  paste0(head, ", ", last, " ", x[[n]])
}

format_styled <- function(
  ...,
  .envir = parent.frame(),
  .glue_open = "<<",
  .glue_close = ">>"
) {
  cli::format_inline(
    glue::glue(..., .envir = .envir, .open = .glue_open, .close = .glue_close),
    .envir = .envir
  )
}

format_plain <- function(
  ...,
  .envir = parent.frame(),
  .glue_open = "<<",
  .glue_close = ">>"
) {
  # TODO: Update this as needed
  plain_theme <- list(
    span.val = list(
      before = "`",
      after = "`",
      color = "none",
      "font-style" = "none"
    ),
    span.val_q = list(
      before = '"',
      after = '"',
      color = "none",
      "font-style" = "none"
    ),
    span.pkg = list(
      before = "{",
      after = "}",
      color = "none",
      "font-weight" = "none"
    ),
    span.cls = list(
      before = "<",
      after = ">",
      color = "none",
      "font-style" = "none"
    ),
    span.cls_q = list(
      before = '"<',
      after = '>"',
      color = "none",
      "font-style" = "none"
    ),
    span.fn = list(
      before = "`",
      after = "()`",
      color = "none",
      "font-style" = "none"
    ),
    span.arg = list(
      before = "`",
      after = "`",
      color = "none",
      "font-style" = "none"
    ),
    span.kbd = list(
      before = "[",
      after = "]",
      color = "none",
      "font-style" = "none"
    ),
    span.key = list(
      before = "[",
      after = "]",
      color = "none",
      "font-style" = "none"
    ),
    span.file = list(
      before = "'",
      after = "'",
      color = "none",
      "font-style" = "none"
    ),
    span.path = list(
      before = "'",
      after = "'",
      color = "none",
      "font-style" = "none"
    ),
    span.email = list(
      before = "<",
      after = ">",
      color = "none",
      "font-style" = "none"
    ),
    span.url = list(
      before = "<",
      after = ">",
      color = "none",
      "font-style" = "none"
    ),
    span.var = list(
      before = "`",
      after = "`",
      color = "none",
      "font-style" = "none"
    ),
    span.envvar = list(
      before = "`",
      after = "`",
      color = "none",
      "font-style" = "none"
    ),
    span.code = list(
      before = "`",
      after = "`",
      color = "none",
      "font-style" = "none"
    ),
    span.strong = list(
      before = "*",
      after = "*",
      color = "none",
      "font-weight" = "none"
    ),
    span.emph = list(
      before = "_",
      after = "_",
      color = "none",
      "font-style" = "none"
    ),
    span.field = list(
      before = "`",
      after = "`",
      color = "none",
      "font-style" = "none"
    )
  )
  cli::cli_div(theme = plain_theme)
  cli::format_inline(
    glue::glue(..., .envir = .envir, .open = .glue_open, .close = .glue_close),
    .envir = .envir
  )
}

# Borrowed from {cli} with thanks:
# https://github.com/r-lib/cli/blob/9cf4733030622fbfd21468e9a6d67041c5e64a56/R/utils.R#L13
cli_escape <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  x
}

# formatters -------------------------------------------------------------------

fmt_vec_string <- function(vec, n_elm_max = 5L, n_chr_max = 50L) {
  has_literals <- function(x) is.numeric(x) || is.logical(x) || is.character(x)
  n_chr_max <- n_chr_max - 3L
  formatted <- fmt_vec_collapse(vec, n_elm_max, n_chr_max, sep = ", ")
  if (vctrs::vec_size(vec) == 1L || !has_literals(vec)) {
    return(backtick(formatted))
  }

  # Wrapping vectors with literals in `c()`, e.g. `c(TRUE, FALSE)` for logical,
  # but just `2020-01-02, 2020-01-03` for dates.
  paste0("`c(", formatted, ")`")
}

fmt_vec_collapse <- function(
  vec,
  n_elm_max = 10L,
  n_chr_max = 50L,
  sep = ", "
) {
  fmt_asis_collapse(fmt_vec(vec))
}

fmt_syms_collapse <- function(
  chr,
  n_elm_max = 10L,
  n_chr_max = 50L,
  sep = ", "
) {
  fmt_asis_collapse(backtick(chr))
}

fmt_asis_collapse <- function(x, n_elm_max = 10L, n_chr_max = 50L, sep = ", ") {
  n <- vctrs::vec_size(x)

  if (n == 0L) {
    return("")
  }
  if (n == 1L) {
    return(chr_trunc(x, n_max = n_chr_max))
  }

  # Everything fits
  collapsed <- collapse(x, sep = sep)
  if (n <= n_elm_max && nchar(collapsed) <= n_chr_max) {
    return(collapsed)
  }

  # Too many elements or characters: head, ..., tail
  init_head_n <- max(1L, min(n_elm_max - 1L, n - 1L))
  head <- x[seq_len(init_head_n)]
  tail <- x[[n]]
  head_n <- max(which(nchar(tail) + cumsum(nchar(head)) <= n_chr_max))
  if (head_n > 0) {
    head <- x[seq_len(head_n)]
    return(collapse(c(head, "...", tail), sep = sep))
  }

  # Still too long: just head, ...
  head <- x[[1L]]
  collapsed <- collapse(c(head, "..."), sep = sep)
  if (nchar(collapsed) <= n_chr_max) {
    return(collapsed)
  }

  # Still too long: truncate the first element itself
  n_max <- max(n_chr_max - nchar(sep) - 3L, 1L)
  truncated <- chr_trunc(head, n_max = n_max)
  collapse(c(truncated, "..."), sep = sep)
}

# TODO: Possibly rounding and custom formatting for other objects
# - See {ivs} iv_format()
fmt_vec <- function(vec) {
  if (is.character(vec)) {
    chr_encode(vec)
  } else {
    format(vec, trim = TRUE)
  }
}

fmt_at_locs <- function(where, place = c("location", "row")) {
  place <- rlang::arg_match0(place, c("location", "row"))
  places <- paste0(place, "s")
  if (is.logical(where)) {
    where <- which(where)
  }
  if (rlang::is_empty(where)) {
    return(glue::glue("at no {places}"))
  }

  n <- length(where)
  where <- fmt_vec(where)
  if (n == 1L) {
    glue::glue("at {place} {backtick(where)}")
  } else if (n <= 5) {
    glue::glue("at {places} {backtick(c_commas(where))}")
  } else {
    glue::glue("at {places} {backtick(c_commas(where))} and {n - 5} more")
  }
}

fmt_locs <- function(where) {
  if (is.logical(where)) {
    where <- which(where)
  }
  if (rlang::is_empty(where)) {
    return(backtick("c()"))
  }

  n <- length(where)
  where <- fmt_vec(where)
  if (n == 1L) {
    backtick(where)
  } else if (n <= 5) {
    backtick(c_commas(where))
  } else {
    glue::glue("{backtick(c_commas(where))} and {n - 5} more")
  }
}

# TODO: We'll need to create our own obj_type_friendly()
# - See: cli:::typename, cli:::friendly_type
# - See rlang:::obj_type_friendly, rlang:::obj_type_oo
fmt_r_type <- function(obj) {
  rlang:::obj_type_friendly(obj)
}
