# cli --------------------------------------------------------------------------

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

# Borrowed with thanks from {cli}:
# https://github.com/r-lib/cli/blob/9cf4733030622fbfd21468e9a6d67041c5e64a56/R/utils.R#L13
cli_escape <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  x
}

cat_bullets <- function(x) {
  # TODO: `cli::format_bullets_raw` performs substitution (still) on
  # words wrapped in "<>". I'd like to keep the bullets, think about
  # what to do here.
  bullets <- cli_bullets_escape(x)
  bullets <- cli_bullets_unescape(cli::format_bullets_raw(bullets))
  cat(bullets, sep = "\n")
}

cli_bullets_escape <- function(x) {
  x <- gsub("<", "<<", x, fixed = TRUE)
  x <- gsub(">", ">>", x, fixed = TRUE)
  x
}

cli_bullets_unescape <- function(x) {
  x <- gsub("<<", "<", x, fixed = TRUE)
  x <- gsub(">>", ">", x, fixed = TRUE)
  x
}

# formatters -------------------------------------------------------------------

oxford <- function(x, last = " and ") {
  fmt_asis_collapse(x, n_elm_max = Inf, n_chr_max = Inf, last = last)
}

commas <- function(x) {
  paste(x, collapse = ", ")
}

backtick <- function(x) {
  paste0("`", x, "`")
}

untick <- function(x) {
  gsub("`+$", "", gsub("^`+", "", x))
}

chr_encode <- function(x, quote = '"') {
  encodeString(x, quote = quote)
}

str_upper1 <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

str_lower1 <- function(x) {
  paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

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
  sep = ", ",
  last = NULL
) {
  fmt_asis_collapse(
    fmt_vec(vec), 
    n_elm_max = n_elm_max, 
    n_chr_max = n_chr_max, 
    sep = sep,
    last = last
  )
}

fmt_asis_collapse <- function(
  x, 
  n_elm_max = 10L,
  n_chr_max = 50L, 
  sep = ", ",
  last = NULL
) {
  n <- vctrs::vec_size(x)

  if (n == 0L) {
    return("")
  }
  if (n == 1L) {
    return(chr_trunc(x, n_max = n_chr_max))
  }

  # Everything fits
  collapsed <- paste(x, collapse = sep)
  if (n <= n_elm_max && nchar(collapsed) + nchar(last %||% "") <= n_chr_max) {
    if (!is.null(last) && n >= 2L) {
      head <- x[seq_len(n - 1L)]
      tail <- x[[n]]
      last_sep <- if (n == 2L) last else gsub("\\s+", " ", paste0(sep, last))
      return(paste0(paste(head, collapse = sep), last_sep, tail))
    }
    return(collapsed)
  }

  # Too many elements or characters: head, ..., tail
  init_head_n <- max(1L, min(n_elm_max - 1L, n - 1L))
  head <- x[seq_len(init_head_n)]
  tail <- x[[n]]
  head_n <- max(which(nchar(tail) + cumsum(nchar(head)) <= n_chr_max))
  if (head_n > 0) {
    head <- x[seq_len(head_n)]
    return(paste(c(head, "...", tail), collapse = sep))
  }

  # Still too long: just head, ...
  head <- x[[1L]]
  collapsed <- paste(c(head, "..."), collapse = sep)
  if (nchar(collapsed) <= n_chr_max) {
    return(collapsed)
  }

  # Still too long: truncate the first element itself
  n_max <- max(n_chr_max - nchar(sep) - 3L, 1L)
  truncated <- chr_trunc(head, n_max = n_max)
  paste(c(truncated, "..."), collapse = sep)
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
  if (n == 1L) {
    glue::glue("at {place} {fmt_vec_string(where)}")
  } else if (n <= 5) {
    glue::glue("at {places} {fmt_vec_string(where)}")
  } else {
    glue::glue("at {places} {fmt_vec_string(where)} and {n - 5} more")
  }
}

# TODO: We'll need to create our own obj_type_friendly()
# - See: cli:::typename, cli:::friendly_type
# - See rlang:::obj_type_friendly, rlang:::obj_type_oo
fmt_r_type <- function(obj) {
  rlang:::obj_type_friendly(obj)
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

chr_trunc <- function(x, n_max, tail = "...") {
  n_max <- n_max
  too_long <- nchar(x) > (n_max + nchar(tail))
  x[too_long] <- paste0(substr(x[too_long], 0, pmin(nchar(x[too_long]), n_max)), tail)
  x
}

str_to_title <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}
