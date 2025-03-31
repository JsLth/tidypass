"%||%" <- function(x, y) if (is.null(x)) y else x
"%|||%" <- function(x, y) if (is.null(x) || all(is.na(x))) y else x


regex_match <- function (text, pattern, i = NULL, ...) {
  match <- regmatches(text, regexec(pattern, text, ...))
  if (!is.null(i)) {
    match <- vapply(match, FUN.VALUE = character(1), function(x) {
      if (length(x) >= i) x[[i]] else NA_character_
    })
  }
  match
}


insert_df <- function(x, y, position, where = "replace") {
  if (is.character(position)) {
    position <- which(position == names(x))
  }

  is_sf <- inherits(x, "sf")
  if (is_sf) {
    sf_col <- attr(x, "sf_column")
    class(x) <- setdiff(class(x), "sf")
  }

  x <- switch(
    where,
    replace = insert_df_impl(x, y, start = position - 1, end = position + 1),
    after = insert_df_impl(x, y, start = position, end = position + 1),
    before = insert_df_impl(x, y, start = position - 1, end = position)
  )

  if (is_sf) {
    x <- sf::st_as_sf(x, sf_column_name = sf_col)
  }

  x
}


insert_df_impl <- function(x, y, start, end) {
  if (start < 1) {
    dplyr::bind_cols(y, x)
  } else if (end > ncol(x)) {
    dplyr::bind_cols(x, y)
  } else {
    before <- x[, seq(1, start), drop = FALSE]
    after <- x[, seq(end, ncol(x)), drop = FALSE]
    dplyr::bind_cols(before, y, after)
  }
}
