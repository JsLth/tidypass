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

table_name <- function() {
  vals <- c(letters, LETTERS, seq_len(9))
  name <- paste0(sample(vals, 10, replace = TRUE), collapse = "")
  paste0("postpass_", name)
}
