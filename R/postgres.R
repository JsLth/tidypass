#' Postgres utilities
#' @description
#' Utilities to ease the conversion from R to Postgres. Currently I can think
#' of only one important function:
#'
#' \itemize{
#'  \item{\code{bigint} signals a character vector as a big integer.
#'  This is necessary to work with big integers in Postgres.}
#' }
#'
#' @param x A numeric or character vector containing only whole numbers.
#' @returns An object of class \code{pg_bigint} that can be inserted into an
#' SQL query.
#'
#' @export
#'
#' @examples
#' bigint(1)
#' bigint(100000000000000000000000) # not useful
#' bigint("100000000000000000000000") # better
bigint <- function(x) {
  if (is.numeric(x)) {
    x <- as.integer(x)
  }
  x <- trimws(format(x, scientific = FALSE))
  if (!grepl("^[0-9]+$", x)) {
    rlang::abort("`x` must contain numbers.")
  }
  x <- dbplyr::sql(as.character(x))
  class(x) <- c("pg_bigint", class(x))
  x
}


#' @export
print.pg_bigint <- function(x, ...) {
  print(noquote(unclass(x)), quote = FALSE)
}
