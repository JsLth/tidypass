#' hstore operations
#' @description
#' Helper functions to work with hstores in \code{dbplyr} queries. In Postpass,
#' all undefined tags are stored in a hstore can be queried using hstore
#' operations.
#'
#' Currently supported:
#'
#' \itemize{
#'  \item{\code{hs_values}: Returns the values of given keys (\code{->})}
#'  \item{\code{hs_exists}: Checks if a key exists in an hstore (\code{?})}
#'  \item{\code{hs_all}: Checks if all specified keys exist in an hstore (\code{?&})}
#'  \item{\code{hs_all}: Checks if any specified keys exist in an hstore (\code{?|})}
#'  \item{\code{hs_delete}: Deletes a key from an hstore (\code{-})}
#'  \item{\code{hs_zip}: Interweaves keys and values in an array (\code{\%\%})}
#'  \item{\code{hs_keyvalue}: Converts an hstore to a 2D key-value array (\code{\%#})}
#' }
#'
#' @param x Left-hand side operator
#' @param y Right-hand side operator
#' @returns A character string of class \code{sql}.
#' @export
#'
#' @examples
#' # example code
#'
hs_values <- function(x, y) {
  x <- rlang::expr_text(rlang::enexpr(x))
  y <- rlang::expr_text(rlang::enexpr(y))
  hstore(x, y, "->")
}

#' @rdname hs_values
#' @export
hs_exists <- function(x, y) {
  x <- rlang::expr_text(rlang::enexpr(x))
  y <- rlang::expr_text(rlang::enexpr(y))
  hstore(x, y, "?")
}

#' @rdname hs_values
#' @export
hs_all <- function(x, y) {
  x <- rlang::expr_text(rlang::enexpr(x))
  y <- rlang::expr_text(rlang::enexpr(y))
  hstore(x, y, "?&")
}

#' @rdname hs_values
#' @export
hs_any <- function(x, y) {
  x <- rlang::expr_text(rlang::enexpr(x))
  y <- rlang::expr_text(rlang::enexpr(y))
  hstore(x, y, "?|")
}

#' @rdname hs_values
#' @export
hs_delete <- function(x, y) {
  x <- rlang::expr_text(rlang::enexpr(x))
  y <- rlang::expr_text(rlang::enexpr(y))
  hstore(x, y, "-")
}

#' @rdname hs_values
#' @export
hs_zip <- function(x) {
  x <- rlang::expr_text(rlang::enexpr(x))
  hstore(x, y, "#=")
}

#' @rdname hs_values
#' @export
hs_keyvalue <- function(x) {
  x <- rlang::expr_text(rlang::enexpr(x))
  hstore(x, y, "%#")
}

hstore <- function(x, y, operator) {
  if (missing(y)) {
    sql(sprintf("%s %s", operator, x))
  } else {
    sql(sprintf("%s %s %s", x, operator, y))
  }

}
