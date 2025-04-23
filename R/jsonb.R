#' hstore/jsonb operations
#' @description
#' Due to the way \code{dbplyr} translates infix operators, there is a number
#' of operators you can use to control Postgres/PostGIS queries. Some useful
#' ones include:
#'
#' \itemize{
#'  \item{\code{\%->>\%}: Extract keys \code{y} from jsonb \code{x}}
#'  \item{\code{\%?\%}: Check if key \code{y} is in hstore \code{x}}
#'  \item{\code{\%?&\%}: Check if all keys \code{y} are in hstore \code{x}}
#'  \item{\code{\%?|\%}: Check if any keys \code{y} are in hstore \code{x}}
#'  \item{\code{\%-\%}: Remove a key \code{y} from hstore \code{x}}
#' }
#'
#' Note that in order to use (non-infix) functions, you need to inject them
#' using \code{\link[rlang:!!]{!!}}.
#'
#' @param hstore Name of a database column containing the \code{hstore} data type.
#' @param key Character string containing an \code{hstore} key.
#' @returns A character string of class \code{sql}.
#' @export
#'
#' @name agg
#'
#' @examples
#' library(dplyr)
#'
#' pp_tbl("point") |>
#'   mutate(amenity = tags %->>% 'amenity', phone = tags %->>% 'phone')
#'   filter(amenity == "fast_food" & way %&&% !!pg_bbox(bbox)) |>
#'   select(name, way, phone)
"%->>%" <- function(agg, y) pg_op(rlang::enquo(agg), rlang::enquo(y), "->>")

#' @rdname agg
#' @export
"%?%" <- function(agg, key) pg_op(rlang::enquo(agg), rlang::enquo(y), "?")

#' @rdname agg
#' @export
"%?&%" <- function(agg, key) pg_op(rlang::enquo(agg), rlang::enquo(y), "?&")

#' @rdname agg
#' @export
"%?|%" <- function(agg, key) pg_op(rlang::enquo(agg), rlang::enquo(y), "?|")

#' @rdname agg
#' @export
"%-%" <- function(agg, key) pg_op(rlang::enquo(agg), rlang::enquo(y), "-")


#' Unwrap jsonb
#' @description
#' \code{dbplyr} helper function to extract multiple keys from a jsonb. This
#' is similar to \code{unwrap_tags} but instead of unwrapping the output of
#' \code{\link{collect}}, it generates an SQL query to extract specific keys
#' inside the database. This is mostly useful to reduce the amount of typing
#' required. So instead of:
#'
#' \preformatted{
#'  pp_tbl("point") |>
#'    transmute(
#'      amenity = tags %->>% "amenity",
#'      cuisine = tags %->>% "cuisine",
#'      name = tags %->>% "name",
#'      email = tags %->>% "email"
#'    )
#' }
#'
#' you can write:
#'
#' \preformatted{
#'  pp_tbl("point") |>
#'    unwrap(tags \%->>\% c("amenity", "cuisine", "name", "email"))
#' }
#'
#' @param query A lazy dataframe of class \code{pp_tbl}.
#' @param ... One or several calls to \code{\link{\%->>\%}} where the left-hand
#' side can contain multiple values.
#' @returns Another \code{pp_tbl}.
#'
#' @export
#' @examples
#' pp_tbl("point") |>
#'    unwrap(tags %->>% c("amenity", "cuisine", "name", "email"))
unwrap <- function(query, ...) {
  quosures <- rlang::quos(..., .ignore_empty = "all")
  keys <- NULL
  n_quos <- length(quosures)
  sql <- lapply(seq_len(n_quos), function(i) {
    cl <- quosures[[i]][[2]]
    jsonb <- cl[[2]]
    keys <- eval(cl[[3]])
    extr <- (!!jsonb) %->>% !!keys
    names(extr) <- keys
    extr
  })
  sql <- unlist(sql)
  keys <- names(sql)
  sql <- unname(sql)
  sql <- as.list(sql)
  names(sql) <- keys
  sql <- lapply(sql, dbplyr::sql)
  dplyr::mutate(dplyr::select(query, !!!colnames(query)), !!!sql)
}

pg_op <- function(x, y, operator) {
  x <- expr_text(rlang::quo_get_expr(x))
  x <- string_to_varchar(x)
  if (missing(y)) {
    dbplyr::sql(sprintf("%s %s", operator, x))
  } else {
    y <- expr_text(rlang::quo_get_expr(y))
    y <- string_to_varchar(y)
    dbplyr::sql(sprintf("%s %s %s", x, operator, y))
  }
}


expr_text <- function(expr) {
  if (is.character(expr)) {
    encodeString(expr, quote = "\"")
  } else if (is.null(expr) || is.atomic(expr)) {
    format(expr)
  } else {
    as.character(expr)
  }
}


string_to_varchar <- function(x) {
  gsub("\"", "'", x)
}
