#' hstore operations
#' @description
#' Due to the way \code{dbplyr} translates infix operators, there is a number
#' of operators you can use to control Postgres/PostGIS queries. Some useful
#' ones include:
#'
#' \itemize{
#'  \item{\code{\%->\%}: Extract keys \code{y} from hstore \code{x}}
#'  \item{\code{\%?\%}: Check if key \code{y} is in hstore \code{x}}
#'  \item{\code{\%?&\%}: Check if all keys \code{y} are in hstore \code{x}}
#'  \item{\code{\%?|\%}: Check if any keys \code{y} are in hstore \code{x}}
#'  \item{\code{\%-\%}: Remove a key \code{y} from hstore \code{x}}
#'  \item{\code{hstore_zip}: Interweave keys and values of hstore \code{x}}
#'  \item{\code{hstore_2d}: Convert hstore \code{x} to a 2D key-value array}
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
#' @name hstore
#'
#' @examples
#' library(dplyr)
#'
#' pp_tbl("point") |>
#'   filter(amenity == "fast_food" & way %&&% !!pg_bbox(bbox)) |>
#'   mutate(phone = tags %->% "phone") |>
#'   select(name, way, phone)
"%->%" <- function(hstore, y) pg_op(hstore, y, "->")

#' @rdname hstore
#' @export
"%?%" <- function(hstore, key) pg_op(hstore, key, "?")

#' @rdname hstore
#' @export
"%?&%" <- function(hstore, key) pg_op(hstore, key, "?&")

#' @rdname hstore
#' @export
"%?|%" <- function(hstore, key) pg_op(hstore, key, "?|")

#' @rdname hstore
#' @export
"%-%" <- function(hstore, key) pg_op(hstore, key, "-")

#' @rdname hstore
#' @export
hstore_zip <- function(hstore) {
  pg_op(hstore, "%%")
}

#' @rdname hstore
#' @export
hstore_2d <- function(hstore) {
  pg_op(hstore, "%#")
}

pg_op <- function(x, y, operator) {
  if (missing(y)) {
    sql(sprintf("%s %s", operator, x))
  } else {
    sql(sprintf("%s %s %s", x, operator, y))
  }
}
