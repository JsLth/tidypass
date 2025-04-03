postpass_url <- function() sprintf(
  "https://postpass.geofabrik.de/api/%s", PP_VERSION
)

#' Postpass tibble
#' @description
#' Creates a lazy tibble based on a simulated Postgres database and a Postpass
#' database schema. This tibble can be manipulated using \code{dplyr} functions.
#' Each operation extends the SQL query which is then sent to the Postpass
#' server by running \code{\link{collect}}.
#'
#' @param table A table name (or at least its suffix). You can retrieve a list
#' of available table names by running \code{\link{list_tables}} or
#' \code{\link{pp_tables}}.
#' @param schema Optionally, a schema dataframe. A schema should contain only
#' columns accessible in the remote PostGIS database along with their prototype
#' classes. Schemas can be retrieved from \code{\link{pp_schema}}. If
#' \code{NULL}, infers a schema from \code{table}.
#' @param name If \code{schema} is provided, specifies the name of the Postpass
#' table from which to query.
#'
#' @details
#' Note that all columns can be treated as either numeric or character vectors
#' with two exceptions:
#'
#' \itemize{
#'  \item{\code{tags} is an
#'  \href{https://www.postgresql.org/docs/current/hstore.html}{\code{hstore}}
#'  which is something like a named list in Postgres. You can interact with it
#'  using \link[hstore]{hstore helpers}}.
#'  \item{\code{way} is a
#'  \href{https://postgis.net/docs/}{PostGIS} geometry. You can interact with
#'  it using \link[postgis]{PostGIS helpers}.}
#' }
#'
#' @returns A lazy tibble of class \code{pp_tbl}.
#' @export
#'
#' @examples
#' pp_tbl("point")
#' pp_tbl("roads")
#'
#' # provide a custom schema
#' pp_tbl(schema = dplyr::tibble(amenity = character(1), way = list(NULL)))
pp_tbl <- function(table, schema = NULL, name = NULL) {
  if (is.null(schema)) {
    if (!any(startsWith(table, c("postpass", "planet_osm")))) {
      table <- sprintf("postpass_%s", table)
    }
    schema <- schemas[[table]]
  }

  if (is.null(schema)) {
    rlang::abort(c(
      sprintf("The provided table '%s' does not exist.", table),
      "i" = "You can run `pp_tables()` to find out which tables exist."
    ))
  }

  name <- name %||% attr(schema, "postpass_table")
  if (is.null(name)) {
    rlang::abort(c(
      "Invalid schema provided. Table name is missing.",
      "i" = paste(
        "You can retrieve schemas using `pp_schema()` or provide a",
        "table name manually using the `name` argument."
      )
    ))
  }

  schema <- schema_to_tbl(schema)
  tbl <- dbplyr::tbl_lazy(
    schema,
    con = dbplyr::simulate_postgres(),
    name = name
  )
  class(tbl) <- c("pp_tbl", class(tbl))
  tbl
}


#' Send SQL query to Postpass
#' @description
#' Accepts a \code{dbplyr} query, sanitizes it, and sends it to Postpass.
#' If the query is valid, returns a (sf) tibble.
#'
#' @param x A lazy tibble of class \code{\link{pp_tbl}}.
#' @param geojson Whether to generate a GeoJSON or not. If \code{TRUE}, returns
#' an sf tibble, otherwise a normal tibble. Should only be \code{TRUE} if
#' \code{collection} is also \code{TRUE}, otherwise Postpass will not return
#' a valid GeoJSON.
#' @param collection Whether to aggregate results to a JSON or not.
#' If \code{FALSE}, a scalar must be returned by \code{x}, i.e. one row and one
#' column. Necessary to compute single counts.
#' @param unwrap Whether to convert the \code{tags} column to a dataframe.
#' If \code{FALSE}, leaves tags as an unparsed JSON. If \code{TRUE}, parses it
#' and inserts it to the position of the \code{tags} column using
#' \code{\link{unwrap_tags}}.
#' @param ... Further parameters passed to Postpass.
#'
#' @returns A (sf) tibble.
#'
#' @section Syntax:
#' \code{tidypass} queries mostly follow the dplyr syntax for data analysis.
#' You can use most functions that work on database backends with some possible
#' exceptions. Due to the special syntax of Postgres and PostGIS,
#' \code{tidypass} provides some utilities to facilitate
#' the translation from R to Postgres (see \code{\link[hstore]{hstore}} and
#' \code{\link[postgis]{PostGIS}} utilities). Generally, operators like
#' \code{&&} or \code{->} must be passed as infix operators, i.e.,
#' \code{\%&&\%} and \code{\%->\%}. Utility functions and global objects must
#' be injected in the traditional tidyverse manner using
#' \code{\link[rlang]{!!}}.
#'
#' @section Limitations:
#' Postpass manages API overload by regulating workers. Depending on their
#' estimated complexity, queries are put in a slow, medium or quick queue.
#'
#' \itemize{
#'  \item{Quick queries: Under 100s, 10 workers}
#'  \item{Medium queries: Under 50,000s, 4 workers}
#'  \item{Slow queries: Over 50,000s, 2 workers}
#' }
#'
#' Additionally, the PostGIS database is read-only. Queries can only retrieve
#' data but not write, e.g., using \code{DROP TABLE}.
#'
#' @exportS3Method dplyr::collect
#'
#' @examples
#' \donttest{library(sf)
#' nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
#' surry <- nc[nc$NAME %in% "Surry", ]
#'
#' # Get all fast food restaurants in Surry, NC
#' res <- pp_tbl("point") |>
#'   filter(amenity == "fast_food" & way %&&% !!pg_bbox(surry)) |>
#'   select(name, way, tags) |>
#'   collect()}
collect.pp_tbl <- function(x,
                           geojson = collection,
                           collection = TRUE,
                           unwrap = TRUE,
                           ...) {
  options <- list(geojson = geojson, collection = collection, pretty = FALSE, ...)
  sql <- dbplyr::sql_render(x)
  sql <- sanitize_sql(sql)
  res <- request_postpass("interpreter", sql, options)
  parse_postpass(res, geojson = geojson, unwrap = unwrap)
}


explain <- function(x) {
  options = list(geojson = FALSE, collection = FALSE)
  sql <- dbplyr::sql_render(x)
  sql <- sanitize_sql(sql)
  res <- request_postpass("explain", sql, options)
  httr2::resp_body_string(res)
}


#' Send SQL to Postpass
#' @description
#' Low-level function to send an SQL query directly to Postpass.
#'
#' @param sql Character string containing a valid Postpass SQL query.
#' @param pretty Whether to return a pretty unformatted JSON. Ignored if
#' \code{parse} is \code{TRUE}.
#' @param parse Whether to parse the response as a (sf) tibble. If \code{FALSE},
#' returns the unparsed JSON as a character string.
#' @inheritParams collect.pp_tbl
#' @returns A (sf) tibble or a character string if \code{parse} is \code{FALSE}.
#' @inheritSection collect.pp_tbl Limitations
#'
#' @examples
#' \donttest{sql <- "
#' SELECT
#'   name, way
#' FROM
#'   planet_osm_point
#' WHERE
#'   amenity = 'fast_food'
#'   AND tags -> 'addr:city' = 'Karlsruhe'
#' "
#'
#' postpass(sql)}
postpass <- function(sql,
                     geojson = collection,
                     collection = TRUE,
                     pretty = FALSE,
                     parse = TRUE,
                     unwrap = TRUE,
                     endpoint = "interpreter") {
  options <- list(
    geojson = geojson,
    collection = collection,
    pretty = pretty
  )

  resp <- request_postpass(endpoint, sql, options)

  if (parse) {
    res <- parse_postpass(resp, geojson = geojson, unwrap = unwrap)
  } else {
    res <- httr2::resp_body_string(resp)
  }

  res
}


request_postpass <- function(endpoint = "interpreter", sql, options) {
  req <- httr2::request(postpass_url())
  req <- httr2::req_url_path_append(req, endpoint)
  args <- c(list(req), data = list(sql), explode_options(options))
  req <- do.call(httr2::req_body_form, args)
  req <- httr2::req_error(req, body = \(resp) httr2::resp_body_string(resp))
  httr2::req_perform(req)
}


parse_postpass <- function(resp, geojson, unwrap) {
  body <- httr2::resp_body_string(resp)

  if (!jsonlite::validate(body)) {
    return(body)
  }

  if (geojson) {
    res <- httr2::resp_body_string(resp)
    res <- sf::read_sf(res)
  } else {
    res <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    res <- dplyr::as_tibble(res$result)
  }

  if (unwrap) {
    res <- unwrap_tags(res)
  }

  res
}


#' Unwrap tags
#' @description
#' Helper function to turn the \code{tags} column returned by
#' \code{\link{collect}} to multiple columns in the same dataframe.
#'
#' @param x A dataframe returned by \code{\link{collect}} containing a
#' \code{tags} column.
#' @param keep Whether to keep the unparsed \code{tags} column or replace it.
#' @returns A (sf) dataframe.
#' @export
#'
#' @examples
#' \donttest{nc <- system.file("shape/nc.shp", package = "sf")
#' surry <- nc[nc$NAME %in% "Surry", ]
#'
#' res <- pp_tbl("point") |>
#'   filter(amenity == "fast_food" & way %&&% !!pg_bbox(surry)) |>
#'   select(name, way, tags) |>
#'   collect(unwrap = FALSE)
#'
#' unwrap_tags(res)}
unwrap_tags <- function(x, keep = FALSE) {
  if (!"tags" %in% colnames(x)) {
    return(x)
  }

  tags <- lapply(x$tags, jsonlite::fromJSON)
  tags <- replace_empty_tags(tags)
  tags <- dplyr::bind_rows(tags)
  where <- ifelse(keep, "after", "replace")
  insert_df(x, tags, position = "tags", where = where)
}


replace_empty_tags <- function(tags) {
  n_tags <- lengths(tags)
  dummy_key <- names(tags[n_tags][[1]])[1]
  dummy_tag <- list(NA)
  names(dummy_tag) <- dummy_key
  tags[!n_tags] <- list(dummy_tag)
  tags
}


explode_options <- function(options) {
  vals <- tolower(unlist(options))
  names(vals) <- sprintf("options[%s]", names(options))
  vals
}


sanitize_sql <- function(sql) {
  as.character(sql) |>
    gsub("`", "\"", x = _)
}
