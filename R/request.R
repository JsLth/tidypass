postpass_url <- function() "https://postpass.geofabrik.de/api/0.1/interpreter"

#' Postpass tibble
#' @description
#' Creates a lazy tibble based on a simulated Postgres database and a Postpass
#' database schema. This tibble can be manipulated using \code{dplyr} functions.
#' Each operation extends the SQL query which is then sent to the Postpass
#' server by running \code{\link{collect}}. An object
#'
#' @param table A table name (or at least its suffix). You can retrieve a list
#' of available table names by running \code{\link{list_tables}} or by taking
#' a look at \href{https://github.com/woodpeck/postpass-ops/blob/main/SCHEMA.md}{SCHEMA.md}.
#' @param schema Optionally, a schema dataframe. A schema should contain only
#' columns accessible in the remote PostGIS database along with their prototype
#' classes. If \code{NULL}, infers a schema from \code{table}.
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
pp_tbl <- function(table = list_tables(), schema = NULL) {
  if (is.null(schema)) {
    rlang::arg_match(table)
    table <- sprintf("planet_osm_%s", table)
    schema <- schemas[[table]]
  } else {
    table <- NULL
  }

  tbl <- dbplyr::tbl_lazy(
    schema,
    con = dbplyr::simulate_postgres(),
    name = table %||% table_name()
  )
  class(tbl) <- c("pp_tbl", class(tbl))
  tbl
}


sanitize_sql <- function(sql) {
  gsub("\n", " ", sql) |>
    gsub("`", "", x = _)
}


#' Send SQL query to Postpass
#' @description
#' Accepts a \code{dbplyr} query, sanitizes it, and sends it to Postpass.
#' If the query is valid, returns a (sf) tibble.
#'
#' @param x A lazy tibble of class \code{\link{pp_tbl}}.
#' @param geojson Whether to generate a GeoJSON or not. If \code{TRUE}, returns
#' an sf tibble, otherwise a normal tibble.
#' @param collection Whether to aggregate results to a JSON or not
#' \href{https://www.postgresql.org/docs/9.5/functions-aggregate.html}{\code{json_agg}}.
#' If \code{FALSE}, a scalar must be returned by \code{x}.
#'
#' @returns A (sf) tibble.
#'
#' @exportS3Method dplyr::collect
collect.pp_tbl <- function(x, geojson = TRUE, collection = TRUE, ...) {
  options <- list(geojson = geojson, collection = collection, ...)
  sql <- dbplyr::sql_render(x)
  sql <- sanitize_sql(sql)
  resp <- request_postpass(sql, options)

  if (geojson) {
    resp <- httr2::resp_body_string(resp)
    sf::read_sf(resp)
  } else {
    resp <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    dplyr::as_tibble(resp$result)
  }
}


request_postpass <- function(sql, options) {
  req <- httr2::request(postpass_url())
  req <- do.call(httr2::req_url_query, c(
    list(req), data = sql, explode_options(options)
  ))

  cli::cli_verbatim(req$url)

  req <- httr2::req_error(req, body = function(resp) {
    httr2::resp_body_string(resp)
  })
  httr2::req_perform(req)
}


explode_options <- function(options) {
  vals <- tolower(unlist(options))
  names(vals) <- sprintf("options[%s]", names(options))
  vals
}
