#' Postpass utilities
#' @description
#' Some possibly useful functions to navigate the PostGIS database behind
#' Postpass.
#'
#' \itemize{
#'  \item{\code{pp_last_import} returns the date-time of the last OSM import.}
#'  \item{\code{pp_tables} returns the names of the database tables.}
#'  \item{\code{pp_schema} returns the schema of a given table.}
#'  \item{\code{pp_columns} returns the column names of a given table.}
#'  \item{\code{pp_peek} returns the first few rows of a given table.}
#' }
#'
#' @param table A table name. A list of names can be retrieved using
#' \code{pp_tables}.
#' @param limit Number of rows to return. Defaults to 10.
#'
#' @returns \code{pp_last_import} returns a date-time object. \code{pp_tables}
#' and \code{pp_columns} return a character vector. \code{pp_peek} and
#' \code{pp_schema} return a dataframe.
#'
#' @export
#' @name postpass_utils
#'
#' @examples
#' \donttest{pp_last_import()
#' pp_tables()
#' pp_schema("planet_osm_point")
#' pp_columns("planet_osm_point")
#' pp_peek("planet_osm_point", 5)}
pp_last_import <- function() {
  res <- postpass(
    "SELECT
      value as timestamp
    FROM
      osm2pgsql_properties
    WHERE
      property = 'import_timestamp'",
    collection = FALSE,
    geojson = FALSE
  )

  as.POSIXct(res, format = "%Y-%m-%dT%H:%M:%OSZ")
}


#' @rdname postpass_utils
#' @export
pp_tables <- function() {
  res <- setdiff(postpass(
    "SELECT
      *
    FROM
      pg_catalog.pg_tables
    WHERE
      schemaname = 'public'
      AND tableowner = 'osm'",
    geojson = FALSE
  )$tablename, "osm2pgsql_properties")
}


#' @rdname postpass_utils
#' @export
pp_schema <- function(table) {
  res <- postpass(
    sprintf(
      "SELECT
        column_name AS column, udt_name AS type, table_name AS table
      FROM
        INFORMATION_SCHEMA.COLUMNS
      WHERE
        TABLE_NAME = '%s'",
      table
    ),
    geojson = FALSE
  )

  attr(res, "postpass_table") <- unique(res$table)
  res$table <- NULL
  res
}


#' @rdname postpass_utils
#' @export
pp_columns <- function(table) {
  postpass(
    sprintf(
      "SELECT
        COLUMN_NAME
      FROM
        INFORMATION_SCHEMA.COLUMNS
      WHERE
        TABLE_NAME = '%s'",
      table
    ),
    geojson = FALSE
  )$column_name
}


#' @rdname postpass_utils
#' @export
pp_peek <- function(table, limit = 10) {
  if (limit > 1000) {
    rlang::abort(paste(
      "`pp_peek()` is for peeking.",
      "To return larger amounts of data, see ?collect."
    ))
  }

  postpass(
    sprintf("SELECT * FROM %s LIMIT %s", table, limit),
    geojson = FALSE,
    unwrap = FALSE
  )
}
