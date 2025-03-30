#' List available geometry tables
#' @description
#' Retrieve a vector of available geometry tables provided by Postpass.
#' Should almost always return the same tables as \code{\link{pp_tables}} but
#' can be run locally. Also abbreviates the table names for convenience.
#'
#' @returns A vector.
#'
#' @export
#' @examples
#' list_tables()
local_tables <- function() {
  regex_match(names(schemas), "_([a-z]+)$", i = 2)
}


schema_to_tbl <- function(schema) {
  if (!all(c("type", "column") %in% names(schema))) {
    rlang::abort(c(
      "Invalid schema provided. Columns \"type\" or \"column\" are missing.",
      "i" = "You can retrieve schemas using `pp_peek()`."
    ))
  }

  types <- lapply(
    schema$type,
    switch,
    int4 = integer(1),
    int8 = bigint(1),
    text = character(1),
    jsonb = new_pg_jsonb(1),
    hstore = new_pg_hstore(1),
    geometry = new_pg_geom(1)
  )
  names(types) <- schema$column
  dplyr::as_tibble(types)
}


new_pg_geom <- function(length = 0L) {
  obj <- rep("ST_GeomFromText('POINT EMPTY')", length)
  class(obj) <- c("pg_geom", "list")
  obj
}

new_pg_hstore <- function(length = 0L) {
  obj <- vector("list", length = length)
  class(obj) <- c("pg_hstore", "list")
  obj
}

new_pg_jsonb <- function(length = 0L) {
  obj <- vector("list", length = length)
  class(obj) <- c("pg_jsonb", "list")
  obj
}

#' @export
print.pg_geom <- function(x, ...) {
  cat("<pg_geom>", sep = "\n")
}

#' @export
print.pg_hstore <- function(x, ...) {
  cat("<pg_hstore>", sep = "\n")
}

#' @export
print.pg_jsonb <- function(x, ...) {
  cat("<pg_jsonb>", sep = "\n")
}
