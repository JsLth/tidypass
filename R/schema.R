schema_url <- function() {
  "https://raw.githubusercontent.com/woodpeck/postpass-ops/refs/heads/main/SCHEMA.md"
}

#' List available geometry tables
#' @description
#' Retrieve a vector of available geometry tables provided by Postpass.
#' Scraped from \href{https://github.com/woodpeck/postpass-ops/blob/main/SCHEMA.md}{SCHEMA.md}.
#'
#' @returns A vector.
#'
#' @export
#' @examples
#' list_tables()
list_tables <- function() {
  regex_match(names(schemas), "_([a-z]+)$", i = 2)
}

tbl_md <- function(x) {
  parsermd::as_tibble(parsermd::parse_rmd(x, parse_yaml = FALSE))
}

parse_schemas <- function() {
  md <- tbl_md(schema_url())
  md <- md[
    md$sec_h2 %in% "Main (Geometry) Tables" &
    !is.na(md$sec_h3) &
    md$type %in% "rmd_markdown",
  ]
  sch_names <- md$sec_h3

  schema_tbl <- lapply(md$ast, function(schema) {
    clean <- schema |>
      trimws() |>
      gsub(",", "", x = _) |>
      gsub("\"", "", x = _) |>
      as.vector()
    clean <- clean[seq(which(startsWith(clean, "CREATE TABLE")), length(clean))]
    cols <- gsub(" .*", "", x = clean)
    ptypes <- gsub(".* ", "", x = clean)

    # no idea how to handle public.hstore and geometries
    filt <- !ptypes %in% c("(", ");", "")

    cols <- cols[filt]
    ptypes <- ptypes[filt]
    ptypes[startsWith(ptypes, "public.geometry")] <- "geometry"
    ptypes[startsWith(ptypes, "public.hstore")] <- "hstore"


    ptypes <- lapply(
      ptypes,
      switch,
      text = character(1),
      integer = integer(1),
      real = double(1),
      geometry = new_pg_geom(1),
      hstore = new_pg_hstore(1),
      bigint = new_pg_bigint(1)
    )
    names(ptypes) <- cols

    do.call(dplyr::tibble, ptypes)
  })
  names(schema_tbl) <- sch_names
  schema_tbl
}

new_pg_geom <- function(length = 0L) {
  obj <- rep("ST_GeomFromText('POINT EMPTY')", length)
  obj <- lapply(obj, str2lang)
  class(obj) <- c("pg_geom", "list")
  obj
}

new_pg_hstore <- function(length = 0L) {
  obj <- vector("list", length = length)
  class(obj) <- c("pg_hstore", "list")
  obj
}

new_pg_bigint <- function(length = 0L) {
  obj <- as.character(vector("integer", length = length))
  class(obj) <- "pg_bigint"
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
print.pg_bigint <- function(x, ...) {
  print(noquote(unclass(x)), quote = FALSE)
}
