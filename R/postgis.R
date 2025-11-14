#' PostGIS utilities
#' @description
#' \code{dbplyr} does not recognize PostGIS commands by default. These functions
#' fill this gap by providing simple wrappers that translate sf to PostGIS.
#' These functions return calls and are designed to be used inside a tidy
#' evaluation context.
#'
#' Currently supported:
#'
#' \itemize{
#'  \item{\code{pg_geom}: Create a PostGIS geometry from an \code{sfc} object (\code{st_geomfromtext})}
#'  \item{\code{pg_bbox}: Compute a PostGIS boundary box from an \code{sfc} object (\code{st_makebox2d})}
#'  \item{\code{\%&&\%}: Check if bbox of \code{x} intersects with bbox of \code{y}}
#'  \item{\code{\%<->\%}: Returns the Euclidean distance between \code{x} and \code{y}}
#' }
#'
#' Note that in order to use any of these functions inside a dplyr query, you
#' need to inject them using the \code{\link[rlang]{!!}}. To use PostGIS
#' operators (e.g. \code{&&}, \code{&>} or \code{>>}), wrap them as infix
#' operators, i.e.
#'
#' \preformatted{way \%&&\% {!!pg_bbox(geometry)}}
#'
#' @param x A \code{\link[sf:st_sfc]{sfc}} object or an object coercible to
#' \code{sfc}.
#'
#' @returns Objects of class \code{call}.
#'
#' @export
#' @name postgis
#'
#' @examples
#' library(sf)
#' nc <- system.file("shape/nc.shp", package = "sf")
#'
#' pg_geom(nc[1,])
#' pg_bbox(nc)
filter_spatial <- function(.data, geom, .predicate = "intersects") {
  .predicate <- to_title(.predicate)
  .predicate <- sprintf("ST_%s", .predicate)

  if (inherits(geom, c("sf", "sfc", "SpatVector"))) {
    geom <- sf::st_geometry(sf::st_as_sf(geom))
    geom <- pg_geom(geom)
  } else {
    geom <- pg_bbox(geom)
  }

  filter(.data, dbplyr::sql(sprintf("%s(%s, %s)", .predicate, "geom", geom)))
}


pg_geom <- function(x) {
  x <- sf::st_geometry(x)
  if (length(x) > 1) x <- sf::st_union(x)
  wkt <- sf::st_as_text(x)
  epsg <- sf::st_crs(x)$epsg %|||% 4326
  dbplyr::sql(sprintf("ST_GeomFromText('%s', %s)", wkt, epsg))
}

#' @rdname postgis
#' @export
pg_bbox <- function(x) {
  if (is.numeric(x) && !length(names(x))) {
    names(x) <- c("xmin", "xmax", "ymin", "ymax")
  }

  if (is.numeric(x) && length(names(x))) {
    x <- x[c("xmin", "xmax", "ymin", "ymax")]
  }

  bbox <- sf::st_bbox(x)
  dbplyr::sql(st_makebox(bbox, crs = sf::st_crs(bbox)$epsg %|||% 4326))
}


pg_place <- function(x, ..., osm_tag = "boundary", box = FALSE) {
  entity <- photon::geocode(x, osm_tag = osm_tag, ...)

  if (!nrow(entity)) {
    cli::cli_abort(c(
      "Geocoding was not successful.",
      "i" = "Retrieved 0 elements matching {.val {x}}."
    ))
  }

  pp_tbl("polygon") |>
    select(osm_id, tags, geom) |>
    filter(
      dplyr::sql(sprintf("CAST(osm_id AS CHAR) = '%s'", !!entity$osm_id)),
      #name %LIKE% !!entity$name,
      !is.na(boundary)
    ) |>
    head() |>
    select(geom)
}


#' @rdname postgis
#' @export
"%&&%" <- function(x, y) pg_op(x, y, "&&")

#' @rdname postgis
#' @export
"%<->%" <- function(x, y) pg_op(x, y, "<->")

st_makebox <- function(bbox, crs = 4326) {
  epsg <- sf::st_crs(crs)$epsg
  xmin <- bbox["xmin"] %|||% bbox[1]
  ymin <- bbox["ymin"] %|||% bbox[2]
  xmax <- bbox["xmax"] %|||% bbox[3]
  ymax <- bbox["ymax"] %|||% bbox[4]
  sprintf(
    "st_setsrid(st_makebox2d(st_makepoint(%s, %s), st_makepoint(%s, %s)), %s)",
    xmin, ymin, xmax, ymax, epsg
  )
}
