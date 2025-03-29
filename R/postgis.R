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
#'  \item{\code{pg_geom}: \code{st_geomfromtext} using \code{\link[sf:st_sfc]{sfc}}}
#'  \item{\code{pg_bbox}: \code{st_makebox2d} using \code{\link[sf]{st_bbox}}}
#' }
#'
#' Note that to use any of these functions inside a dplyr query, you need to
#' defuse them using the \link[rlang:!!]{bang-bang operator} (\code{!!}). To use
#' PostGIS operators (e.g. \code{&&}, \code{&>} or \code{>>}), wrap them as
#' infix operators, i.e.
#'
#' \preformatted{way \%&&\% {!!pg_bbox(geometry)}}
#'
#' @param x A \code{\link[sf:st_sfc]{sfc}} object or an object coercible to
#' \code{sfc}.
#'
#' @returns Objects of class \code{call}.
#'
#' @export
#'
#' @examples
#' library(sf)
#' nc <- system.file("shape/nc.shp", package = "sf")
#'
#' pg_geom(nc[1,])
#' pg_bbox(nc)
pg_geom <- function(x) {
  wkt <- sf::st_as_text(sf::st_geometry(x))
  epsg <- sf::st_crs(x)$epsg %|||% 4326
  str2lang(sprintf("ST_GeomFromText('%s', %sL)", wkt, epsg))
}

#' @rdname pg_geom
#' @export
pg_bbox <- function(x) {
  bbox <- sf::st_bbox(x)
  str2lang(st_makebox(bbox, crs = sf::st_crs(x)$epsg %|||% 4326))
}

st_makebox <- function(bbox, crs = 4326) {
  epsg <- sf::st_crs(crs)$epsg
  xmin <- bbox["xmin"] %|||% bbox[1]
  ymin <- bbox["ymin"] %|||% bbox[2]
  xmax <- bbox["xmax"] %|||% bbox[3]
  ymax <- bbox["ymax"] %|||% bbox[4]
  sprintf(
    "st_setsrid(st_makebox2d(st_makepoint(%s, %s), st_makepoint(%s, %s)), %sL)",
    xmin, ymin, xmax, ymax, epsg
  )
}
