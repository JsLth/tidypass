postpass_url <- function() "https://postpass.geofabrik.de/api/0.1/interpreter"

pp_tbl <- function(table) {
  table <- sprintf("planet_osm_%s", table)
  tbl <- dbplyr::tbl_lazy(
    schema_tbl[[table]],
    con = dbplyr::simulate_postgres(),
    name = table
  )
  class(tbl) <- c("pp_tbl", class(tbl))
  tbl
}


#' @exportS3Method dplyr::collect
collect.pp_tbl <- function(x, geometry = TRUE, collection = TRUE, ...) {
  options <- list(geometry = geometry, collection = collection, ...)
  sql <- dplyr::collapse(x)
  resp <- request_postpass(x, options)

  if (geometry) {
    resp <- httr2::resp_body_string(resp)
    sf::read_sf(resp)
  } else {
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  }
}


request_postpass <- function(sql, options) {
  req <- httr2::request(postpass_url())
  req <- httr2::req_url_query(
    data = sql,
    options = options
  )

  httr2::req_perform(req)
}
