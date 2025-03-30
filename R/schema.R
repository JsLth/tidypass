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

