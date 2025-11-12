#' @rdname collect.pp_tbl
#' @exportS3Method dplyr::explain
#' @importFrom dplyr explain
explain.pp_tbl <- function(x) {
  options <- list(geojson = FALSE, collection = FALSE)
  sql <- dbplyr::sql_render(x)
  sql <- sanitize_sql(sql)
  res <- request_postpass("explain", sql, options)
  res <- httr2::resp_body_json(res, simplifyVector = TRUE)
  new_explain(res)
}


new_explain <- function(res) {
  cost <- res$plan$Plan$`Total Cost`
  rows <- res$plan$Plan$`Plan Rows`
  width <- res$plan$Plan$`Plan Width`

  out <-   list(
    total_cost = cost,
    rows = rows,
    width = width,
    queue = res$queue
  )
  class(out) <- "pp_explain"
  out
}


#' @export
format.pp_explain <- function(x, ...) {
  scanned <- format_bytes(pages_to_mb(x$total_cost))
  size <- format_bytes(x$rows * x$width)
  queue <- switch(
    x$queue,
    quick = cli::col_green("fast queue"),
    medium = cli::col_yellow("medium queue"),
    slow = cli::col_red("slow queue")
  )

  scanned <- sprintf("Data scanned: %s", scanned)
  size <- sprintf("Result size:  %s", size)
  queue <- sprintf("Queue:%s%s", strrep(" ", 8), queue)
  paste(scanned, size, queue, sep = "\n")
}


#' @export
print.pp_explain <- function(x, ...) {
  cat(format(x), "\n", ...)
  invisible(x)
}


pages_to_mb <- function(pages) {
  pages * 8 * 1024
}


format_bytes <- function(x) {
  sizes <- list(kb = 1024, mb = 1048576, gb = 1073741824)

  unit <- "bytes"
  if (x > sizes$gb) {
    unit <- "GB"
    x <- x / sizes$gb
  } else if (x > sizes$mb) {
    unit <- "MB"
    x <- x / sizes$mb
  } else if (x > sizes$kb) {
    unit <- "kB"
    x <- x / sizes$kb
  }

  sprintf("%s %s", nice(x), unit)
}
