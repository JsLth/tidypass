#' @export
mutate.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @export
filter.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


unwrap_expr <- function(.data, ...) {
  exprs <- rlang::enexprs(...)
  vars <- unlist(lapply(exprs, all.vars))
  vars <- setdiff(vars, names(.data$lazy_query$x))
  vars <- lapply(vars, as.name)
  unwrap(.data, !!!vars)
}
