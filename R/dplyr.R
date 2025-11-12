#' @exportS3Method dplyr::mutate
#' @importFrom dplyr mutate
mutate.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::transmute
#' @importFrom dplyr transmute
transmute.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::summarise
#' @importFrom dplyr summarise
summarise.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::summarize
#' @importFrom dplyr summarize
summarize.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::group_by
#' @importFrom dplyr group_by
group_by.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::filter
#' @importFrom dplyr filter
filter.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::select
#' @importFrom dplyr select
select.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::relocate
#' @importFrom dplyr relocate
relocate.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::arrange
#' @importFrom dplyr arrange
arrange.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::distinct
#' @importFrom dplyr distinct
distinct.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::rename
#' @importFrom dplyr rename
rename.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' @exportS3Method dplyr::rename_with
#' @importFrom dplyr rename_with
rename_with.pp_tbl <- function(.data, ...) {
  .data <- unwrap_expr(.data, ...)
  NextMethod()
}


#' Given a Postpass query, extracts all unknown variables (that are neither
#' in the postpass scheme nor in the search path) from the tags jsonb
#' @param .data A `pp_tbl`.
#' @param ... Expressions.
#' @returns A `pp_tbl`.
unwrap_expr <- function(.data, ...) {
  quos <- rlang::enquos(...)
  env <- rlang::quo_get_env(quos[[1]])
  vars <- unique(unlist(lapply(quos, vars_from_expr)))
  tbl_syms <- names(.data$lazy_query$x)
  vars <- setdiff(vars, tbl_syms)
  vars <- lapply(vars, as.name)
  unwrap(.data, !!!vars)
}


#' Extracts all vars from a quosure that cannot be found in the search path
#' @param q A quosure.
vars_from_expr <- function(q) {
  env <- rlang::quo_get_env(q)
  vars <- all.vars(rlang::quo_get_expr(q))
  Filter(function(v) !var_known(v, env), vars)
}


#' Determines whether a symbol is found in the search path of an environment
var_known <- function(sym, env) {
  rlang::env_has(env, sym, inherit = TRUE)
}
