#' Eat columns from another data frame
#'
#' Modified left_join where only a specified subset of \code{y} is kept, with
#'   optional checks and transformations.
#'
#' Character codes are the same as for `safe_*_join` functions, with the
#' addition of `"d"`to check if dots were filled.
#'
#' @inheritParams dplyr::left_join
#' @inheritParams safe_joins
#' @param .x,.y	tbls to join
#' @param ... One or more unquoted expressions, passed to \code{dplyr::select},
#'   defining the columns to keep from \code{y}
#' @param .by a character vector of variables to join by.
#' @param .agg function or formula or \code{NULL}, if not \code{NULL}, \code{y}
#'   will be grouped by its \code{by} columns and \code{fun} will be applied to
#'   all kept columns from {y}
#'
#' @param .check a string, see details about `check` parameter in ?safe_joins
#' @param .conflict if `NULL`, in case of column conflict both columns are
#'   suffixed as in *dplyr*, if a function of two parameters or a formula,
#'   a function is applied on both columns. If the string "patch", matched
#'   values from `y` will overwrite existing values in `x` while the other
#'   values will be kept`
#' @param .prefix prefix of new columns or function/formula to apply on names of new
#'   columns
#' @return a data frame
#' @export
eat <- function(.x, .y, ..., .by = NULL, .agg = NULL,
                .check = "~blC",
                .conflict = NULL,
                .prefix = NULL) {

  l <- safe_check(.x, .y, .by, .check, .conflict,
                  in_eat = TRUE, .agg, .prefix, ...)
  res <- dplyr::left_join(l$x,l$y, by = setNames(l$by$y,l$by$x))
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}



