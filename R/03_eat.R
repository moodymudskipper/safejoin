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
#' @param .na_matches	 Use `"never"` to always treat two `NA` or `NaN` values as
#'   different, like joins for database sources, similarly to
#'   `merge(incomparables = FALSE)`. The default, `"na"`, always treats two `NA`
#'    or `NaN` values as equal, like `merge()`. Users and package authors can
#'    change the default behavior by calling
#'    `pkgconfig::set_config("dplyr::na_matches" = "never")`.
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
#' @param .join type of join o build on, a left join by default
#' @return a data frame
#' @export
eat <- function(.x, .y, ..., .by = NULL,
                .na_matches = pkgconfig::get_config("dplyr::na_matches"),
                .agg = NULL,
                .check = "~blC",
                .conflict = NULL,
                .prefix = NULL,
                .join = c("left_join","right_join","inner_join","full_join")) {

  .join <- match.arg(.join)
  join_fun <- getFromNamespace(.join, "dplyr")
  l <- safe_check(.x, .y, .by, .check, .conflict,
                  in_eat = TRUE, .agg, .prefix, eval(substitute(alist(...))))
  res <- join_fun(l$x,l$y, by = `names<-`(l$by$y,l$by$x),
                          na_matches = .na_matches)
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}

# debugonce(eat)
# debugonce(safe_check)

