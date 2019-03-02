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
                .match_fun = NULL,
                .agg = NULL,
                .check = "~blC",
                .conflict = NULL,
                .prefix = NULL,
                .mode = c("left","right","inner","full")) {

  .mode <- match.arg(.mode)

  ##############################
  # HANDLE FORMULA INPUT OF BY #
  ##############################
  multi_by <- NULL
  if (inherits(.by, "formula")) {
    multi_by <- lapply(extract_xy_content(.by), unique)
    multi_by$x <- purrr::map_chr(
      multi_by$x, ~if(is.numeric(.)) ..2[.] else ., names(.x))
    multi_by$y <- purrr::map_chr(
      multi_by$y, ~if(is.numeric(.)) ..2[.] else ., names(.y))
    if (!is.null(.match_fun))
      rlang::abort("Don't provide .match_fun if you specify by as a formula")
    .match_fun <- .by
    .by <- multi_by
  }

  ###############
  # MAIN CHECKS #
  ###############
  L <- safe_check(x = .x, y = .y, by = .by, match_fun = .match_fun,
                  check = .check, conflict = .conflict, in_eat = TRUE,
                  suffix = c(".x",".y"),
                  agg = .agg, prefix = .prefix, dots = eval(substitute(alist(...))))
  with(L,{
    if (!is.null(.match_fun)) {
      ###############
      # FUZZY JOINS #
      ###############
      if (is.null(multi_by)) {
        # standard fuzzy join
        res <- fuzzyjoin:::fuzzy_join(
          x, y, by = `names<-`(by$y,by$x),
          match_fun = rlang::as_function(.match_fun),
          mode = .mode)
      } else {
        # using multi_by or safejoin formula notation
        res <- fuzzyjoin::fuzzy_join(
          x, y,
          multi_by = multi_by,
          multi_match_fun = rlang::as_function(.match_fun),
          mode = .mode)
        check_fuzzy_conflicts(res, .check, x, y)
      }
    } else {
      ##################
      # STANDARD JOINS #
      ##################
      join <- getFromNamespace(paste0(.mode,"_join"), "dplyr")
      res <- join(
        x, y, by = `names<-`(by$y,by$x), na_matches = .na_matches)
    }
    #####################
    # RESOLVE CONFLICTS #
    #####################
    res <- resolve_conflicts(
      res, patch, apply_conflict_fun, conflict_fun, conflicted_nms)
    res
  })
}


eat.list <- function(.x, .y, ..., .by = NULL,
                     .na_matches = pkgconfig::get_config("dplyr::na_matches"),
                     .agg = NULL,
                     .check = "~blC",
                     .conflict = NULL,
                     .prefix = NULL,
                     .mode = c("left","right","inner","full")){
  Reduce(function(x,y) eat(x,y, ..., .by = .by, .agg = .agg, .check = .check,
                           .conflict = .conflict, .prefix = .prefix,
                           .join = .join),.y, .x)
}
#
# debugonce(eat)
# eat.list(band_instruments,list(band_members, band_members),.by="name", .prefix = "Z")
# # debugonce(eat)
# # debugonce(safe_check)
#
