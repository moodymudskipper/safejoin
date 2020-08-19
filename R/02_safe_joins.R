#' Safe joins
#'
#' Wrappers around *dplyr*'s joining function that allow to check a variety of
#' things on the fly and either `inform`, `warn` or `abort` as a result.
#'
#' `check` is a combination of characters which will trigger different checks:
#' \describe{
#'   \item{b}{as in **b**y, check that `by` was given explicitly. Default behavior
#'     in *dplyr* is to trigger a message}
#'   \item{c}{as in **c**olumn **c**onflict, check if, among non join
#'   columns, some column names are found in both `x` and `y`. Default behavior
#'   in *dplyr*'s joining functions is to suffix them silently.}
#'   \item{u}{as in **u**nique, check if no set of values of join columns
#'   is duplicated in `x`}
#'   \item{v}{the letter after **u**, check if no set of values of join columns
#'   is duplicated in `y`}
#'   \item{m}{as in **m**atch, check if all sets of values of join columns in
#'    `x` wil be matched in `y`}
#'   \item{n}{the letter after **m**, check if all sets of values of join columns in
#'    `y` wil be matched in `x`}
#'   \item{e}{as in **e**xpanded, check that all combinations of values of
#'     join columns are present in `x`}
#'   \item{f}{the letter after **e**, check that all combinations of values of
#'     join columns are present in `y`}
#'   \item{l}{as in **l**evels, check that join columns are consistent in term of
#'     factor levels}
#'   \item{t}{as in  **t**ype, check that joining columns have same class and type}
#' }
#'
#' An upper case letter will trigger `abort`, a lower case letter will trigger
#'   `warn`, a lower case letter prefixed with `~` will trigger a message. Other
#'   characters will be ignored.
#'
#' @inheritParams dplyr::join
#' @inheritParams dplyr::nest_join
#' @param x,y	tbls to join
#' @param check a string, see details
#' @param match_fun	Vectorized function
#'   given two columns, returning `TRUE` or `FALSE` as to whether they are a
#'   match. Can be a list of functions one for each pair of columns specified in
#'   by (if a named list, it uses the names in x). If only one function is given
#'    it is used on all column pairs.
#' @param conflict if `NULL`, in case of column conflict both columns are
#'   suffixed as in *dplyr*, if a function of two parameters or a formula,
#'   a function is applied on both columns. If the string "patch", matched
#'   values from `y` will overwrite existing values in `x` while the other
#'   values will be kept
#'
#' @name safe_joins
NULL

library(rlang)
safe_join <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL,
                      suffix = c(".x", ".y"),
                      na_matches = c("na", "never"),
                      match_fun = NULL,
                      check = "~blC", conflict = NULL,
                      mode = c("left","right","inner","full","semi","anti","nest")){
  na_matches <- match.arg(na_matches)
  mode <- match.arg(mode)
  join <- utils::getFromNamespace(paste0(mode,"_join"), "dplyr")

  ##############################
  # HANDLE FORMULA INPUT OF BY #
  ##############################
  multi_by <- NULL
  if (inherits(by, "formula")) {
    multi_by <- lapply(extract_xy_content(by), unique)
    multi_by$x <- purrr::map_chr(multi_by$x,~ (
      if (is.numeric(.)) names(x)[.] else .))
    multi_by$y <- purrr::map_chr(multi_by$y, ~ (
      if(is.numeric(.)) names(y)[.] else .))
    if (!is.null(match_fun))
      rlang::abort("Don't provide match_fun if you specify by as a formula")
    match_fun <- by
    by <- multi_by
  }
  ###############
  # MAIN CHECKS #
  ###############
  L <- safe_check(x = x, y = y, by = by, check = check, conflict = conflict,
                  suffix = suffix, match_fun = match_fun, agg = NULL, prefix = NULL, dots = NULL)
  x <- L$x
  y <- L$y
  by    <- L$by
  patch <- L$patch
  apply_conflict_fun <- L$apply_conflict_fun
  conflict_fun       <- L$conflict_fun
  conflicted_nms     <- L$conflicted_nms
  if (!is.null(match_fun)) {
    ###############
    # FUZZY JOINS #
    ###############
    if (is.null(multi_by)) {
      # standard fuzzy join
      res <- fuzzy_join(
        x, y, by = `names<-`(by$y,by$x),
        match_fun = rlang::as_function(match_fun),
        mode = mode)
    } else {
      # using multi_by or safejoin formula notation
      res <- fuzzy_join(
        x, y,
        multi_by = by,
        multi_match_fun = rlang::as_function(match_fun),
        mode = mode)
      check_fuzzy_conflicts(res, check, x , y)
    }
  } else if (mode %in% c("left","right","inner","full")) {
    ##################
    # STANDARD JOINS #
    ##################
    res <- join(
      x, y, by = `names<-`(by$y,by$x), copy, suffix, na_matches = na_matches)
  } else if (mode %in% c("semi","anti")) {
    ###################
    # SEMI/ANTI JOINS #
    ###################
    res <- join(
      x, y, by = `names<-`(by$y,by$x), copy, na_matches = na_matches)
  } else if (mode == "nest") {
    #############
    # NEST JOIN #
    #############
    res <- join(
      x, y, by = `names<-`(by$y,by$x), copy, keep, name)
  }

  #####################
  # RESOLVE CONFLICTS #
  #####################
  res <- resolve_conflicts(
    res, patch, apply_conflict_fun, conflict_fun, conflicted_nms)
  res
}


frmls <- formals(safe_join)
frmls$mode <- NULL
frmls$keep <- NULL
frmls$name <- NULL

#' @export
#' @rdname safe_joins
safe_left_join <- rlang::new_function(
  frmls, expr(safe_join(!!!rlang::set_names(syms(names(frmls))), mode = "left")))

#' @export
#' @rdname safe_joins
safe_right_join <- rlang::new_function(
  frmls, expr(safe_join(!!!rlang::set_names(syms(names(frmls))), mode = "right")))

#' @export
#' @rdname safe_joins
safe_inner_join <- rlang::new_function(
  frmls, expr(safe_join(!!!rlang::set_names(syms(names(frmls))), mode = "inner")))

#' @export
#' @rdname safe_joins
safe_full_join <- rlang::new_function(
  frmls, expr(safe_join(!!!rlang::set_names(syms(names(frmls))), mode = "full")))

##################
# SEMI AND ANTI  #
##################

frmls$suffix <- NULL
#' @export
#' @rdname safe_joins
safe_semi_join <- rlang::new_function(
  frmls, expr(safe_join(!!!rlang::set_names(syms(names(frmls))), mode = "semi")))

#' @export
#' @rdname safe_joins
safe_anti_join <- rlang::new_function(
  frmls, expr(safe_join(!!!rlang::set_names(syms(names(frmls))), mode = "anti")))

################
#     NEST     #
################

frmls <- formals(safe_join)
frmls$mode <- NULL
frmls$suffix <- NULL
frmls$na_matches <- NULL

#' @export
#' @rdname safe_joins
safe_nest_join <- rlang::new_function(
  frmls, expr(safe_join(!!!rlang::set_names(syms(names(frmls))), mode = "nest")))

rm(frmls)
