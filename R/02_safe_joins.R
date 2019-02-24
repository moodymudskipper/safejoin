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
#' }
#'
#' An upper case letter will trigger `abort`, a lower case letter will trigger
#'   `warn`, a lower case letter prefixed with `~` will trigger a message. Other
#'   characters will be ignored.
#'
#' @inheritParams dplyr::join
#' @param x,y	tbls to join
#' @param check a string, see details
#' @param conflict if `NULL`, in case of column conflict both columns are
#'   suffixed as in *dplyr*, if a function of two parameters or a formula,
#'   a function is applied on both columns. If the string "patch", matched
#'   values from `y` will overwrite existing values in `x` while the other
#'   values will be kept
#'
#'
#' @name safe_joins
NULL

#' @export
#' @rdname safe_joins
safe_left_join <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...,
                           check = "~blC", conflict = NULL){
  l <- safe_check(x, y, by, check, conflict)
  res <- dplyr::left_join(l$x, l$y, by = `names<-`(l$by$y,l$by$x), copy, suffix, ...)
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}

#' @export
#' @rdname safe_joins
safe_right_join <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...,
                           check = "~blC", conflict = NULL) {
  l <- safe_check(x, y, by, check, conflict)
  res <- dplyr::right_join(l$x, l$y, by = `names<-`(l$by$y,l$by$x), copy,
                   suffix = c(".x", ".y"), ...)
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}

#' @export
#' @rdname safe_joins
safe_inner_join <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...,
                           check = "~blC", conflict = NULL
) {

  l <- safe_check(x, y, by, check, conflict)
  res <- dplyr::inner_join(l$x, l$y, by = `names<-`(l$by$y,l$by$x), copy,
                   suffix = c(".x", ".y"), ...)
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}

#' @export
#' @rdname safe_joins
safe_full_join <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...,
                           check = "~blC", conflict = NULL
) {

  l <- safe_check(x, y, by, check, conflict)
  res <- dplyr::full_join(l$x, l$y, by = `names<-`(l$by$y,l$by$x), copy,
                   suffix = c(".x", ".y"), ...)
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}

#' @export
#' @rdname safe_joins
safe_semi_join <- function(x, y, by = NULL, copy = FALSE, ...,
                           check = "~blC", conflict = NULL
) {

  l <- safe_check(x, y, by, check, conflict)
  res <- dplyr::semi_join(l$x, l$y, by = `names<-`(l$by$y,l$by$x), copy, ...)
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}

#' @export
#' @rdname safe_joins
safe_anti_join <- function(x, y, by = NULL, copy = FALSE, ...,
                           check = "~blC", conflict = NULL) {
  l <- safe_check(x, y, by, check, conflict)
  res <- dplyr::anti_join(l$x, l$y, by = `names<-`(l$by$y,l$by$x), copy, ...)
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}

#' @export
#' @rdname safe_joins
safe_nest_join <- function(x, y, by = NULL, copy = FALSE, keep = FALSE,
                           name = NULL, ..., check = "~blC", conflict = NULL){
  l <- safe_check(x, y, by, check, conflict)
  res <- dplyr::nest_join(l$x, l$y, by = `names<-`(l$by$y,l$by$x), copy, keep, name, ...)
  res <- resolve_conflicts(
    res, l$patch, l$apply_conflict_fun, l$conflict_fun, l$common_aux)
  res
}
