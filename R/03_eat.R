#' Eat columns from another data frame
#'
#' Modified join where only a specified subset of \code{y} is kept, with
#'   optional checks and transformations.
#'
#' Character codes are the same as for `safe_*_join` functions, with the
#' addition of `"d"`to check if dots were filled.
#'
#' @param .x,.y	tbls to join
#' @param ... One or more unquoted expressions, passed to `dplyr::select`,
#'   defining the columns to keep from `.y`
#' @param .by a character vector of variables to join by, or a formula defining
#'   a joining condition using a notation such as
#'   `~ X("var1") > Y("var2") & X("var3") < Y("var4")`
#' @param .na_matches	 Use `"never"` to always treat two `NA` or `NaN` values as
#'   different, like joins for database sources, similarly to
#' @param .match_fun Vectorized function
#'   given two columns, returning `TRUE` or `FALSE` as to whether they are a
#'   match. Can be a list of functions one for each pair of columns specified in
#'   by (if a named list, it uses the names in `.x`). If only one function is given
#'    it is used on all column pairs.
#' @param .agg function or formula or `NULL`, if not `NULL`, `.y`
#'   will be grouped by its `.by` columns and `.agg` will be applied to
#'   all kept columns from `.y`.
#'
#' @param .check a string, see details about `check` parameter in ?safe_joins
#' @param .conflict if `NULL`, in case of column conflict both columns are
#'   suffixed as in *dplyr*, if a function of two parameters or a formula,
#'   a function is applied on both columns. If the string `"patch"`, matched
#'   values from `.y` will overwrite existing values in `.x` while the other
#'   values will be kept`
#' @param .prefix prefix of new columns or function/formula to apply on names of new
#'   columns
#' @param .fill if not `NULL`, value to fill missing values in eaten columns
#' @param .mode type of join on build on, a left join by default
#' @return a data frame
#' @export
eat <- function(.x, .y, ..., .by = NULL,
                .na_matches = c("na", "never"),
                .match_fun = NULL,
                .agg = NULL,
                .check = "~blC",
                .conflict = NULL,
                .prefix = NULL,
                .fill = NULL,
                .mode = c("left","right","inner","full")) {
  .na_matches <- match.arg(.na_matches)
  .mode <- match.arg(.mode)

  ######################
  # HANDLE LIST INPUTS #
  ######################

  # if .y is a list
  if (is.data.frame(.x) && is.list(.y) && !is.data.frame(.y)) {
    dots <- rlang::enquos(...)
    res <- purrr::reduce2(
      .y, rlang::names2(.y), .init = .x,
      ~ eat(
        .x, .y, !!!dots, .by = .by, .na_matches = .na_matches,
        .match_fun = .match_fun, .agg = .agg, .check = .check,
        .conflict = .conflict, .fill = .fill, .mode = .mode,
        # if list element is unnamed we'll use prefix to rename it
        .prefix = if(..3 != "") ..3 else .prefix))
    return(res)
  }

  # if .x is a list
  if (missing(.y) && is.list(.x) && !is.data.frame(.x)) {
    dots <- rlang::enquos(...)
    .y <- .x[-1]
    .x <- .x[[1]]
    res <- purrr::reduce2(
      .y, rlang::names2(.y), .init = .x,
      ~ eat(
        .x, .y, !!!dots, .by = .by, .na_matches = .na_matches,
        .match_fun = .match_fun, .agg = .agg, .check = .check,
        .conflict = .conflict, .fill = .fill, .mode = .mode,
        # if list element is unnamed we'll use prefix to rename it
        .prefix = if(..3 != "") ..3 else .prefix))
    return(res)
  }

  ###########################
  # HANDLE VARS INPUT OF BY #
  ###########################

  # # if .by is a single quosures (plural) element, put it in a list
  # if(inherits(.by, "quosures"))
  #   .by <- list(.by)

  # if .by contains quosure elements, put them together into one quosures element
  quosure_elements_lgl <- purrr::map_lgl(.by, inherits, "quosure")
  quosures_elements_lgl <- purrr::map_lgl(.by, inherits, "quosures")
  if (any(quosures_elements_lgl) || any(quosure_elements_lgl)) {
    quosures_elements <- c(unlist(.by[quosures_elements_lgl], FALSE),
                           .by[quosure_elements_lgl])
    quosures_elements <- as.list(quosures_elements)
    class(quosures_elements) <- "quosures"
    additional_by <-
      intersect(tbl_at_vars(.x, quosures_elements),
                tbl_at_vars(.y, quosures_elements))
    .by <- c(unclass(.by[!quosures_elements_lgl & !quosure_elements_lgl]),additional_by)
    # following has to go when we implement #33:
    .by <- unlist(.by)
    if (is.null(.by)) abort("No .by columns are compatible with given argument")
  }

  ##############################
  # HANDLE FORMULA INPUT OF BY #
  ##############################
  multi_by <- NULL
  if (inherits(.by, "formula")) {
    # get lists of indices of elements registered by X and Y
    multi_by <- lapply(extract_xy_content(.by), unique)
    # convert numeric indices to character
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
                  agg = .agg, prefix = .prefix, dots = rlang::enquos(...))
  x <- L$x
  y <- L$y
  by    <- L$by
  patch <- L$patch
  apply_conflict_fun <- L$apply_conflict_fun
  conflict_fun       <- L$conflict_fun
  conflicted_nms     <- L$conflicted_nms

  if (!is.null(.match_fun)) {
    ###############
    # FUZZY JOINS #
    ###############
    if (is.null(multi_by)) {
      # standard fuzzy join
      res <- fuzzy_join(
        x, y, by = `names<-`(by$y,by$x),
        match_fun = rlang::as_function(.match_fun),
        mode = .mode)
    } else {
      # using multi_by or safejoin formula notation
      res <- fuzzy_join(
        x, y,
        multi_by = by,
        multi_match_fun = rlang::as_function(.match_fun),
        mode = .mode)
      check_fuzzy_conflicts(res, .check, x, y)
    }
  } else {
    ##################
    # STANDARD JOINS #
    ##################
    join <- utils::getFromNamespace(paste0(.mode,"_join"), "dplyr")
    res <- join(
      x, y, by = `names<-`(by$y,by$x), na_matches = .na_matches)
  }
  #####################
  # RESOLVE CONFLICTS #
  #####################
  res <- resolve_conflicts(
    res, patch, apply_conflict_fun, conflict_fun, conflicted_nms)

  ########
  # FILL #
  ########
  if (!is.null(.fill)) {
    # relevant columns are those from y (already renamed) that are not conflicted
    # to these must be added cols ending with ".y" that are not part of
    # x, as it means they've been added by the joining fun
    suffixed <- setdiff(grep("\\.y$",names(res), value = TRUE),
                        paste0(names(x)))
    nms <- union(setdiff(names(y), names(x)), suffixed)
    res[nms][is.na(res[nms])] <- .fill
  }
  res
}
