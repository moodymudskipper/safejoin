#' Eat columns from another data frame
#'
#' Modified left_join where only a specified subset of \code{y} is kept, with
#'   optional checks and transformations.
#'
#' @inheritParams dplyr::left_join
#' @param ... One or more unquoted expressions, passed to \code{dplyr::select},
#'   defining the columns to keep from \code{y}.
#' @param fun function or formula or \code{NULL}, if not \code{NULL}, \code{y}
#'   will be grouped by its \code{by} columns and \code{fun} will be applied to
#'   all kept columns from {y}.
#' @param check string which determines what will trigger a message/warning or
#'   a failure, see details
#' @param replace in case of column conflict, should we keep both (default)
#'   or replace/ditch one of them ?
#' @param prefix prefix of new columns or function to apply on names of new
#'   columns, supports formula notation to reformat the new names further if
#'   needed
#' @return a data frame
#' @export
eat <- function (x, y, ..., by = NULL, fun = NULL,
                 # xyjc, we need new letters for perfect match (p), all x (?) and all y (?)
                 # maybe use metaphor of eating to ease the asymetry
                 check = "~j",
                 # to handle this, we can create dummy columns = 1 on each side,
                 # give a new name to
                 conflicts = c("keep_xy", "keep_x", "keep_y", "patch"),
                 prefix = NULL) {

  conflicts <- match.arg(conflicts)

  # guess by and act depending on "j" check
  j_check <- check_for_letter(check,"j")
  if(is.null(by) && j_check$fun == "abort")
    abort("`by`is `NULL`, joining columns should be explicit")
  by <- purrr::quietly(common_by)(by, x, y)
  if(length(by$messages)) get(j_check$fun)(by$messages)
  by <- by$result

  # check x unicity
  x_check <- check_for_letter(check,"x")
  if(x_check$lgl && anyDuplicated(x[by$x])){
    txt <- sprintf("x is not unique on %s", paste_enum(by$x))
    get(x_check)$fun(txt)
  }

  # check dots
  d_check <- check_for_letter(check,"d")
  if(d_check$check && length(substitute(list(...)))==1){
    if(d_check$fun == "abort") {
      abort("Eaten columns must be given explicitly")
    }
    txt <- sprintf(
      "Column names not provided, all columns from y will be eaten :",
      paste_enum(setdiff(names(y),by$y)))
    get(d_check$fun)(txt)
  }



  by_y_syms <- rlang::syms(by$y)

  # check unicity on left side
  if(x_check$check &
     nrow(x[by$x]) != n_distinct(x[by$x])){
    txt <- sprintf("x is not unique on %s",
                   paste(by$x, sep =", "))
    x_check$fun(txt)
  }

  # prefix y cols if relevant
  if(!is.null(prefix)){
    if (is.character(prefix)) prefix <- ~stringr::str_c(prefix, "_", .)
    y <- rename_at(y, -vars(!!!by_y_syms), prefix)
  }

  # transform y with fun if relevant
  if(!is.null(fun)){
    fun <- rlang::as_function(fun)
    . <- dplyr::group_by(y, !!!by_y_syms)
    y <- dplyr::summarize_all(., fun)
  }

  # check column conflicts
  c_check <- check_for_letter(check,"c")
  if(c_check$lgl &&
     length(common_aux <-
            intersect(setdiff(names(x),by$x), setdiff(names(y),by$y)))){
    txt <- sprintf("Conflict of auxiliary columns: %s", paste_enum(common_aux))
    get(c_check$fun)(txt)
    if(conflicts == "keep_x") y[common_aux] <- NULL else
      if(conflicts == "keep_y") x[common_aux] <- NULL else
        if(conflicts == "patch") {
          x <- mutate(x, ..x.dummy.. = 1)
          y <- mutate(y, ..y.dummy.. = 1)
          # in progress
        }

  # check y unicity
  y_check <- check_for_letter(check,"y")
  if(x_check$lgl && anyDuplicated(y[by$y])){
    txt <- sprintf("y is not unique on %s", paste_enum(by$y))
    y_check$fun(txt)
  }

  left_join(x,y, by = setNames(by$y,by$x))
  }
}
