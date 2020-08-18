
#' Title
#'
#' @param lhs expression giving the values to match on the lhs
#' @param rhs expression giving the values to match on the rhs
#' @param ignore_case boolean, if `TRUE` not only will character columns be set
#'   to lower case before matching, but `rapply` will be used on lists and data
#'   frame to make all characters lower case before comparison
#' @param ignore_element_order boolean, if `TRUE` elements of lists (including
#'   data frames) will be sorted before comparison
#' @param ignore_row_order boolean, if `TRUE` data frames will be sorted before
#'   comparison
#' @param ignore_class boolean, if `TRUE` unclass will be used on elements before
#'   comparison, so for instance tibbles and lists can be
#'
#' @return
#' @export
#'
#' @examples
match_equal <- function(lhs, rhs,
                        ignore_case = FALSE, ignore_element_order = FALSE, ignore_row_order = FALSE, ignore_class = FALSE){
  expl_args   <- as.list(match.call())[-1]
  res     <- formals()
  res[names(expl_args)] <- expl_args
  class(res) <- "safejoin_match"
  res
}

match_fuzzy <- function(lhs, rhs, rowwise = FALSE){
  expl_args <- as.list(match.call())[-1]
  res       <- formals()
  res[names(expl_args)] <- expl_args
  class(res) <- "safejoin_match"
  res
}

print.safejoin_match <- function(x,...){
  cat("\nsafejoin_match\n-----------------\n")
  print(unclass(x),...)
}
#
#
# test <- list("A","b",1,list("C","d"))
# rapply(test,tolower,"character",how = "replace")

match_equal(test(aa),zz+yy, TRUE,,T)
