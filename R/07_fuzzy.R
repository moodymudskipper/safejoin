# makes lists of strings held in X(str) and Y(str)
extract_xy_content <- function(e, xy = list(x = list(),y = list())) {
  for (call in as.list(e)) {
    if (is.call(call)) {
      if (call[[1L]] == quote(X)) xy$x <- append(xy$x, eval(call[[2L]])) else
        if (call[[1L]] == quote(Y)) xy$y <- append(xy$y, eval(call[[2L]]))
        else xy <- extract_xy_content(call, xy)
    }
  }
  xy
}

#' Select a variable from to opperate a fuzzy join
#'
#' #' Should be called only inside a formula fed to the match_fun argument of a
#' safejoin function.
#'
#' Use `X` to select a variable from the lhs, and `Y` to select a variable from
#' the rhs, calls to `X` and `Y` will be detected to register the variables used
#' so that the proper cartesian product can be operated.
#'
#' @param var a string containing the name of the variable to select
#' @export
#' @name XY
X <- function(var){
  if (identical(parent.frame(), .GlobalEnv))
    stop("X shouldn't be called from the global environment")
  eval.parent(substitute({
    if (is.matrix(.x)) .x[,var] else .x
  }))
}

#' @export
#' @rdname XY
Y <- function(var){
  if (identical(parent.frame(), .GlobalEnv))
    stop("Y shouldn't be called from the global environment")
  # because it's run outside of the namespace we don't use rename_to_conflicted
  # but copy it's definition
  eval.parent(substitute({
    if (is.matrix(.y)) {
      if (is.numeric(var) || var %in% colnames(.y)) .y[,var] else .y[, paste0("...", var, "_conflicted...")]
        } else .y
  }))
}


check_fuzzy_conflicts <- function(res, check, x, y){
  c_check <- check_for_letter(check, "c")
  new_cols <- setdiff(names(res),c(names(x), names(y)))
  conflicted_nms <- intersect(sub("\\.{3}\\d$","",new_cols), c(names(x), names(y)))
  if (c_check$lgl && length(conflicted_nms)) {
    # we might have created a new col here by creating a distance column
    # it would have been renamed with a "1" suffix automatically if it was
    # the case, one might have created
    # it on purpose with this name, so let's fail if needed but be explicit
    txt <- paste0(
      "Some columns created in the `by` argument resulted in conflict: ",
      toString(conflicted_nms)
    )
    get(c_check$fun)(txt)
  }}
