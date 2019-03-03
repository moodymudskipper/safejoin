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

# extracts relevant subset of matrix or vector
X <- function(var){
  eval.parent(substitute({
    if (is.matrix(.x)) .x[,var] else .x
  }))
}

Y <- function(var){
  eval.parent(substitute({
    if (is.matrix(.y)) {
      if(is.numeric(var) || var %in% colnames(.y)) .y[,var] else .y[,rename_to_conflicted(var)]
        } else .y
  }))
}


check_fuzzy_conflicts <- function(res, check, x, y){
  c_check <- check_for_letter(check, "c")
  new_cols <- setdiff(names(res),c(names(x), names(y)))
  conflicted_nms <- intersect(sub("1$","",new_cols), c(names(x), names(y)))
  if (c_check$lgl && length(conflicted_nms)) {
    # we might have created a new col here by creating a distance column
    # it would have been renamed with a "1" suffix automatically if it was
    # the case, one might have created
    # it on purpose with this name, so let's fail if needed but be explicit
    txt <- paste0("Fuzzy join created: ", paste_enum(new_cols),
                  "\nSource table contained: ", paste_enum(conflicted_nms),
                  "\nIt's probable, but not certain, that there was a ",
                  "column name conflict.",
                  "\nConsider renaming the columns of the data frame ",
                  "returned by the `by` argument or use a less strict check")
    get(c_check$fun)(txt)
  }}
