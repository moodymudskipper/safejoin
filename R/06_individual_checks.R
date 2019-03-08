check_b <- function(x, y, by, b_check){
  if (is.null(by) && b_check$fun == "abort")
    abort("`by`is `NULL`, join columns should be explicit")
  by <- purrr::quietly(dplyr::common_by)(by, x, y)
  if (length(by$messages)) get(b_check$fun)(by$messages)
  by <- by$result
}



check_t <- function(x, y, by, t_check){
  if (t_check$lgl) {
    mismatch_lgl <- mapply(function(x,y) !identical(
      list(typeof(x), class(x)), list(typeof(y), class(y))), x[by$x], y[by$y])
    if (any(mismatch_lgl)) {
      txt <- ""
      for (i in which(mismatch_lgl)) {
        txt <- paste0(txt, sprintf(
          "The pair %s/%s don't have the same type/class:\nx: %s / %s\ny: %s / %s",
          by$x[i], by$y[i],
          typeof(x[[by$x[i]]]),
          paste(class(x[[by$x[i]]]), collapse = ", "),
          typeof(y[[by$y[i]]]),
          paste(class(y[[by$y[i]]]), collapse = ", ")))
      }
      get(t_check$fun)(txt)
    }
  }
}


check_l <- function(x, y, by, l_check){
  mismatch_lgl <- mapply(function(x,y) !identical(levels(x), levels(y)), x[by$x], y[by$y])
  if (any(mismatch_lgl)) {
    txt <- ""
    for (i in which(mismatch_lgl)) {
      txt <- paste0(txt, sprintf(
        "The pair %s/%s don't have the same levels:\nx: %s\ny: %s\n",
        by$x[i], by$y[i],
        paste(levels(x[[by$x[i]]]), collapse = ", "),
        paste(levels(y[[by$y[i]]]), collapse = ", ")))
      x[[by$x[i]]] <- as.character(x[[by$x[i]]])
      y[[by$y[i]]] <- as.character(y[[by$y[i]]])
    }
    if (l_check$fun != "abort")
      txt <- paste0(txt, "They'll be coerced to character\n")
    get(l_check$fun)(txt)
  }
  dplyr::lst(x,y)
}


check_u <- function(x, y, by, u_check){
  if (u_check$lgl && anyDuplicated(x[by$x])) {
    txt <- sprintf("x is not unique on %s", paste_enum(by$x))
    get(u_check$fun)(txt)
  }
}

check_m <- function(x, y, by, m_check){
  if (m_check$lgl && nrow(
    unmatched <- dplyr::setdiff(dplyr::distinct(x[by$x]),
                                `names<-`(dplyr::distinct(y[by$y]),by$x)))) {
    txt <- paste("x has unmatched sets of joining values: \n",
                 paste(utils::capture.output(unmatched),collapse = "\n"))
    get(m_check$fun)(txt)
  }
}

check_n <- function(x, y, by, n_check){
  if (n_check$lgl && nrow(
    unmatched <- dplyr::setdiff(dplyr::distinct(y[by$y]),
                                `names<-`(dplyr::distinct(x[by$x]),by$y)))) {
    txt <- paste("y has unmatched sets of joining values: \n",
                 paste(utils::capture.output(unmatched),collapse = "\n"))
    get(n_check$fun)(txt)
  }
}

check_e <- function(x, y, by, e_check){
  if (e_check$lgl && nrow(
    absent_comb <- dplyr::setdiff(
      purrr::cross_df(d <- dplyr::distinct(x[by$x])), d))) {
    txt <- paste("Some combinations of joining values are absent from x: \n%s",
                 paste(utils::capture.output(absent_comb),collapse = "\n"))
    get(e_check$fun)(txt)
  }
}

check_f <- function(x, y, by, f_check){
  if (f_check$lgl &&
      nrow( absent_comb <- dplyr::setdiff(
        purrr::cross_df(d <- dplyr::distinct(y[by$y])), d))) {
    txt <- paste("Some combinations of joining values are absent from y: \n%s",
                 paste(utils::capture.output(absent_comb),collapse = "\n"))
    get(f_check$fun)(txt)
  }
}
