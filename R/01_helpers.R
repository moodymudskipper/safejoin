check_for_letter <- function(str, letter){
  pattern <- sprintf("^.*?([~]*%s).*?$",letter)
  if (!grepl(pattern,str,ignore.case = TRUE))
    return(list(lgl = FALSE, fun = "ignore"))
  token   <- sub(pattern,"\\1",str,ignore.case = "TRUE")
  res <- list(lgl = TRUE)
  if (substr(token,1,1) == "~") res$fun <- "inform"
  else if (tolower(token) == token) res$fun <- "warn"
  else if (toupper(token) == token) res$fun <- "abort"
  res
}

inform <- rlang::inform
warn   <- rlang::warn
abort  <- rlang::abort
ignore <- function(...) NULL

# from list or dots, enumerate with a different last separator
paste_enum <- function(..., sep = ", ", sep2 = " and "){
  l <- unlist(list(...))
  if (length(l) <= 1) return(l)
  paste(paste(head(l,-1),collapse = sep), tail(l,1), sep = sep2)
}

safe_check <- function(x, y, by, check, skip_some = FALSE){
  #browser()
  # guess by and act depending on "b" check
  b_check <- check_for_letter(check,"b")
  if (is.null(by) && b_check$fun == "abort")
    abort("`by`is `NULL`, join columns should be explicit")
  by <- purrr::quietly(dplyr::common_by)(by, x, y)
  if (length(by$messages)) get(b_check$fun)(by$messages)
  by <- by$result

  # check if levels are the same on both sides
  l_check <- check_for_letter(check,"l")
  if (TRUE) { # l_check$lgl
    mismatch_lgl <- mapply(function(x,y) !identical(levels(x), levels(y)), x[by$x], y[by$y])
    if (any(mismatch_lgl)) {
      txt <- ""
      for (i in which(mismatch_lgl)) {
        lev_x <- levels(x[[by$x[i]]])
        lev_y <- levels(y[[by$y[i]]])
        txt <- paste0(txt, sprintf(
          "The pair %s/%s don't have the same levels:\nnot in x: %s\nnot in y: %s\n",
          by$x[i], by$y[i],
          paste( setdiff(lev_y, lev_x), collapse = " "),
          paste( setdiff(lev_x, lev_y), collapse = " ")))
      }

      x[[by$x[i]]] <- as.character(x[[by$x[i]]])
      y[[by$y[i]]] <- as.character(y[[by$y[i]]])
      get(l_check$fun)(txt)
    }
  }

  # check x unicity
  u_check <- check_for_letter(check,"u")
  if (u_check$lgl && anyDuplicated(x[by$x])) {
    txt <- sprintf("x is not unique on %s", paste_enum(by$x))
    get(u_check$fun)(txt)
  }

  if (!skip_some) {
    # check y unicity
    v_check <- check_for_letter(check,"v")
    if (v_check$lgl && anyDuplicated(y[by$y])) {
      txt <- sprintf("y is not unique on %s", paste_enum(by$y))
      get(v_check$fun)(txt)
    }
    # check column conflicts
    c_check <- check_for_letter(check,"c")
    if (c_check$lgl &&
       length(common_aux <-
              intersect(setdiff(names(x),by$x), setdiff(names(y),by$y)))) {
      txt <- sprintf("Conflict of auxiliary columns: %s", paste_enum(common_aux))
      get(c_check$fun)(txt)
    }
  }

  # check if all combinations match on x
  m_check <- check_for_letter(check,"m")
  if (m_check$lgl && nrow(
    unmatched <- dplyr::setdiff(dplyr::distinct(x[by$x]),
                               setNames(dplyr::distinct(y[by$y]),by$x)))) {
    txt <- paste("x has unmatched sets of joining values: \n%s",
                 paste(capture.output(unmatched),collapse = "\n"))
    get(m_check$fun)(txt)
  }

  # check if all combinations match on y
  n_check <- check_for_letter(check,"n")
  if (n_check$lgl && nrow(
    unmatched <- dplyr::setdiff(dplyr::distinct(y[by$y]),
                                setNames(dplyr::distinct(x[by$x]),by$y)))) {
    txt <- paste("y has unmatched sets of joining values: \n%s",
                 paste(capture.output(unmatched),collapse = "\n"))
    get(n_check$fun)(txt)
  }

  # check if all combinations are present on x
  e_check <- check_for_letter(check,"e")
  if (e_check$lgl && nrow(
    absent_comb <- dplyr::setdiff(
      purrr::cross_df(d <- dplyr::distinct(x[by$x])), d))) {
    txt <- paste("Some combinations of joining values are absent from x: \n%s",
                 paste(capture.output(absent_comb),collapse = "\n"))
    get(e_check$fun)(txt)
  }

  # check if all combinations are present on x
  f_check <- check_for_letter(check,"f")
  if (f_check$lgl &&
     nrow( absent_comb <- dplyr::setdiff(
       purrr::cross_df(d <- dplyr::distinct(y[by$y])), d))) {
    txt <- paste("Some combinations of joining values are absent from y: \n%s",
                 paste(capture.output(absent_comb),collapse = "\n"))
    get(f_check$fun)(txt)
  }


  dplyr::lst(x,y,by)
}
