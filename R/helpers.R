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

ignore <- function(...) NULL

# from list or dots, enumerate with a different last separator
paste_enum <- function(..., sep = ", ", sep2 = " and "){
  l <- unlist(list(...))
  if(length(l) <= 1) return(l)
  paste(paste(head(l,-1),collapse = sep), tail(l,1), sep=sep2)
}

# paste_enum("a","b","c")
# # [1] "a, b and c"
# paste_enum("a")
# # [1] "a, b and c"


safe_check <- function(x, y, by, check){
  # guess by and act depending on "j" check
  b_check <- check_for_letter(check,"b")
  if(is.null(by) && b_check$fun == "abort")
    abort("`by`is `NULL`, joining columns should be explicit")
  by <- purrr::quietly(common_by)(by, x, y)
  if(length(by$messages)) get(b_check$fun)(by$messages)
  by <- by$result

  # check x unicity
  u_check <- check_for_letter(check,"u")
  if(u_check$lgl && anyDuplicated(x[by$x])){
    txt <- sprintf("x is not unique on %s", paste_enum(by$x))
    get(u_check$fun)(txt)
  }

  # check y unicity
  v_check <- check_for_letter(check,"v")
  if(u_check$lgl && anyDuplicated(y[by$y])){v
    txt <- sprintf("y is not unique on %s", paste_enum(by$y))
    get(v_check$fun)(txt)
  }

  # check column conflicts
  c_check <- check_for_letter(check,"c")
  if(c_check$lgl &&
     length(common_aux <-
            intersect(setdiff(names(x),by$x), setdiff(names(y),by$y)))){
    txt <- sprintf("Conflict of auxiliary columns: %s", paste_enum(common_aux))
    get(c_check$fun)(txt)
  }

  # check if all combinations match on x
  m_check <- check_for_letter(check,"m")
  if(m_check$lgl &&
     nrow( unmatched <- dplyr::setdiff(distinct(x[by$x]), setNames(distinct(y[by$y]),by$x)))){
    txt <- paste("x has unmatched sets of joining values: \n%s",
                 paste(capture.output(unmatched),collapse="\n"))
    get(m_check$fun)(txt)
  }

  # check if all combinations match on y
  n_check <- check_for_letter(check,"n")
  if(n_check$lgl &&
     nrow( unmatched <- dplyr::setdiff(distinct(y[by$y]), setNames(distinct(x[by$x]),by$y)))){
    txt <- paste("y has unmatched sets of joining values: \n%s",
                 paste(capture.output(unmatched),collapse="\n"))
    get(n_check$fun)(txt)
  }

  # check if all combinations are present on x
  e_check <- check_for_letter(check,"e")
  if(e_check$lgl &&
     nrow( absent_comb <- dplyr::setdiff(cross_df(d <- distinct(x[by$x])), d))){
    txt <- paste("Some combinations of joining values are absent from x: \n%s",
                 paste(capture.output(absent_comb),collapse="\n"))
    get(e_check$fun)(txt)
  }

  # check if all combinations are present on x
  f_check <- check_for_letter(check,"f")
  if(f_check$lgl &&
     nrow( absent_comb <- dplyr::setdiff(cross_df(d <- distinct(y[by$y])), d))){
    txt <- paste("Some combinations of joining values are absent from y: \n%s",
                 paste(capture.output(absent_comb),collapse="\n"))
    get(f_check$fun)(txt)
  }
  by
}
