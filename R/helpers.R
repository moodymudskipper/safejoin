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

  # check y unicity
  y_check <- check_for_letter(check,"y")
  if(x_check$lgl && anyDuplicated(y[by$y])){
    txt <- sprintf("y is not unique on %s", paste_enum(by$y))
    y_check$fun(txt)
  }

  # check column conflicts
  c_check <- check_for_letter(check,"c")
  if(c_check$lgl &&
     length(common_aux <-
            intersect(setdiff(names(x),by$x), setdiff(names(y),by$y)))){
    txt <- sprintf("Conflict of auxiliary columns: %s", paste_enum(common_aux))
    get(c_check$fun)(txt)
  }
  by
}
