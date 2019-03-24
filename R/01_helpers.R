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
  paste(paste(utils::head(l,-1),collapse = sep), utils::tail(l,1), sep = sep2)
}

resolve_conflicts <- function(
  res, patch, apply_conflict_fun, conflict_fun, common_aux){
  if (patch) {
    dummy_col <- "*temp_dummy_y*"
    rows_lgl <- !is.na(res[[dummy_col]])
    temp_cols <-  rename_to_conflicted(common_aux)
    res[rows_lgl, common_aux] <- res[rows_lgl,temp_cols]
    res <- dplyr::mutate_at(res,c(dummy_col,temp_cols), ~NULL)
  } else if (apply_conflict_fun) {
    for (aux in common_aux) {
      res[[aux]] <- conflict_fun(res[[aux]], res[[rename_to_conflicted(aux)]])
    }
    temp_cols <-  rename_to_conflicted(common_aux)
    res[temp_cols] <- NULL
  }
  res
}

rename_to_conflicted <- function(x) paste0("...", x, "_conflicted...")
rename_to_unconflicted <- function(x) gsub("^\\.\\.\\.(.*?)_conflicted\\.\\.\\.$","\\1",x)

# with_friendly_dot_error <- function(fun){
#   fiendly_fun <- fun
#   body(fiendly_fun) <- substitute({
#     MC <- match.call()
#     MC[[1]] <- quote(fun)
#     res <- try(eval.parent(MC),silent = TRUE)
#     if (inherits(res,"try-error")) {
#       frmls <- setdiff(names(formals()),"...")
#       dot_names <- names(eval(substitute(alist(...))))
#       candidates <- intersect(paste0(".",dot_names), frmls)
#       stop(attr(res,"condition")$message,
#            "\nDid you forget the dots in argument(s): ",
#            paste0(candidates, collapse = ", ")," ?")
#     }
#     res
#   })
#   fiendly_fun
# }
