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



safe_check <- function(x, y, by, check, conflict, in_eat = FALSE, agg, prefix, ...){
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

  # check x unicity
  u_check <- check_for_letter(check,"u")
  if (u_check$lgl && anyDuplicated(x[by$x])) {
    txt <- sprintf("x is not unique on %s", paste_enum(by$x))
    get(u_check$fun)(txt)
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

  if (in_eat) {
    # check dots
    d_check <- check_for_letter(check,"d")
    if (d_check$lgl && length(substitute(list(...))) == 1) {
      if (d_check$fun == "abort") {
        rlang::abort("Eaten columns must be given explicitly")
      }
      txt <- sprintf(
        "Column names not provided, all columns from y will be eaten :\n%s",
        paste_enum(setdiff(names(y),by$y)))
      get(d_check$fun)(txt)
    }

    # store y's by columns as symbols
    by_y_syms <- rlang::syms(by$y)

    # subset y by column
    if (rlang::dots_n(...)) y <- dplyr::select(y, !!!by_y_syms, ...)

    # prefix y cols if relevant
    if (!is.null(prefix)) {
      if (is.character(prefix)) prefix <- eval(rlang::expr( ~stringr::str_c(!!prefix, "_", .)))
      y <- dplyr::rename_at(y, setdiff(names(y),by$y), prefix)
    }

    # transform y with .agg if relevant
    if (!is.null(agg)) {
      agg <- rlang::as_function(agg)
      . <- dplyr::group_by(y, !!!by_y_syms)
      y <- dplyr::summarize_all(., agg)
    }
  }

  # check column conflict
  c_check <- check_for_letter(check,"c")
  patch <- FALSE
  apply_conflict_fun <- FALSE
  conflict_fun <- NULL
  common_aux <- NULL
  if ((c_check$lgl || !is.null(conflict)) &&
      length(common_aux <-
             intersect(setdiff(names(x),by$x), setdiff(names(y),by$y)))) {
    if (is.null(conflict)){
      # if conflict is null trigger appropriate output
      txt <- sprintf("Conflict of auxiliary columns: %s", paste_enum(common_aux))
      get(c_check$fun)(txt)
    } else if (is.function(conflict) || inherits(conflict,"formula")) {
      # if it's a function, rename y conflicted columns to temp names,
      # define function and switch flag
      conflict_fun <- rlang::as_function(conflict)
      y <- dplyr::rename_at(y, common_aux,~paste0("*", .x, "_conflicted*"))
      apply_conflict_fun <- TRUE
    } else if (conflict == "patch") {
      # if it's "patch", rename y conflicted columns to temp names,
      # creates column to know where matches occured and switch flag
      y <- dplyr::rename_at(y, common_aux,~paste0("*", .x, "_conflicted*"))
      #x <- mutate(x, `*temp_dummy_x*` = 1)
      y <- dplyr::mutate(y, `*temp_dummy_y*` = 1)
      patch <- TRUE
    } else abort("Unsupported conflict argument")
  }


  # check y unicity
  v_check <- check_for_letter(check,"v")
  if (v_check$lgl && anyDuplicated(y[by$y])) {
    txt <- sprintf("y is not unique on %s", paste_enum(by$y))
    get(v_check$fun)(txt)
  }


  # # check column conflicts
  # c_check <- check_for_letter(check,"c")
  # if (c_check$lgl &&
  #     length(common_aux <-
  #            intersect(setdiff(names(x),by$x), setdiff(names(y),by$y)))) {
  #   txt <- sprintf("Conflict of auxiliary columns: %s", paste_enum(common_aux))
  #   get(c_check$fun)(txt)
  # }
  dplyr::lst(x, y, by, patch, apply_conflict_fun, conflict_fun, common_aux)
}


resolve_conflicts <- function(
  res, patch, apply_conflict_fun, conflict_fun, common_aux){
  if (patch) {
    dummy_col <- "*temp_dummy_y*"
    rows_lgl <- !is.na(res[[dummy_col]])
    temp_cols <-  paste0("*",common_aux,"_conflicted*")
    res[rows_lgl, common_aux] <- res[rows_lgl,temp_cols]
    res <- dplyr::mutate_at(res,c(dummy_col,temp_cols), ~NULL)
  } else if (apply_conflict_fun) {
    for (aux in common_aux) {
      res[[aux]] <- conflict_fun(res[[aux]], res[[paste0("*",aux,"_conflicted*")]])
      temp_cols <-  paste0("*",common_aux,"_conflicted*")
    }
    res <- dplyr::mutate_at(res,temp_cols, ~NULL)
  }
  res
}
