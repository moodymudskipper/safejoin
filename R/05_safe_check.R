
check_d <- function(x, y, by, d_check, dots){
  if (d_check$lgl && length(dots) == 0) {
    if (d_check$fun == "abort") {
      rlang::abort("Eaten columns must be given explicitly")
    }
    txt <- sprintf(
      "Column names not provided, all columns from y will be eaten :\n%s",
      paste_enum(setdiff(names(y),by$y)))
    get(d_check$fun)(txt)
  }
}

safe_check <- function(x, y, by, check, conflict, suffix, match_fun, in_eat = FALSE, agg, prefix, dots){

  # all checks (all with elt lgl to test and elt fun containing function to apply)
  b_check <- check_for_letter(check,"b")
  t_check <- check_for_letter(check,"t")
  l_check <- check_for_letter(check,"l")
  u_check <- check_for_letter(check,"u")
  m_check <- check_for_letter(check,"m")
  n_check <- check_for_letter(check,"n")
  e_check <- check_for_letter(check,"e")
  f_check <- check_for_letter(check,"f")
  d_check <- check_for_letter(check,"d")

  # guessif by was explicit
  by <- check_b(x, y, by, b_check)

  # check if types and classes are the same on both sides
  check_t(x, y, by, t_check)

  # check if levels are the same on both sides, convert if relevant
  # irrelevant for fuzzy matches (in the general case)
  if (is.null(match_fun)) {
  L <- check_l(x, y, by, l_check)
  x <- L$x
  y <- L$y
  }

  # check x unicity
  check_u(x, y, by, u_check)

  # check if all combinations match on x
  check_m(x, y, by, m_check)

  # check if all combinations match on y
  check_n(x, y, by, n_check)

  # check if all combinations are present on x
  check_e(x, y, by, e_check)

  # check if all combinations are present on y
  check_f(x, y, by, f_check)

  if (in_eat) {
    # check dots
    check_d(x, y, by, d_check, dots)

    # store y's by columns as symbols
    by_y_syms <- rlang::syms(by$y)

    # subset y by column
    if (length(dots)) y <- dplyr::select(y, !!!dots, !!!by_y_syms)

    # prefix y cols if relevant
    if (!is.null(prefix)) {
      if (is.character(prefix))
        prefix <- rlang::as_function(eval(rlang::expr( ~paste0(!!prefix, "_", .))))
      if (is.null(match_fun)) {
        y <- dplyr::rename_at(y, setdiff(names(y),by$y), prefix)
      } else {
        y <- dplyr::rename_all(y, prefix)
        by$y <- prefix(by$y)
      }
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
  conflicted_nms <- NULL
  if (c_check$lgl || !is.null(conflict)) {
    if (!is.null(match_fun)) {
      conflicted_nms <- intersect(names(x), names(y))
    } else {
      conflicted_nms <- union(
        # colname in x AND in y's aux columns
        intersect(names(x), setdiff(names(y),by$y)),
        # colname in y AND in x's aux columns
        intersect(setdiff(names(x), by$x), names(y)))
    }
    if (length(conflicted_nms)) {
      if (is.null(conflict)) {
        # if conflict is null trigger appropriate output
        txt <- sprintf("Conflict of columns: %s", paste_enum(conflicted_nms))
        get(c_check$fun)(txt)
      } else if (is.function(conflict) || inherits(conflict,"formula")) {
        # if it's a function, rename y conflicted columns to temp names,
        # define function and switch flag
        conflict_fun <- rlang::as_function(conflict)
        if (any(conflicted_nms %in% by$y)) # in case of a fuzzy join
          by$y[by$y %in% conflicted_nms] <-
            safejoin:::rename_to_conflicted(by$y[by$y %in% conflicted_nms])
        y <- dplyr::rename_at(y, conflicted_nms, safejoin:::rename_to_conflicted)
        apply_conflict_fun <- TRUE
      } else if (conflict == "patch") {
        # if it's "patch", rename y conflicted columns to temp names,
        # creates column to know where matches occured and switch flag
        if (any(conflicted_nms %in% by$y)) # in case of a fuzzy join
          by$y[by$y %in% conflicted_nms] <-
            safejoin:::rename_to_conflicted(by$y[by$y %in% conflicted_nms])
        y <- dplyr::rename_at(y, conflicted_nms, safejoin:::rename_to_conflicted)
        #x <- mutate(x, `*temp_dummy_x*` = 1)
        y <- dplyr::mutate(y, `*temp_dummy_y*` = 1)
        patch <- TRUE
      } else abort("Unsupported conflict argument")
    }
  }

  # check y unicity
  v_check <- check_for_letter(check,"v")
  if (v_check$lgl && anyDuplicated(y[by$y])) {
    txt <- sprintf("y is not unique on %s", paste_enum(by$y))
    get(v_check$fun)(txt)
  }
  dplyr::lst(x, y, by, patch, apply_conflict_fun, conflict_fun, conflicted_nms)
}
