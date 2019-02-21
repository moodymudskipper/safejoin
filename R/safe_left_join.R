#' safe joins!
#'
#' @inheritParams dplyr::join
#' @param check a string
#'
#'
#' @name safe_joins
NULL

#' @export
#' @rdname safe_joins
safe_left_join <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ...,
                                  check = "~jC") {

  by <- jxyc_checks(x, y, by, check)
  dplyr::left_join(x, y, by = setNames(by$y,by$x), copy,
             suffix = c(".x", ".y"), ...)
}

#' @export
#' @rdname safe_joins
safe_right_join <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...,
                           check = "~jC") {

  by <- jxyc_checks(x, y, by, check)
  dplyr::right_join(x, y, by = setNames(by$y,by$x), copy,
                   suffix = c(".x", ".y"), ...)
}

#' @export
#' @rdname safe_joins
safe_inner_join <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...,
                           check = "~jC"
) {

  by <- jxyc_checks(x, y, by, check)
  dplyr::inner_join(x, y, by = setNames(by$y,by$x), copy,
                   suffix = c(".x", ".y"), ...)
}

#' @export
#' @rdname safe_joins
safe_full_join <- function(x, y, by = NULL, copy = FALSE,
                           suffix = c(".x", ".y"), ...,
                           check = "~jC"
) {

  by <- jxyc_checks(x, y, by, check)
  dplyr::full_join(x, y, by = setNames(by$y,by$x), copy,
                   suffix = c(".x", ".y"), ...)
}

#' @export
#' @rdname safe_joins
safe_semi_join <- function(x, y, by = NULL, copy = FALSE, ...,
                           check = "~jC"
) {

  by <- jxyc_checks(x, y, by, check)
  dplyr::semi_join(x, y, by = setNames(by$y,by$x), copy, ...)
}

#' @export
#' @rdname safe_joins
safe_anti_join <- function(x, y, by = NULL, copy = FALSE, ...,
                           check = "~jC") {
  by <- jxyc_checks(x, y, by, check)
  dplyr::anti_join(x, y, by = setNames(by$y,by$x), copy, ...)
}

#' @export
#' @rdname safe_joins
safe_nest_join <- function(x, y, by = NULL, copy = FALSE, keep = FALSE,
                           name = NULL, ..., check = "~jC"){
  by <- jxyc_checks(x, y, by, check)
  dplyr::nest_join(x, y, by = setNames(by$y,by$x), copy, keep, name, ...)
}

# safe_left_join(band_members,
#                band_instruments %>% rename_at(2,~"band"),
#                by = "name",
#                check ="cj")
#
# safe_left_join(band_members,
#                band_instruments,
#                check ="cj")
