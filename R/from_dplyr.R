# * `"x"` to check unicity on `x` (lhs)
# * `"y"` to check unicity on `y` (lhs)
# * `"c"` to check Conflicts of Columns
# * `"j"` like _"join"_ checks if joining columns were given explicitly
# * `"d"` like _"dots"_ checks that dots were used to give explicitely eaten columns

library(rlang)
# temp <- dplyr::common_by
# common_by  <-  as.function(c(formals(temp),alist(check = ),body(temp)))
# temp <- getFromNamespace("common_by.character","dplyr")
# common_by.character  <-  as.function(c(formals(temp),alist(check = ),body(temp)))
# temp <- getFromNamespace("common_by.default","dplyr")
# common_by.default  <-  as.function(c(formals(temp),alist(check = ),body(temp)))
# temp <- getFromNamespace("common_by.list","dplyr")
# common_by.list  <-  as.function(c(formals(temp),alist(check = ),body(temp)))
# rm(temp)
#
# check_valid_names <- getFromNamespace("check_valid_names","dplyr")
# auto_by_msg       <- getFromNamespace("auto_by_msg","dplyr")
# check_suffix      <- getFromNamespace("check_suffix","dplyr")
# check_na_matches  <- getFromNamespace("check_na_matches","dplyr")
# join_vars         <- getFromNamespace("join_vars","dplyr")
# left_join_impl    <- getFromNamespace("left_join_impl","dplyr")
# common_by_from_vector    <- getFromNamespace("common_by_from_vector","dplyr")
#
# common_by.NULL <- function(by, x, y, check)
# {
#   if (check$fun == "stop")
#     stop("`by`is `NULL`, joining columns should be explicit")
#   by <- intersect(tbl_vars(x), tbl_vars(y))
#   if (length(by) == 0) {
#     bad_args("by", "required, because the data sources have no common variables")
#   }
#   get(check$fun)(auto_by_msg(by))
#   list(x = by, y = by)
# }
