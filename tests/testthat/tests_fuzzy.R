context("fuzzy")
library(dplyr)


test_that("fuzzy joins work using safe_left_join", {
  # standard join recognize conflict
  x <- data.frame(a = 1:2,b = 3:4)
  y <- data.frame(a = 2:3,c = 5:6)
  expect_error(safe_left_join(x,y,match_fun = `<`, check = "C"),
               "Conflict")

  # simple join from
  # https://stackoverflow.com/questions/50583341/
  a <- data.frame(x = c(1,3,5))
  b <- data.frame(start = c(0,4),end = c(2,6),y = c("a","b"))
  testthat::expect_equal(
    dim(safe_left_join(a, b, ~ X("x") > Y("start") & X("x") <  Y("end"))),
    c(3,4))
  # numeric indices
  testthat::expect_equal(
    safe_left_join(a, b, ~ X("x") > Y("start") & X("x") <  Y("end")),
    safe_left_join(a, b, ~ X(1) > Y(1) & X(1) <  Y(2)))

  # fill works
  # with suffix .y
  testthat::expect_equal(
    sum(is.na(eat(x,y,.match_fun = `<`, .check = "", .fill = 0))),
    0)
  # with renamed cols
  testthat::expect_equal(
    sum(is.na(eat(x,y,.match_fun = `<`, .check = "c", .fill = 0, .prefix = "NEW"))),
    0)

})

test_that("fuzzy joins work using eat", {
  # standard join recognize conflict
  x <- data.frame(a = 1:2,b = 3:4)
  y <- data.frame(a = 2:3,c = 5:6)
  expect_error(eat(x,y, c, .match_fun = `<`, .check = "C"),
               "Conflict")
  # prefix avoid the conflict
  expect_silent(eat(x,y,c, .match_fun = `<`, .prefix = "NEW", .check = "C"))

  # simple join from
  # https://stackoverflow.com/questions/50583341/
  a <- data.frame(x = c(1,3,5))
  b <- data.frame(start = c(0,4),end = c(2,6),y = c("a","b"))
  testthat::expect_equal(
    dim(eat(a, b, .by = ~ X("x") > Y("start") & X("x") <  Y("end"))),
    c(3,4))
  # numeric indices
  testthat::expect_equal(
    eat(a, b, .by = ~ X("x") > Y("start") & X("x") <  Y("end")),
    eat(a, b, .by = ~ X(1) > Y(1) & X(1) <  Y(2)))
})

test_that("fuzzy joins, adding a column, work with eat", {
  # a case with a distance column
  # https://stackoverflow.com/questions/41472722
  set.seed(123)
  mz1    <- c(seq(100, 190, by = 10))
  rt1    <- c(seq(1, 10, by = 1))
  value1 <- runif(10, min = 100, max = 100000)
  mz2    <- mz1 + runif(10, -0.1, 0.1)
  rt2    <- rt1 + runif(10, -0.2, 0.2)
  value2 <- runif(10, min = 100, max = 100000)
  df1 <- as.data.frame(cbind(mz1, rt1, value1))
  df2 <- as.data.frame(cbind(mz2, rt2, value2))
  df2 <- rbind(df2, c(180.001, 9.09, 0))

  # fuzzy join adding a column
  mmf1 <- function(x, y) {
    mz_dist <- abs(x[, 1] - y[, 1])
    rt_dist <- abs(x[, 2] - y[, 2])
    rt_dist <= 0.1 & mz_dist < 0.05
  }

  testthat::expect_equal(
    fuzzyjoin::fuzzy_join(df1, df2, multi_by = c("mz1" = "mz2", "rt1" = "rt2"),
               multi_match_fun = mmf1, mode = "full"),
    eat(df1,df2, .by = ~abs(X(1) - Y(1)) < .05 & abs(X(2) - Y(2)) <= .1,
    .mode = "full"))

  mmf2 <- function(x, y) {
    mz_dist <- abs(x[, 1] - y[, 1])
    rt_dist <- abs(x[, 2] - y[, 2])
    data.frame(merge = rt_dist <= 0.1 & mz_dist < 0.05,
               dist = sqrt(mz_dist^2 + rt_dist^2))
  }
  testthat::expect_equal(
    fuzzyjoin::fuzzy_join(df1, df2, multi_by = c("mz1" = "mz2", "rt1" = "rt2"),
               multi_match_fun = mmf2, mode = "full"),
    eat(df1,df2, .mode = "full", .by = ~ data.frame(
      abs(X(1) - Y(1)) < .05 & abs(X(2) - Y(2)) <= .1,
      dist = sqrt(abs(X(1) - Y(1))^2 +  abs(X(2) - Y(2))^2))))

  # column conflict
  mmf3 <- function(x, y) {
    mz_dist <- abs(x[, 1] - y[, 1])
    rt_dist <- abs(x[, 2] - y[, 2])
    data.frame(merge = rt_dist <= 0.1 & mz_dist < 0.05,
               rt2 = sqrt(mz_dist^2 + rt_dist^2))
  }

  testthat::expect_error(
    eat(df1,df2, .mode = "full", .by =~ data.frame(
      abs(X(1) - Y(1)) < .05 & abs(X(2) - Y(2)) <= .1,
      rt2 = sqrt(abs(X(1) - Y(1))^2 +  abs(X(2) - Y(2))^2))),
    "column name conflict.")

})



test_that("fuzzy joins, work with conflict argument", {
  # this used to bug because we used multi_by instead of by in one spot
  # and because our temp names were not syntactic and fuzzyjoin doesn't like
  # that because `select_` doesn't like that
  DF1 <- data.frame("col1" = rep(c("A","B"), 18),
                    "col2" = rep(c("C","D","E"), 12),
                    "value"= (sample(1:100,36)),
                    "col4" = rep(NA,36))

  DF2 <- data.frame("col1" = rep("A",6),
                    "col2" = rep(c("C","D"),3),
                    "data" = rep(c(1,3),3),
                    "min" = seq(0,59,by=10),
                    "max" = seq(10,69,by=10))
  expect_silent(safe_left_join(DF1, DF2,  ~
                 X("col1") == Y("col1") &
                 X("col2") == Y("col2") &
                 X("value") >= Y("min") &
                 X("value") <= Y("max"),conflict = ~.x))
})

