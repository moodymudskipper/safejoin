context("renaming")
library(dplyr)
test_that("renaming on the fly works", {
  x <- band_instruments
  y <- band_members
  expect_equal(
    x %>% eat(y, BAND = band, .by = "name", .check = "C") %>% names() %>% last,
    "BAND")
  # we create a conflicting column but we rename it so it shouldn't conflict
  x <- band_instruments
  y <- band_members %>% mutate(plays = "yes")
  expect_equal(
    x %>% eat(y, PLAYS = plays, .by = "name", .check = "C") %>% names() %>% last,
    c("PLAYS"))
})
