context("prefix")
library(dplyr)
test_that("prefix works", {
  x <- band_instruments
  y <- band_members
  expect_equal(
    x %>% eat(y, .by = "name", .prefix = "NEW") %>% names() %>% last,
    "NEW_band")
  expect_equal(
    x %>% eat(y, .by = "name", .prefix = toupper) %>% names() %>% last,
    c("BAND"))
})


