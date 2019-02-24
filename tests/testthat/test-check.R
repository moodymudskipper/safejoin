context("check")
library(dplyr)
test_that("character 'b' works", {
  x <- band_instruments
  y <- band_members
  expect_silent(safe_left_join(x, y, check = ""))
  txt_error <- "join columns should be explicit"
  expect_error(  safe_left_join(x, y,  check = "B"), txt_error)
  txt <- "Joining, by"
  expect_warning(safe_left_join(x, y, check =  "b"), txt)
  expect_message(safe_left_join(x, y, check = "~b"), txt)
  expect_message(eat(x, y, .check = "~b"), txt)
})

test_that("character 'l' works", {
  # same factors
  x <- band_instruments %>%
    mutate(name = factor(name,c("John","Paul","Keith","Mick")))
  y <- band_members %>%
    mutate(name = factor(name,c("John","Paul","Keith","Mick")))
  expect_silent(safe_left_join(x, y, check =  "L"))
  expect_silent(safe_left_join(x, y, check =  "l"))
  expect_silent(safe_left_join(x, y, check = "~l"))
  # different factors
  x <- band_instruments %>%
    mutate(name = factor(name))
  y <- band_members %>%
    mutate(name = factor(name))
  txt <- "don't have the same levels"
  expect_error(  safe_left_join(x, y, check =  "L"), txt)
  expect_warning(safe_left_join(x, y, check =  "l"), txt)
  expect_message(safe_left_join(x, y, check = "~l"), txt)
  expect_message(eat(x, y, .check = "~l"), txt)
})

test_that("character 'u' works", {
  # add join col to have 2, but no dupes
  x <- band_instruments %>% mutate(j = "cst")
  y <- band_members %>% mutate(j = "cst")
  expect_silent(safe_left_join(x, y, check =  "U"))
  expect_silent(safe_left_join(x, y, check =  "u"))
  expect_silent(safe_left_join(x, y, check = "~u"))
  # duplicate x keys
  x <- band_instruments %>%
    add_row(name = "John", plays = "bass") %>%
    mutate(j = "cst")
  y <- band_members %>% mutate(j = "cst")
  txt <- "x is not unique on"
  expect_error(  safe_left_join(x, y, check =  "U"), txt)
  expect_warning(safe_left_join(x, y, check =  "u"), txt)
  expect_message(safe_left_join(x, y, check = "~u"), txt)
  expect_message(eat(x, y, .check = "~u"), txt)
})

test_that("character 'v' works", {
  # add join col to have 2, but no dupes
  x <- band_instruments %>% mutate(j = "cst")
  y <- band_members %>% mutate(j = "cst")
  expect_silent(safe_left_join(x, y, check =  "V"))
  expect_silent(safe_left_join(x, y, check =  "v"))
  expect_silent(safe_left_join(x, y, check = "~v"))
  # duplicate y keys
  x <- band_instruments %>% mutate(j = "cst")
  y <- band_members %>%
    add_row(name = "John", band = "The Who") %>%
    mutate(j = "cst")
  txt <- "y is not unique on"
  expect_error(  safe_left_join(x, y, check =  "V"), txt)
  expect_warning(safe_left_join(x, y, check =  "v"), txt)
  expect_message(safe_left_join(x, y, check = "~v"), txt)
  expect_message(eat(x, y, .check = "~v"), txt)
})

test_that("character 'c' works", {
  # no conflict
  x <- band_instruments
  y <- band_members
  expect_silent(safe_left_join(x, y, check =  "C"))
  expect_silent(safe_left_join(x, y, check =  "c"))
  expect_silent(safe_left_join(x, y, check = "~c"))
  # conflict on j column
  x <- band_instruments %>% mutate(j = "cst")
  y <- band_members %>% mutate(j = "cst")
  txt <- "Conflict of auxiliary columns"
  expect_error(  safe_left_join(x, y, check =  "C", by = "name"), txt)
  expect_warning(safe_left_join(x, y, check =  "c", by = "name"), txt)
  expect_message(safe_left_join(x, y, check = "~c", by = "name"), txt)
  expect_message(eat(x, y, .check = "~c", .by = "name"), txt)
})

test_that("character 'm' works", {
  # all x rows matche
  x <- band_instruments %>% filter(name != "Keith")
  y <- band_members
  expect_silent(safe_left_join(x, y, check =  "M"))
  expect_silent(safe_left_join(x, y, check =  "m"))
  expect_silent(safe_left_join(x, y, check = "~m"))
  # some rows don't
  x <- band_instruments
  y <- band_members
  txt <- "x has unmatched"
  expect_error(  safe_left_join(x, y, check =  "M"), txt)
  expect_warning(safe_left_join(x, y, check =  "m"), txt)
  expect_message(safe_left_join(x, y, check = "~m"), txt)
  expect_message(eat(x, y, .check = "~m"), txt)
})


test_that("character 'e' works", {
  # x has all combinations
  x <- band_instruments %>%
    filter(name != "Keith") %>%
    add_row(name = "John", plays = "bass") %>%
    add_row(name = "Paul", plays = "bass") %>%
    mutate(j = c(1,2,2,1))
  y <- band_members %>% mutate(j = 1)
  expect_silent(safe_left_join(x, y, check =  "E"))
  expect_silent(safe_left_join(x, y, check =  "e"))
  expect_silent(safe_left_join(x, y, check = "~e"))
  # some rows don't
  x <- band_instruments %>%
    filter(name != "Keith") %>%
    mutate(j = c(1,2))
  y <- band_members  %>% mutate(j = 1)
  txt <- "Some combinations of"
  expect_error(  safe_left_join(x, y, check =  "E"), txt)
  expect_warning(safe_left_join(x, y, check =  "e"), txt)
  expect_message(safe_left_join(x, y, check = "~e"), txt)
  expect_message(eat(x, y, .check = "~e"), txt)
})

test_that("character 'f' works", {
  # x has all combinations
  x <- band_instruments %>% mutate(j = 1)

  y <- band_members %>%
    filter(name != "Mick") %>%
    add_row(name = "John", band = "The Who") %>%
    add_row(name = "Paul", band = "Led Zeppelin") %>%
    mutate(j = c(1,2,2,1))
  expect_silent(safe_left_join(x, y, check =  "F"))
  expect_silent(safe_left_join(x, y, check =  "f"))
  expect_silent(safe_left_join(x, y, check = "~f"))
  # some rows don't
  x <- band_instruments %>% mutate(j = 1)
  y <- band_members  %>%
    filter(name != "Mick") %>%
    mutate(j = c(1, 2))
  txt <- "Some combinations of"
  expect_error(  safe_left_join(x, y, check =  "F"), txt)
  expect_warning(safe_left_join(x, y, check =  "f"), txt)
  expect_message(safe_left_join(x, y, check = "~f"), txt)
  expect_message(eat(x, y, .check = "~f"), txt)
})

test_that("character 'd' works", {
  # x has all combinations
  x <- band_instruments
  y <- band_members
  expect_silent(eat(x, y, band, .check =  "D"))
  expect_silent(eat(x, y, band, .check =  "d"))
  expect_silent(eat(x, y, band, .check = "~d"))

  txt <- "columns must be given explicitly"
  expect_error(  eat(x, y, .check =  "D"), txt)
  txt <- "not provided"
  expect_warning(eat(x, y, .check =  "d"), txt)
  expect_message(eat(x, y, .check = "~d"), txt)
})
