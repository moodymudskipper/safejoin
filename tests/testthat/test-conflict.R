context("conflict")

test_that("conflict works", {
  x <- band_instruments  %>% mutate(plays = c(NA,"bass","guitar"))
  y <- band_members %>% mutate(plays = c(NA,"GUITAR","BASS")) %>%
    add_row(name = "Keith",plays = NA)
  expect_equal(
    x %>% eat(y, by = "name", conflict = coalesce) %>% pull(plays),
    c("GUITAR", "bass", "guitar"))
  expect_equal(
    x %>% eat(y, by = "name", conflict = ~coalesce(.y,.x))  %>% pull(plays),
    c("GUITAR", "BASS", "guitar"))
  expect_equal(
    x %>% eat(y, by = "name", conflict = "patch") %>% pull(plays),
    c("GUITAR", "BASS", NA))
})

