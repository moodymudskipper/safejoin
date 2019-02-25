context("ellipsis")

test_that("nothing unexpected is passed to dots", {
  expect_error(eat(band_instruments, band_members, CHECK = 3),
               "invalid column index")
  expect_error(eat(band_instruments, band_members, check = 3),
               "invalid column index")
})


