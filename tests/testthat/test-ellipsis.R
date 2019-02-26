context("ellipsis")

test_that("nothing unexpected is passed to dots", {
  expect_error(eat(band_instruments, band_members, CHECK = 3),
               "invalid column index")
  expect_error(eat(band_instruments, band_members, check = 3),
               "invalid column index")
  expect_error(safe_left_join(band_instruments, band_members, .CHECK = 3))
  expect_error(safe_left_join(band_instruments, band_members, .check = 3))
})


# debugonce(safe_check)
# eat(band_instruments, band_members, conflict=coalesce)
