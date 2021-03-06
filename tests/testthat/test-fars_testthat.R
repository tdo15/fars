context("fars_testthat")

test_that("filename generation works", {
  expect_that(make_filename(2013), is_identical_to("accident_2013.csv.bz2"))
})

test_that("data import works", {
  expect_that(nrow(fars_read_years(2013)[[1]]), equals(30202))
})
