
test_that("File can be named given a year", {
  expect_equal(make_filename(2012),"accident_2012.csv")
})
