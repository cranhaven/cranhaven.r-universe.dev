context("rtDist")

test_that("rtDist", {

  set.seed(1)

  # test 1
  x <- rtDist()
  testthat::expect_equal(length(x), 10000)
  testthat::expect_equal(600, round(mean(x)))

  # test 2
  x <- rtDist(n = 100000, gaussMean = 500)
  testthat::expect_equal(length(x), 100000)
  testthat::expect_equal(500, round(mean(x)))

})
