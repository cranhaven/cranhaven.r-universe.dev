context("meanStrT")

test_that("meanStrT", {

  set.seed(1)

  # simulated data for t-test
  comp   <- rtDist(100, 500, 50, 200)
  incomp <- rtDist(100, 550, 50, 200)
  tObj   <- t.test(incomp, comp, paired = TRUE)

  testthat::expect_equal(meanStrT(tObj), "54")
  testthat::expect_equal(meanStrT(tObj, unit = "ms"), "54 ms")
  testthat::expect_equal(meanStrT(tObj, unit = "%"), "54 \\%")

  tObj   <- t.test(incomp, comp, paired = FALSE)
  testthat::expect_equal(meanStrT(tObj), "553 vs. 499")
  testthat::expect_equal(meanStrT(tObj, unit = "ms"), "553 vs. 499 ms")

})
