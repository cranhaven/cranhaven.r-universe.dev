context("tValueString")

test_that("tValueString", {

  set.seed(1)

  # simulated data for t-test
  comp   <- rtDist(100, 500, 50, 200)
  incomp <- rtDist(100, 550, 50, 200)
  tObj   <- t.test(incomp, comp, paired = TRUE)
  testthat::expect_equal(tValueString(tObj), "\\emph{t}(99) = 1.70")

  tObj   <- t.test(incomp, comp, paired = FALSE)
  testthat::expect_equal(tValueString(tObj), "\\emph{t}(193.61) = 1.83")

})
