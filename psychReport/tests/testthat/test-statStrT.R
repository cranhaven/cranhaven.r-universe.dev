context("statStrT")

test_that("statStrT", {

  # simulated data for t-test
  set.seed(1)

  comp   <- rtDist(100, 500, 50, 200)
  incomp <- rtDist(100, 550, 50, 200)
  tObj   <- t.test(incomp, comp, paired = TRUE)

  testthat::expect_equal(statStrT(tObj), "\\emph{t}(99) = 1.70, \\emph{p} = .092")

})
