context("ciStrT")

test_that("ciStrT", {

  # simulated data for t-test
  set.seed(1)

  comp   <- rtDist(100, 500, 50, 200)
  incomp <- rtDist(100, 550, 50, 200)
  tObj   <- t.test(incomp, comp, paired = TRUE)

  testthat::expect_equal(ciStrT(tObj), "95\\% CI: -9 to 117")
  testthat::expect_equal(ciStrT(tObj, unit = "ms"), "95\\% CI: -9 to 117 ms")
  testthat::expect_equal(ciStrT(tObj, unit = "%"), "95\\% CI: -9 to 117 \\%")

})
