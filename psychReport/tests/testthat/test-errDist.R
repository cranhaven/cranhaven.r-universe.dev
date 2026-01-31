context("errDist")

test_that("errDist", {

  set.seed(1)

  # test 1: 10% errors
  errs <- errDist(1000, 10)

  testthat::expect_equal(length(errs), 1000)
  testthat::expect_equal(sum(errs), 96)

  # test 2: 20% errors
  errs <- errDist(2000, 20)

  testthat::expect_equal(length(errs), 2000)
  testthat::expect_equal(sum(errs), 440)

  # test 3: 30% errors
  errs <- errDist(3000, 30)

  testthat::expect_equal(length(errs), 3000)
  testthat::expect_equal(sum(errs), 867)

  # test 5: 100% errors
  errs <- errDist(10, 100)

  testthat::expect_equal(length(errs), 10)
  testthat::expect_equal(sum(errs), 10)

})
