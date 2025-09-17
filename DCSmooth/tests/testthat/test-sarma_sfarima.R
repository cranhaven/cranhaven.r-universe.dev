################################################################################
#                                                                              #
#                           Test for SARMA/SFARIMA                             #
#                                                                              #
################################################################################

### Test simulation
context("Test SARMA simulation")

test_that("exception handling works for sarma.sim()", {
  expect_error(sarma.sim(100, 100, model = list(ar = 1, ma = "a", sigma = 1)),
               "Only numerical values in subelements")
  expect_error(sarma.sim(100, 100, model = list(ar = 1, ma = 1, sigma = NA)),
               "Missing values in subelements")
  expect_error(sarma.sim(100, 100, model = list(ar = 1, ma = 1,
               sigma = c(0.5, 0.5))), "should be a scalar value.")
  expect_warning(sarma.sim(100, 100, model = list(ar = 0.5, ma = 1, 
                 sigma = 0.5)), "Entry in upper left")
})

test_that("results of sarma.sim() ar correct", {
  # set up Model
  ar = matrix(c(1, 0.3, -0.2, -0.06), 2, 2)
  ma = matrix(c(1, -0.5, -0.2, 0.1), 2, 2)
  model = list(ar = ar, ma = ma, sigma = 1)
  
  sarma_sim = sarma.sim(104, 104, model = model)
  
  expect_equal(class(sarma_sim), "sarma")
  expect_equal(attr(sarma_sim, "subclass"), "sim")
  expect_equal(dim(sarma_sim$Y), c(104, 104))
  expect_true(is.numeric(sarma_sim$Y))
  expect_true(is.matrix(sarma_sim$Y))
  expect_equal(dim(sarma_sim$innov), c(104, 104))
  expect_true(is.numeric(sarma_sim$innov))
  expect_true(is.matrix(sarma_sim$innov))
  expect_equal(sarma_sim$model, model)
  expect_true(is.logical(sarma_sim$stnry))
})

context("Test SARMA estimation")

test_that("exception handling works for sarma.est()", {
  # set up Model
  ar = matrix(c(1, 0.3, -0.2, -0.06), 2, 2)
  ma = matrix(c(1, -0.5, -0.2, 0.1), 2, 2)
  model = list(ar = ar, ma = ma, sigma = 1)
  
  sarma_sim = sarma.sim(104, 104, model = model)$Y
  Y = sarma_sim
  Y[5, 5] = NA
  
  expect_error(sarma.est(Y), "Y contains missing values")
})
