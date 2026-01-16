# Tests for classes exposed in src/Module-lognormal.cpp
# Classes are tested in the order they appear in the C++ file

# Test data setup
data(simulated.data, package = "bayestransmission")

# 1. LogNormalICP ----
test_that("CppLogNormalICP class", {
  skip("Need to create standalone test - currently tested via integration")
})

# 2. LogNormalAbxICP ----
test_that("CppLogNormalAbxICP class", {
  skip("Need to create standalone test - currently tested via integration")
})

# 3. LogNormalModel ----
test_that("CppLogNormalModel class", {
  skip("Need to create standalone test - currently tested via integration")
})

# 4. LinearAbxModel ----
test_that("CppLinearAbxModel constructor and basic properties", {
  sys <- CppSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )

  model <- CppLinearAbxModel$new(2, 10, 1, 0)
  expect_s4_class(model, "Rcpp_CppLinearAbxModel")

  # Test that InColParams exists
  icp <- model$InColParams
  expect_s4_class(icp, "Rcpp_CppLogNormalAbxICP")

  # Set time origin
  icp$timeOrigin <- (sys$endTime() - sys$startTime()) / 2

  # Create history
  hist <- CppSystemHistory$new(sys, model, FALSE)
  expect_s4_class(hist, "Rcpp_CppSystemHistory")

  # Test logLikelihood
  ll <- model$logLikelihood(hist)
  expect_type(ll, "double")
})

test_that("CppLinearAbxModel with SystemHistory", {
  sys <- CppSystem$new(
    simulated.data$facility,
    simulated.data$unit,
    simulated.data$time,
    simulated.data$patient,
    simulated.data$type
  )

  model <- CppLinearAbxModel$new(2, 10, 1, 0)
  hist <- CppSystemHistory$new(sys, model, FALSE)

  # Test UnitHeads (requires ALL_CLASSES)
  skip("UnitHeads requires ALL_CLASSES")
  h <- hist$UnitHeads
  expect_s4_class(h, "Rcpp_CppMap")
  expect_equal(h$size, 3)
})

# 5. LinearAbxModel2 ----
test_that("CppLinearAbxModel2 constructor", {
  model2 <- CppLinearAbxModel2$new(2, 10, 1, 0)
  expect_s4_class(model2, "Rcpp_CppLinearAbxModel2")
})
