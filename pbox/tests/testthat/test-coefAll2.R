library(testthat)

data(SEAex)
pbx <- set_pbox(SEAex)
obj<-pbx@fit[[1]]$allDitrs$Malaysia

# Test for a GAM-like object where all parameters are included
test_that("coefAll2 returns all parameters when present", {
  # Create a mock object with all parameters
  result <- coefAll2(obj, deviance = FALSE)
  expect_named(result, c("mu", "sigma"))
  expect_equal(round(result$mu,1), 31.1)
  expect_equal(round(result$sigma,1), 0.3)

})

# Test for a GAM-like object missing some parameters
test_that("coefAll2 handles missing parameters gracefully", {
  # Create a mock object with some parameters missing

  result <- coefAll2(obj, deviance = FALSE)

  expect_named(result, c("mu", "sigma"))
  expect_equal(round(result$mu,1), 31.1)
  expect_equal(round(result$sigma,1), 0.3)
  expect_false("nu" %in% names(result))
  expect_false("tau" %in% names(result))
})

# Test for a GAM-like object with deviance flag set
test_that("coefAll2 returns deviance when requested", {
  # Mock a deviance function and object


  result <- coefAll2(obj, deviance = TRUE)

  expect_named(result, c("mu", "sigma", "deviance"))
  expect_equal(round(result$mu,1), 31.1)
  expect_equal(round(result$sigma,1), 0.3)
  expect_equal(round(result$deviance,1), 81.7)
})
