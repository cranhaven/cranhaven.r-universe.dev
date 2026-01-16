test_that("LogNormalModel example does not segfault and InColParams accessible", {
  params <- LogNormalModelParams("LogNormalModel")
  model <- newCppModel(params)
  expect_true(inherits(model, "C++Object"))
  # Access InColParams (was causing segfault previously due to wrong constructor args)
  icp <- model$InColParams
  rm(icp)
  gc()
  icp <- model$InColParams
  expect_true(inherits(icp, "C++Object"))
  # Values should be numeric vector length >= 1
  vals <- icp$values
  expect_true(is.numeric(vals))
  expect_true(length(vals) >= 1)
})
