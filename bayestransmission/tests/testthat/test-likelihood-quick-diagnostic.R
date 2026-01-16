test_that("Quick diagnostic: Model initialization check", {
  # This test checks if we can even CREATE a model without errors
  
  params <- LinearAbxModel(
    nstates = 2,
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 0.0, weight = 0),
      latent = Param(init = 0.0, weight = 0),
      colonized = Param(init = 0.8, weight = 1)
    )
  )
  
  expect_true(is.list(params))
  expect_equal(params$modname, "LinearAbxModel", info = "Model name should be 'LinearAbxModel'")
  
  # Try to create the C++ model object
  model <- newCppModel(params, verbose = FALSE)
  expect_s4_class(model, "Rcpp_CppLinearAbxModel")
})

test_that("Quick diagnostic: Check for P=0 in surveillance test", {
  # The issue is likely here: P(+|uncolonized) = 0.0
  # If ANY patient who is uncolonized gets a positive test, log(0) = -Inf
  
  params_zero <- LinearAbxModel(
    nstates = 2,
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 0.0, weight = 0),  # PROBLEM: Zero probability
      latent = Param(init = 0.0, weight = 0),
      colonized = Param(init = 0.8, weight = 1)
    )
  )
  
  params_small <- LinearAbxModel(
    nstates = 2,
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 1e-10, weight = 0),  # FIX: Small but non-zero
      latent = Param(init = 0.0, weight = 0),
      colonized = Param(init = 0.8, weight = 1)
    )
  )
  
  # Both should create successfully
  model_zero <- newCppModel(params_zero, verbose = FALSE)
  model_small <- newCppModel(params_small, verbose = FALSE)
  
  expect_s4_class(
    model_zero,
    "Rcpp_CppLinearAbxModel"
  )
  expect_s4_class(
    model_small,
    "Rcpp_CppLinearAbxModel"
  )
})

test_that("Quick diagnostic: Examine simulated.data_sorted for impossible events", {
  data <- simulated.data_sorted
  
  # Get all surveillance test events
  surv_pos <- data[data$type == 2, ]  # Positive surveillance tests
  surv_neg <- data[data$type == 1, ]  # Negative surveillance tests
  
  # Note: If P(+|uncolonized)=0, any positive assigned to an uncolonized state yields log(0)=-Inf.
  # Recommendation: Use P(+|uncolonized) >= 1e-10 (never exactly 0).
  # Data summary: total=nrow(data), positive surv=nrow(surv_pos), negative surv=nrow(surv_neg)
  
  expect_gt(nrow(surv_pos), 0)
})
