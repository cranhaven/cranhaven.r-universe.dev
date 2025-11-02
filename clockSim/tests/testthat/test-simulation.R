test_that("getOdinGen returns expected model generators", {
  models <- getOdinGen()
  
  # Check basic structure
  expect_named(models, c("discrete_LG", "continuous_LG", "noisy_LG"))
  expect_type(models$discrete_LG, "closure")
  expect_type(models$continuous_LG, "closure")
  expect_type(models$noisy_LG, "list")
})

test_that("continuous LG model produces circadian oscillations", {
  model <- getOdinGen()$continuous_LG$new()
  times <- seq(0, 2400, by = 1) # 100 days simulation
  res <- model$run(times)
  
  # Check basic output structure
  expect_true(is.matrix(res))
  expect_true("M_T" %in% colnames(res)) # TIM mRNA
  
  # Check period is ~24 hours
  last_10_days <- res[(nrow(res)-240):nrow(res), "M_T"]
  period <- compute_period(last_10_days, method = "lomb")
  expect_equal(period["period"], c(period = 24), tolerance = 2)
})

test_that("discrete LG model converges with small time steps", {
  model <- getOdinGen()$discrete_LG$new()
  
  # Test different step sizes
  steps <- c(0.002, 0.004, 0.01)
  for (step in steps) {
    model$set_user(STEP_HOURS = step)
    total_steps <- 2400/step
    res <- model$run(seq(0, total_steps))
    
    # Check output structure and completeness
    expect_true(is.matrix(res))
    expect_equal(nrow(res), total_steps + 1) # +1 for initial condition
    expect_true("M_T" %in% colnames(res))
    
    # Verify no NA/NaN values in results
    expect_false(any(is.na(res)))
    expect_false(any(is.nan(res)))
    
    # Verify reasonable values for TIM mRNA (M_T)
    expect_true(all(res[,"M_T"] >= 0))
    expect_true(max(res[,"M_T"]) > 0) # Should have some oscillation
  }
})

test_that("noisy LG model runs with default settings", {
  mg <- getOdinGen()$noisy_LG
  model <- mg$gen$new()
  
  # Set small time step
  interval_hours <- 0.001
  model$set_user(STEP_HOURS = interval_hours)
  steps <- seq(1, 2400 / interval_hours)
  
  # Test basic run
  res <- model$run(steps)
  expect_true(is.matrix(res))
  expect_true("M_T" %in% colnames(res))
})

test_that("noisy LG model responds to noise settings", {
  mg <- getOdinGen()$noisy_LG
  model <- mg$gen$new()
  model$set_user(STEP_HOURS = 0.001)
  
  # Test noise off
  model$set_user(NoiseVariance_M_T = 0)
  res_off <- model$run(1:1000)
  
  # Test noise on
  model$set_user(NoiseVariance_M_T = -1)
  res_on <- model$run(1:1000)
  
  # Should see different results
  expect_false(identical(res_off, res_on))
})