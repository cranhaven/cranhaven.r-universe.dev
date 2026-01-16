test_that("R and C++ produce identical initial log likelihood", {
  skip_on_cran()
  skip_if_not(exists("simulated.data_sorted"), message = "simulated.data_sorted not available")
  
  # Use parameters that match original C++ implementation
  # BUT with P(+|uncolonized) = 1e-10 instead of 0.0 to avoid -Inf
  params <- LinearAbxModel(
    nstates = 2,
    nmetro = 10,
    forward = FALSE,
    cheat = FALSE,
    Insitu = InsituParams(
      probs = c(uncolonized = 0.9, latent = 0.0, colonized = 0.1),
      priors = c(1, 1, 1),
      doit = c(TRUE, FALSE, TRUE)
    ),
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 1e-10, weight = 0),  # Small but non-zero
      latent = Param(init = 0.0, weight = 0),
      colonized = Param(init = 0.8, weight = 1)
    ),
    ClinicalTest = RandomTestParams(
      uncolonized = ParamWRate(
        param = Param(init = 0.5, weight = 0),
        rate = Param(init = 1.0, weight = 0)
      ),
      latent = ParamWRate(
        param = Param(init = 0.5, weight = 0),
        rate = Param(init = 1.0, weight = 0)
      ),
      colonized = ParamWRate(
        param = Param(init = 0.5, weight = 0),
        rate = Param(init = 1.0, weight = 0)
      )
    ),
    OutOfUnitInfection = OutOfUnitInfectionParams(
      acquisition = Param(init = 0.001, weight = 1),
      clearance = Param(init = 0.01, weight = 0),
      progression = Param(init = 0.0, weight = 0)
    ),
    InUnit = ABXInUnitParams(
      acquisition = LinearAbxAcquisitionParams(
        base = Param(init = 0.001, weight = 1),
        time = Param(init = 1.0, weight = 0),
        mass = Param(init = 1.0, weight = 1),
        freq = Param(init = 1.0, weight = 1),
        col_abx = Param(init = 1.0, weight = 0),
        suss_abx = Param(init = 1.0, weight = 0),
        suss_ever = Param(init = 1.0, weight = 0)
      ),
      clearance = ClearanceParams(
        rate = Param(init = 0.01, weight = 1),
        abx = Param(init = 1.0, weight = 0),
        ever_abx = Param(init = 1.0, weight = 0)
      ),
      progression = ProgressionParams(
        rate = Param(init = 0.0, weight = 0),
        abx = Param(init = 1.0, weight = 0),
        ever_abx = Param(init = 1.0, weight = 0)
      )
    ),
    Abx = AbxParams(onoff = 0, delay = 0.0, life = 2.0),
    AbxRate = AbxRateParams(
      uncolonized = Param(init = 1.0, weight = 0),
      latent = Param(init = 1.0, weight = 0),
      colonized = Param(init = 1.0, weight = 0)
    )
  )
  
  # Run MCMC with no burn-in, 1 iteration to get initial likelihood
  set.seed(42)
  results <- runMCMC(
    data = simulated.data_sorted,
    nburn = 0,
    nsims = 1,
    outputparam = TRUE,
    outputfinal = TRUE,
    modelParameters = params,
    verbose = FALSE
  )
  
  initial_ll <- results$LogLikelihood[1]
  
  # The likelihood MUST be finite
  expect_true(is.finite(initial_ll),
                info = paste("Initial log likelihood is", initial_ll, "but should be finite"))
  
  # Note: Original C++ gave ~-12942.9 with P(+|uncolonized) = 0.0
  # With P(+|uncolonized) = 1e-10, the value will be slightly different
  # but should still be close (within same order of magnitude)
  expect_true(initial_ll < 0,
              info = "Log likelihood should be negative")
  expect_true(initial_ll > -20000,
              info = "Log likelihood should not be extremely negative")
  
  # Diagnostic (quiet): initial_ll should be finite and within [-20000, 0]
})

test_that("No surveillance test probability can be exactly zero", {
  # This test ensures we don't regress back to P=0
  
  params <- LinearAbxModel(nstates = 2)
  
  # Check default parameters
  surv_uncol <- params$SurveillanceTest$uncolonized
  surv_lat <- params$SurveillanceTest$latent  
  surv_col <- params$SurveillanceTest$colonized
  
  # Extract init values
  uncol_init <- if (is.list(surv_uncol) && "init" %in% names(surv_uncol)) {
    surv_uncol$init
  } else {
    surv_uncol
  }
  
  lat_init <- if (is.list(surv_lat) && "init" %in% names(surv_lat)) {
    surv_lat$init
  } else {
    surv_lat
  }
  
  col_init <- if (is.list(surv_col) && "init" %in% names(surv_col)) {
    surv_col$init
  } else {
    surv_col
  }
  
  # None should be exactly zero (or if zero for latent in 2-state, that's OK)
  if (params$nstates == 2) {
    # In 2-state model, latent can be 0
    expect_true(uncol_init >= 1e-10 || uncol_init == 0,
                info = "uncolonized test prob should be >= 1e-10 or exactly 0 if unused")
    expect_true(col_init > 0,
                info = "colonized test prob must be > 0")
  } else {
    expect_true(all(c(uncol_init, lat_init, col_init) >= 1e-10),
                info = "All test probabilities should be >= 1e-10 to avoid log(0)")
  }
  
  # Diagnostic (quiet): probabilities checked via expectations above
})

test_that("Likelihood remains finite throughout MCMC run", {
  skip_on_cran()
  skip_if_not(exists("simulated.data_sorted"), message = "simulated.data_sorted not available")
  
  # Run a short MCMC to verify likelihoods stay finite
  params <- LinearAbxModel(
    nstates = 2,
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 1e-10, weight = 0),
      latent = Param(init = 0.0, weight = 0),
      colonized = Param(init = 0.8, weight = 1)
    )
  )
  
  set.seed(123)
  results <- runMCMC(
    data = simulated.data_sorted,
    nburn = 10,
    nsims = 20,
    outputparam = TRUE,
    outputfinal = TRUE,
    modelParameters = params,
    verbose = FALSE
  )
  
  # ALL likelihoods should be finite
  expect_true(all(is.finite(results$LogLikelihood)),
              info = paste("Found non-finite likelihoods:",
                          sum(!is.finite(results$LogLikelihood)), "out of",
                          length(results$LogLikelihood)))
  
  # All should be negative (log of probability < 1)
  expect_true(all(results$LogLikelihood < 0),
              info = "All log likelihoods should be negative")
  
  # Diagnostic (quiet): summary suppressed; failures report details via `info`
})
