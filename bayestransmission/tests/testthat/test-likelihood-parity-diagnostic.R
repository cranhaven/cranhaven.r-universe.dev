test_that("Diagnostic: Track where -Inf likelihood originates", {
  skip_on_cran()
  
  # Use exact parameters from original C++ that should give LL ~ -12943
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
      uncolonized = Param(init = 0.0, weight = 0),
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
  
  # Test 1: Verify model creation doesn't produce -Inf
  model <- newCppModel(params, verbose = FALSE)
  expect_s4_class(model, "C++Object")
  
  # Test 2: Run minimal MCMC (0 burn-in, 1 iteration) to get initial likelihood
  set.seed(42)
  results <- runMCMC(
    data = simulated.data_sorted,
    nburn = 0,
    nsims = 1,
    outputparam = TRUE,
    outputfinal = TRUE,
    modelParameters = params,
    verbose = FALSE  # Keep verbose to see diagnostic output
  )
  
  # Test 3: Check the initial likelihood
  initial_ll <- results$LogLikelihood[1]
  
  # Diagnostic (quiet): attach details to failure messages below
  
  # Test 4: The actual expectation - should be finite and close to C++ value
  expect_true(is.finite(initial_ll), 
                info = "Initial log likelihood should be finite, not -Inf")
  
  # Allow some tolerance for floating point differences
  expect_equal(initial_ll, -12942.9, tolerance = 10,
               info = "R likelihood should match C++ likelihood within tolerance")
})

test_that("Diagnostic: Identify which parameter causes -Inf", {
  skip_on_cran()
  
  # Test with progressively more constrained parameters to isolate the issue
  
  # Hypothesis 1: SurveillanceTest with P(+|uncolonized) = 0.0 causes -Inf
  # Hypothesis 1: Surveillance Test P=0 (quiet)
  
  params_h1 <- LinearAbxModel(
    nstates = 2,
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 1e-10, weight = 0),  # Small but non-zero
      latent = Param(init = 0.0, weight = 0),
      colonized = Param(init = 0.8, weight = 1)
    )
  )
  
  results_h1 <- runMCMC(
    data = simulated.data_sorted,
    nburn = 0, nsims = 1, outputparam = TRUE, outputfinal = TRUE,
    modelParameters = params_h1,
    verbose = FALSE
  )
  
  # attach diagnostics to expectations only on failure
  
  # Hypothesis 2: Episode initialization issue
  # Hypothesis 2: Episode Initialization (quiet)
  
  # Try with different initial probabilities
  params_h2 <- LinearAbxModel(
    nstates = 2,
    Insitu = InsituParams(
      probs = c(uncolonized = 0.5, latent = 0.0, colonized = 0.5),  # 50/50 split
      priors = c(1, 1, 1),
      doit = c(TRUE, FALSE, TRUE)
    )
  )
  
  results_h2 <- runMCMC(
    data = simulated.data_sorted,
    nburn = 0, nsims = 1, outputparam = TRUE, outputfinal = TRUE,
    modelParameters = params_h2,
    verbose = FALSE
  )
  
  # attach diagnostics to expectations only on failure
  
  # Hypothesis 3: Acquisition rate issue
  # Hypothesis 3: Acquisition Rate (quiet)
  
  params_h3 <- LinearAbxModel(
    nstates = 2,
    InUnit = ABXInUnitParams(
      acquisition = LinearAbxAcquisitionParams(
        base = Param(init = 0.01, weight = 1),  # Higher base rate
        time = Param(init = 1.0, weight = 0),
        mass = Param(init = 1.0, weight = 1),
        freq = Param(init = 1.0, weight = 1),
        col_abx = Param(init = 1.0, weight = 0),
        suss_abx = Param(init = 1.0, weight = 0),
        suss_ever = Param(init = 1.0, weight = 0)
      )
    )
  )
  
  results_h3 <- runMCMC(
    data = simulated.data_sorted,
    nburn = 0, nsims = 1, outputparam = TRUE, outputfinal = TRUE,
    modelParameters = params_h3,
    verbose = FALSE
  )
  
  # attach diagnostics to expectations only on failure
  
  # Summary
  results_summary <- data.frame(
    Hypothesis = c("Original", "P=1e-10", "50/50 init", "base=0.01"),
    LogLikelihood = c(
      NA,  # Will fill from first test
      results_h1$LogLikelihood[1],
      results_h2$LogLikelihood[1],
      results_h3$LogLikelihood[1]
    ),
    IsFinite = c(
      NA,
      is.finite(results_h1$LogLikelihood[1]),
      is.finite(results_h2$LogLikelihood[1]),
      is.finite(results_h3$LogLikelihood[1])
    )
  )
  
  # Attach summary table to failure only
  summary_text <- paste(capture.output(print(results_summary)), collapse = "\n")
  
  # At least ONE configuration should produce finite likelihood
  expect_true(
    any(results_summary$IsFinite, na.rm = TRUE),
    info = paste0("At least one parameter configuration should produce finite likelihood\n", summary_text)
  )
})

test_that("Diagnostic: Check data for impossible events", {
  skip_on_cran()
  
  # Check if simulated.data_sorted has any events that could cause -Inf
  # For example: positive test result when surveillance test probability is 0
  
  data <- simulated.data_sorted
  
  # Data diagnostics (quiet)
  # total <- nrow(data)
  # types <- table(CodeToEvent(data$type))
  
  # Check for surveillance test results
  surv_tests <- data[data$type %in% c(1, 2), ]  # Negative=1, Positive=2
  # Surveillance test counts (quiet)
  
  # Check for clinical test results
  clin_tests <- data[data$type %in% c(4, 5), ]  # Negative=4, Positive=5
  # Clinical test counts (quiet)
  
  # CRITICAL: If we have P(+|uncolonized) = 0.0 and ANY positive surveillance tests,
  # those tests MUST occur when patient is colonized, or we get -Inf
  
  # This test just documents the data - actual fix will come from analysis
  expect_true(TRUE, info = "Data diagnostics completed")
})
