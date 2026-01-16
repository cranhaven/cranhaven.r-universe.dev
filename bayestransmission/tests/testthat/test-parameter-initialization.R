library(bayestransmission)

test_that("InsituParams initialization for 2-state model", {
  # For 2-state models, indices [0, 1, 2] = [uncolonized, latent, colonized]
  # Index 1 (latent) is unused, so we put values at indices 0 and 2
  
  # Test with explicit probability values
  probs_input <- c(0.9, 0, 0.1)  # colonized at index 2, not 1
  
  model_params <- LinearAbxModel(
    nstates = 2,
    Insitu = InsituParams(
      probs = probs_input,
      priors = c(0.9, 0, 0.1),
      doit = c(TRUE, FALSE, TRUE)
    )
  )
  
  # Extract model parameters would require running runMCMC
  # For now, verify the structure is correct
  expect_type(model_params, "list")
  expect_named(model_params, c("modname", "nstates", "nmetro", "forward", "cheat",
                                "Insitu", "SurveillanceTest", "ClinicalTest", 
                                "OutCol", "InCol", "Abx", "AbxRate"))
  expect_equal(model_params$Insitu$probs, probs_input)
})

test_that("RandomTestParams initialization for 2-state model", {
  # RandomTest uses indices [0, 1, 2] for [uncolonized, latent, colonized]
  # For 2-state, we pass values to uncolonized and latent positions
  # The latent values will appear in the colonized position in output
  
  unc_prob <- 0.5
  col_prob <- 0.5
  unc_rate <- 1.0
  col_rate <- 1.0
  
  model_params <- LinearAbxModel(
    nstates = 2,
    ClinicalTest = RandomTestParams(
      uncolonized = ParamWRate(
        param = Param(init = unc_prob, weight = 0, update = FALSE),
        rate = Param(init = unc_rate, weight = 0, update = FALSE)
      ),
      colonized = ParamWRate(
        param = Param(init = 0.0, weight = 0, update = FALSE),
        rate = Param(init = 0.0, weight = 0, update = FALSE)
      ),
      latent = ParamWRate(
        param = Param(init = col_prob, weight = 0, update = FALSE),
        rate = Param(init = col_rate, weight = 0, update = FALSE)
      )
    )
  )
  
  expect_type(model_params$ClinicalTest, "list")
  expect_equal(model_params$ClinicalTest$uncolonized$param$init, unc_prob)
  expect_equal(model_params$ClinicalTest$latent$param$init, col_prob)
})

test_that("LinearAbxAcquisitionParams initialization", {
  # Test that acquisition parameters are set correctly
  # LinearAbxICP::set() applies log/logit transformations internally
  # So we should NOT use dolog=true in modelsetup
  
  # Use 0.9999 for logit-transformed parameters (mass, freq) to avoid infinity
  model_params <- LinearAbxModel(
    nstates = 2,
    InUnit = ABXInUnitParams(
      acquisition = LinearAbxAcquisitionParams(
        base = Param(init = 0.001, weight = 1, update = TRUE),
        time = Param(init = 1.0, weight = 0, update = FALSE),
        mass = Param(init = 0.9999, weight = 1, update = TRUE),
        freq = Param(init = 0.9999, weight = 1, update = TRUE),
        col_abx = Param(init = 1.0, weight = 0, update = FALSE),
        suss_abx = Param(init = 1.0, weight = 0, update = FALSE),
        suss_ever = Param(init = 1.0, weight = 0, update = FALSE)
      )
    )
  )
  
  expect_equal(model_params$InCol$acquisition$base$init, 0.001)
  expect_equal(model_params$InCol$acquisition$time$init, 1.0)
  expect_equal(model_params$InCol$acquisition$mass$init, 0.9999)
  expect_equal(model_params$InCol$acquisition$freq$init, 0.9999)
})

test_that("AbxRateParams initialization for 2-state model", {
  # AbxParams uses indices [0, 1, 2] = [uncolonized, latent, colonized]
  # For 2-state models, colonized should be at index 2, not 1
  
  unc_rate <- 1.0
  col_rate <- 1.0
  
  model_params <- LinearAbxModel(
    nstates = 2,
    AbxRate = AbxRateParams(
      uncolonized = Param(init = unc_rate, weight = 0, update = FALSE),
      colonized = Param(init = col_rate, weight = 0, update = FALSE),
      latent = Param(init = 0.0, weight = 0, update = FALSE)
    )
  )
  
  expect_equal(model_params$AbxRate$uncolonized$init, unc_rate)
  expect_equal(model_params$AbxRate$colonized$init, col_rate)
  expect_equal(model_params$AbxRate$latent$init, 0.0)
})

test_that("Complete parameter initialization test with random values", {
  # Generate random parameters within valid ranges
  set.seed(123)
  
  # Insitu probabilities (must sum to 1 for indices 0 and 2)
  insitu_prob_col <- runif(1, 0.05, 0.5)
  insitu_prob_unc <- 1 - insitu_prob_col
  
  # Surveillance test probability
  surv_test_prob <- runif(1, 0.5, 0.9)
  
  # Random test probabilities and rates
  rtest_prob_unc <- runif(1, 0.1, 0.9)
  rtest_prob_col <- runif(1, 0.1, 0.9)
  rtest_rate_unc <- runif(1, 0.5, 2.0)
  rtest_rate_col <- runif(1, 0.5, 2.0)
  
  # Out of unit infection rates (small positive values)
  out_acquire <- runif(1, 0.0001, 0.01)
  out_clear <- runif(1, 0.001, 0.1)
  
  # LABX acquisition parameters
  labx_base <- runif(1, 0.0001, 0.01)
  labx_time <- runif(1, 0.5, 2.0)
  # Use values < 1.0 for logit-transformed parameters
  labx_mass <- runif(1, 0.9, 0.9999)
  labx_freq <- runif(1, 0.9, 0.9999)
  labx_col_abx <- runif(1, 0.5, 1.5)
  labx_suss_abx <- runif(1, 0.5, 1.5)
  labx_suss_ever <- runif(1, 0.5, 1.5)
  
  # LABX clearance parameters
  labx_clr <- runif(1, 0.001, 0.1)
  labx_clr_abx <- runif(1, 0.5, 1.5)
  labx_clr_ever <- runif(1, 0.5, 1.5)
  
  # Abx rates
  abx_rate_unc <- runif(1, 0.5, 2.0)
  abx_rate_col <- runif(1, 0.5, 2.0)
  
  # Create model parameters
  model_params <- LinearAbxModel(
    nstates = 2,
    Insitu = InsituParams(
      probs = c(insitu_prob_unc, 0, insitu_prob_col),
      priors = c(0.9, 0, 0.1),
      doit = c(TRUE, FALSE, TRUE)
    ),
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 0.0, weight = 0, update = FALSE),
      colonized = Param(init = surv_test_prob, weight = 1, update = TRUE),
      latent = Param(init = 0.0, weight = 0, update = FALSE)
    ),
    ClinicalTest = RandomTestParams(
      uncolonized = ParamWRate(
        param = Param(init = rtest_prob_unc, weight = 0, update = FALSE),
        rate = Param(init = rtest_rate_unc, weight = 0, update = FALSE)
      ),
      colonized = ParamWRate(
        param = Param(init = 0.0, weight = 0, update = FALSE),
        rate = Param(init = 0.0, weight = 0, update = FALSE)
      ),
      latent = ParamWRate(
        param = Param(init = rtest_prob_col, weight = 0, update = FALSE),
        rate = Param(init = rtest_rate_col, weight = 0, update = FALSE)
      )
    ),
    OutOfUnitInfection = OutOfUnitInfectionParams(
      acquisition = Param(init = out_acquire, weight = 1, update = TRUE),
      clearance = Param(init = out_clear, weight = 0, update = FALSE)
    ),
    InUnit = ABXInUnitParams(
      acquisition = LinearAbxAcquisitionParams(
        base = Param(init = labx_base, weight = 1, update = TRUE),
        time = Param(init = labx_time, weight = 0, update = FALSE),
        mass = Param(init = labx_mass, weight = 1, update = TRUE),
        freq = Param(init = labx_freq, weight = 1, update = TRUE),
        col_abx = Param(init = labx_col_abx, weight = 0, update = FALSE),
        suss_abx = Param(init = labx_suss_abx, weight = 0, update = FALSE),
        suss_ever = Param(init = labx_suss_ever, weight = 0, update = FALSE)
      ),
      clearance = ClearanceParams(
        rate = Param(init = labx_clr, weight = 1, update = TRUE),
        abx = Param(init = labx_clr_abx, weight = 0, update = FALSE),
        ever_abx = Param(init = labx_clr_ever, weight = 0, update = FALSE)
      )
    ),
    Abx = AbxParams(
      onoff = FALSE,
      delay = 0.0,
      life = 2.0
    ),
    AbxRate = AbxRateParams(
      uncolonized = Param(init = abx_rate_unc, weight = 0, update = FALSE),
      colonized = Param(init = abx_rate_col, weight = 0, update = FALSE),
      latent = Param(init = 0.0, weight = 0, update = FALSE)
    )
  )
  
  # Verify all parameters were set correctly
  expect_equal(model_params$Insitu$probs[1], insitu_prob_unc, tolerance = 1e-10)
  expect_equal(model_params$Insitu$probs[3], insitu_prob_col, tolerance = 1e-10)
  expect_equal(model_params$SurveillanceTest$colonized$init, surv_test_prob, tolerance = 1e-10)
  expect_equal(model_params$ClinicalTest$uncolonized$param$init, rtest_prob_unc, tolerance = 1e-10)
  expect_equal(model_params$ClinicalTest$latent$param$init, rtest_prob_col, tolerance = 1e-10)
  expect_equal(model_params$OutCol$acquisition$init, out_acquire, tolerance = 1e-10)
  expect_equal(model_params$OutCol$clearance$init, out_clear, tolerance = 1e-10)
  expect_equal(model_params$InCol$acquisition$base$init, labx_base, tolerance = 1e-10)
  expect_equal(model_params$InCol$acquisition$mass$init, labx_mass, tolerance = 1e-10)
  expect_equal(model_params$AbxRate$uncolonized$init, abx_rate_unc, tolerance = 1e-10)
  expect_equal(model_params$AbxRate$colonized$init, abx_rate_col, tolerance = 1e-10)
})

test_that("Parameter validation - logit boundary values", {
  # Test that logit-transformed parameters handle boundary values correctly
  
  # Should accept 0.9999 (safe value)
  expect_silent({
    model_params <- LinearAbxModel(
      nstates = 2,
      InUnit = ABXInUnitParams(
        acquisition = LinearAbxAcquisitionParams(
          mass = Param(init = 0.9999, weight = 1, update = TRUE),
          freq = Param(init = 0.9999, weight = 1, update = TRUE)
        )
      )
    )
  })
  
  # Test with very small values (should also work)
  expect_silent({
    model_params <- LinearAbxModel(
      nstates = 2,
      InUnit = ABXInUnitParams(
        acquisition = LinearAbxAcquisitionParams(
          mass = Param(init = 0.0001, weight = 1, update = TRUE),
          freq = Param(init = 0.0001, weight = 1, update = TRUE)
        )
      )
    )
  })
})

test_that("3-state model parameter initialization", {
  # Test that 3-state models work correctly (includes latent state)
  
  insitu_probs <- c(0.8, 0.05, 0.15)  # unc, lat, col
  
  model_params <- LinearAbxModel(
    nstates = 3,
    Insitu = InsituParams(
      probs = insitu_probs,
      priors = c(0.8, 0.05, 0.15),
      doit = c(TRUE, TRUE, TRUE)
    ),
    ClinicalTest = RandomTestParams(
      uncolonized = ParamWRate(
        param = Param(init = 0.1, weight = 0, update = FALSE),
        rate = Param(init = 1.0, weight = 0, update = FALSE)
      ),
      colonized = ParamWRate(
        param = Param(init = 0.9, weight = 0, update = FALSE),
        rate = Param(init = 1.0, weight = 0, update = FALSE)
      ),
      latent = ParamWRate(
        param = Param(init = 0.5, weight = 0, update = FALSE),
        rate = Param(init = 1.0, weight = 0, update = FALSE)
      )
    ),
    AbxRate = AbxRateParams(
      uncolonized = Param(init = 1.0, weight = 0, update = FALSE),
      colonized = Param(init = 1.0, weight = 0, update = FALSE),
      latent = Param(init = 1.0, weight = 0, update = FALSE)
    )
  )
  
  expect_equal(model_params$nstates, 3)
  expect_equal(model_params$Insitu$probs, insitu_probs)
  expect_equal(model_params$ClinicalTest$latent$param$init, 0.5)
  expect_equal(model_params$AbxRate$latent$init, 1.0)
})

test_that("Index mapping consistency across parameter types", {
  # Verify that all parameter types use consistent index mapping
  # For 2-state: [0, 1, 2] = [uncolonized, latent(unused), colonized]
  
  model_params <- LinearAbxModel(
    nstates = 2,
    Insitu = InsituParams(
      probs = c(0.9, 0, 0.1),  # colonized at index 2
      priors = c(0.9, 0, 0.1),
      doit = c(TRUE, FALSE, TRUE)
    ),
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 0.0, weight = 0, update = FALSE),
      colonized = Param(init = 0.8, weight = 1, update = TRUE),
      latent = Param(init = 0.0, weight = 0, update = FALSE)
    ),
    ClinicalTest = RandomTestParams(
      uncolonized = ParamWRate(
        param = Param(init = 0.5, weight = 0, update = FALSE),
        rate = Param(init = 1.0, weight = 0, update = FALSE)
      ),
      colonized = ParamWRate(
        param = Param(init = 0.0, weight = 0, update = FALSE),
        rate = Param(init = 0.0, weight = 0, update = FALSE)
      ),
      latent = ParamWRate(
        param = Param(init = 0.5, weight = 0, update = FALSE),
        rate = Param(init = 1.0, weight = 0, update = FALSE)
      )
    ),
    AbxRate = AbxRateParams(
      uncolonized = Param(init = 1.0, weight = 0, update = FALSE),
      colonized = Param(init = 1.0, weight = 0, update = FALSE),
      latent = Param(init = 0.0, weight = 0, update = FALSE)
    )
  )
  
  # All should have colonized values properly positioned
  expect_equal(model_params$Insitu$probs[3], 0.1)  # colonized at index 3 (0-based index 2)
  expect_equal(model_params$SurveillanceTest$colonized$init, 0.8)
  expect_equal(model_params$ClinicalTest$latent$param$init, 0.5)  # colonized value passed as latent
  expect_equal(model_params$AbxRate$colonized$init, 1.0)
})
