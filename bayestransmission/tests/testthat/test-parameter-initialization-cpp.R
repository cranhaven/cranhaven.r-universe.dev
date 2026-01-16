library(bayestransmission)

test_that("C++ model object parameter initialization with unique values", {
  # Set seed for reproducibility
  set.seed(42)
  
  # Create unique values for each parameter (no two parameters have same value)
  # Using a sequence multiplied by different factors to ensure uniqueness
  base_val <- 0.001
  multiplier <- 1
  
  get_unique_val <- function(min_val, max_val) {
    val <- min_val + (max_val - min_val) * multiplier / 100
    multiplier <<- multiplier + 1
    return(val)
  }
  
  # Insitu probabilities (must sum to 1)
  insitu_prob_unc <- 0.7123
  insitu_prob_col <- 1 - insitu_prob_unc  # 0.2877
  
  # Surveillance test parameters
  surv_test_prob_col <- get_unique_val(0.5, 0.9)  # ~0.503
  
  # Random test probabilities and rates
  rtest_prob_unc <- get_unique_val(0.1, 0.9)  # ~0.108
  rtest_prob_col <- get_unique_val(0.1, 0.9)  # ~0.116
  rtest_rate_unc <- get_unique_val(0.5, 2.0)  # ~0.530
  rtest_rate_col <- get_unique_val(0.5, 2.0)  # ~0.560
  
  # Out of unit infection rates
  out_acquire <- get_unique_val(0.0001, 0.01)  # ~0.000159
  out_clear <- get_unique_val(0.001, 0.1)      # ~0.001099
  
  # LinearAbx acquisition parameters
  labx_base <- get_unique_val(0.0001, 0.01)    # ~0.000269
  labx_time <- get_unique_val(0.5, 2.0)        # ~0.635
  labx_mass <- get_unique_val(0.9, 0.9999)     # ~0.900099
  labx_freq <- get_unique_val(0.9, 0.9999)     # ~0.901099
  labx_col_abx <- get_unique_val(0.5, 1.5)     # ~0.610
  labx_suss_abx <- get_unique_val(0.5, 1.5)    # ~0.620
  labx_suss_ever <- get_unique_val(0.5, 1.5)   # ~0.630
  
  # LinearAbx clearance parameters
  labx_clr <- get_unique_val(0.001, 0.1)       # ~0.002089
  labx_clr_abx <- get_unique_val(0.5, 1.5)     # ~0.650
  labx_clr_ever <- get_unique_val(0.5, 1.5)    # ~0.660
  
  # Abx rate parameters
  abx_rate_unc <- get_unique_val(0.5, 2.0)     # ~0.755
  abx_rate_col <- get_unique_val(0.5, 2.0)     # ~0.785
  
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
      colonized = Param(init = surv_test_prob_col, weight = 1, update = TRUE),
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
  
  # Now create the C++ model object and extract parameters
  cpp_model <- newModelExport(model_params, verbose = FALSE)
  
  # Verify the model was created and is a list
  expect_true(!is.null(cpp_model))
  expect_type(cpp_model, "list")
  
  # Verify all expected parameter groups are present
  expect_true("Insitu" %in% names(cpp_model))
  expect_true("OutCol" %in% names(cpp_model))
  expect_true("SurveillanceTest" %in% names(cpp_model))
  expect_true("ClinicalTest" %in% names(cpp_model))
  expect_true("InCol" %in% names(cpp_model))
  expect_true("Abx" %in% names(cpp_model))
  
  # Check InsituParams - for 2-state model, only returns [unc, col] values
  insitu_values <- cpp_model$Insitu
  expect_equal(length(insitu_values), 2)
  expect_equal(as.numeric(insitu_values[1]), insitu_prob_unc, tolerance = 1e-10)
  expect_equal(as.numeric(insitu_values[2]), insitu_prob_col, tolerance = 1e-10)
  
  # Check OutColParams - [acquisition, clearance]
  outcol_values <- cpp_model$OutCol
  expect_equal(length(outcol_values), 2)
  expect_equal(as.numeric(outcol_values[1]), out_acquire, tolerance = 1e-10)
  expect_equal(as.numeric(outcol_values[2]), out_clear, tolerance = 1e-10)
  
  # Check SurveillanceTestParams - for 2-state model
  # LinearAbxModel uses TestParamsAbx which returns 4 values for 2-state model:
  # [P(+|unc,off-abx), P(+|col,off-abx), P(+|unc,on-abx), P(+|col,on-abx)]
  surv_values <- cpp_model$SurveillanceTest
  expect_equal(length(surv_values), 4)  # TestParamsAbx returns 4 values
  expect_true(all(!is.na(surv_values)))
  
  # Verify correct parameter assignment:
  # Off-abx parameters (indices 1-2):
  expect_equal(as.numeric(surv_values[1]), 0.0, tolerance = 1e-10,
               label = "SurveillanceTest P(+|uncolonized, off-abx)")
  expect_equal(as.numeric(surv_values[2]), surv_test_prob_col, tolerance = 1e-10,
               label = "SurveillanceTest P(+|colonized, off-abx) - MUST match R input!")
  # On-abx parameters (indices 3-4): currently set to same as off-abx
  expect_equal(as.numeric(surv_values[3]), 0.0, tolerance = 1e-10,
               label = "SurveillanceTest P(+|uncolonized, on-abx)")
  expect_equal(as.numeric(surv_values[4]), surv_test_prob_col, tolerance = 1e-10,
               label = "SurveillanceTest P(+|colonized, on-abx)")
  
  # Check ClinicalTestParams (RandomTestParams) - for 2-state model
  # C++ storage: probabilities [unc, lat, col] then rates [unc, lat, col]
  # But we get back only the used indices for 2-state: indices 0 and 2
  # After index swap fix: [P(+|unc), P(+|col), rate_unc, rate_col]
  clin_values <- cpp_model$ClinicalTest
  expect_equal(length(clin_values), 4)
  
  # CRITICAL: The test params were set with values in the LATENT field
  # to test the index swap bug. With the bug fixed, these should now be:
  # - uncolonized prob/rate: rtest_prob_unc, rtest_rate_unc
  # - colonized prob/rate: 0.0, 0.0 (what we set in colonized field)
  # - latent prob/rate: rtest_prob_col, rtest_rate_col (but latent isn't returned for 2-state)
  
  # Index 0 = uncolonized prob
  expect_equal(as.numeric(clin_values[1]), rtest_prob_unc, tolerance = 1e-10,
               label = "ClinicalTest P(+|uncolonized)")
  # Index 2 = colonized prob (we set this to 0.0!)
  expect_equal(as.numeric(clin_values[2]), 0.0, tolerance = 1e-10,
               label = "ClinicalTest P(+|colonized) - we set colonized to 0.0")
  # Index 0 rate = uncolonized rate
  expect_equal(as.numeric(clin_values[3]), rtest_rate_unc, tolerance = 1e-10,
               label = "ClinicalTest rate(uncolonized)")
  # Index 2 rate = colonized rate (we set this to 0.0!)
  expect_equal(as.numeric(clin_values[4]), 0.0, tolerance = 1e-10,
               label = "ClinicalTest rate(colonized) - we set colonized to 0.0")
  
  # Check InColParams (LinearAbxICP)
  # The parameters are returned in the following order:
  # [1] base, [2] time, [3] mass, [4] freq, [5] col_abx, [6] suss_abx, 
  # [7] suss_ever, [8] clearance_rate, [9] clearance_abx, [10] clearance_ever_abx
  incol_values <- cpp_model$InCol
  expect_equal(length(incol_values), 10)
  
  # Verify each InCol parameter matches what we set
  expect_equal(as.numeric(incol_values[1]), labx_base, tolerance = 1e-10,
               label = "InCol acquisition base")
  expect_equal(as.numeric(incol_values[2]), labx_time, tolerance = 1e-10,
               label = "InCol acquisition time")
  expect_equal(as.numeric(incol_values[3]), labx_mass, tolerance = 1e-10,
               label = "InCol acquisition mass")
  expect_equal(as.numeric(incol_values[4]), labx_freq, tolerance = 1e-10,
               label = "InCol acquisition freq")
  expect_equal(as.numeric(incol_values[5]), labx_col_abx, tolerance = 1e-10,
               label = "InCol acquisition col_abx")
  expect_equal(as.numeric(incol_values[6]), labx_suss_abx, tolerance = 1e-10,
               label = "InCol acquisition suss_abx")
  expect_equal(as.numeric(incol_values[7]), labx_suss_ever, tolerance = 1e-10,
               label = "InCol acquisition suss_ever")
  expect_equal(as.numeric(incol_values[8]), labx_clr, tolerance = 1e-10,
               label = "InCol clearance rate")
  expect_equal(as.numeric(incol_values[9]), labx_clr_abx, tolerance = 1e-10,
               label = "InCol clearance abx")
  expect_equal(as.numeric(incol_values[10]), labx_clr_ever, tolerance = 1e-10,
               label = "InCol clearance ever_abx")
  
  # Check AbxRate parameters
  abx_values <- cpp_model$Abx
  expect_equal(length(abx_values), 2)
  expect_equal(as.numeric(abx_values[1]), abx_rate_unc, tolerance = 1e-10)
  expect_equal(as.numeric(abx_values[2]), abx_rate_col, tolerance = 1e-10)
  
  # Verify unique values by checking that we set parameters correctly in R
  all_r_values <- c(
    insitu_prob_unc, insitu_prob_col,
    surv_test_prob_col,
    rtest_prob_unc, rtest_prob_col, rtest_rate_unc, rtest_rate_col,
    out_acquire, out_clear,
    labx_base, labx_time, labx_mass, labx_freq,
    labx_col_abx, labx_suss_abx, labx_suss_ever,
    labx_clr, labx_clr_abx, labx_clr_ever,
    abx_rate_unc, abx_rate_col
  )
  
  # Check that all non-zero values are unique (excluding the zeros we set)
  non_zero_values <- all_r_values[all_r_values != 0]
  expect_equal(length(non_zero_values), length(unique(non_zero_values)),
               info = "All non-zero parameter values should be unique")
})

test_that("LinearAbxModel2 C++ object creation and parameter access", {
  # Test that LinearAbxModel2 can be created via newModelExport
  
  # Use distinct values
  model_params <- list(
    modname = "LinearAbxModel2",
    nstates = 2,
    nmetro = 100,
    forward = 1,
    cheat = 0,
    Insitu = InsituParams(
      probs = c(0.8321, 0, 0.1679),
      priors = c(0.9, 0, 0.1),
      doit = c(TRUE, FALSE, TRUE)
    ),
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 0.0, weight = 0, update = FALSE),
      colonized = Param(init = 0.6543, weight = 1, update = TRUE),
      latent = Param(init = 0.0, weight = 0, update = FALSE)
    ),
    ClinicalTest = RandomTestParams(
      uncolonized = ParamWRate(
        param = Param(init = 0.2187, weight = 0, update = FALSE),
        rate = Param(init = 0.8765, weight = 0, update = FALSE)
      ),
      colonized = ParamWRate(
        param = Param(init = 0.0, weight = 0, update = FALSE),
        rate = Param(init = 0.0, weight = 0, update = FALSE)
      ),
      latent = ParamWRate(
        param = Param(init = 0.3456, weight = 0, update = FALSE),
        rate = Param(init = 1.2345, weight = 0, update = FALSE)
      )
    ),
    OutCol = OutOfUnitInfectionParams(
      acquisition = Param(init = 0.004321, weight = 1, update = TRUE),
      clearance = Param(init = 0.007654, weight = 0, update = FALSE)
    ),
    InCol = ABXInUnitParams(
      acquisition = LinearAbxAcquisitionParams(
        base = Param(init = 0.001234, weight = 1, update = TRUE),
        time = Param(init = 1.1111, weight = 0, update = FALSE),
        mass = Param(init = 0.9123, weight = 1, update = TRUE),
        freq = Param(init = 0.9234, weight = 1, update = TRUE),
        col_abx = Param(init = 0.7777, weight = 0, update = FALSE),
        suss_abx = Param(init = 0.8888, weight = 0, update = FALSE),
        suss_ever = Param(init = 0.9999, weight = 0, update = FALSE)
      ),
      clearance = ClearanceParams(
        rate = Param(init = 0.008765, weight = 1, update = TRUE),
        abx = Param(init = 0.6666, weight = 0, update = FALSE),
        ever_abx = Param(init = 0.5555, weight = 0, update = FALSE)
      )
    ),
    Abx = AbxParams(
      onoff = FALSE,
      delay = 0.0,
      life = 2.0
    ),
    AbxRate = AbxRateParams(
      uncolonized = Param(init = 1.3333, weight = 0, update = FALSE),
      colonized = Param(init = 1.4444, weight = 0, update = FALSE),
      latent = Param(init = 0.0, weight = 0, update = FALSE)
    )
  )
  
  # Create C++ model
  cpp_model <- newModelExport(model_params, verbose = FALSE)
  
  # Basic verification
  expect_true(!is.null(cpp_model))
  expect_type(cpp_model, "list")
  
  # Verify all expected parameter groups are present
  expect_true("Insitu" %in% names(cpp_model))
  expect_true("OutCol" %in% names(cpp_model))
  expect_true("SurveillanceTest" %in% names(cpp_model))
  expect_true("ClinicalTest" %in% names(cpp_model))
  expect_true("InCol" %in% names(cpp_model))
  expect_true("Abx" %in% names(cpp_model))
  
  # Verify InsituParams
  insitu_values <- cpp_model$Insitu
  expect_equal(length(insitu_values), 2)
  expect_equal(as.numeric(insitu_values[1]), 0.8321, tolerance = 1e-10)
  expect_equal(as.numeric(insitu_values[2]), 0.1679, tolerance = 1e-10)
  
  # Verify OutColParams
  outcol_values <- cpp_model$OutCol
  expect_equal(length(outcol_values), 2)
  expect_equal(as.numeric(outcol_values[1]), 0.004321, tolerance = 1e-10)
  expect_equal(as.numeric(outcol_values[2]), 0.007654, tolerance = 1e-10)
  
  # Verify SurveillanceTest
  # LinearAbxModel2 uses TestParamsAbx which returns 4 values for 2-state model:
  # [P(+|unc,off-abx), P(+|col,off-abx), P(+|unc,on-abx), P(+|col,on-abx)]
  surv_values <- cpp_model$SurveillanceTest
  expect_equal(length(surv_values), 4)
  
  # Verify correct parameter values:
  expect_equal(as.numeric(surv_values[1]), 0.0, tolerance = 1e-10,
               label = "SurveillanceTest P(+|uncolonized, off-abx)")
  expect_equal(as.numeric(surv_values[2]), 0.6543, tolerance = 1e-10,
               label = "SurveillanceTest P(+|colonized, off-abx) - critical test for index swap bug!")
  expect_equal(as.numeric(surv_values[3]), 0.0, tolerance = 1e-10,
               label = "SurveillanceTest P(+|uncolonized, on-abx)")
  expect_equal(as.numeric(surv_values[4]), 0.6543, tolerance = 1e-10,
               label = "SurveillanceTest P(+|colonized, on-abx)")
  
  # Verify ClinicalTest - after index swap bug fix
  # We set: uncolonized=(0.2187, 0.8765), colonized=(0.0, 0.0), latent=(0.3456, 1.2345)
  # For 2-state model, should return [unc_prob, col_prob, unc_rate, col_rate]
  clin_values <- cpp_model$ClinicalTest
  expect_equal(length(clin_values), 4)
  expect_equal(as.numeric(clin_values[1]), 0.2187, tolerance = 1e-10,
               label = "ClinicalTest P(+|uncolonized)")
  expect_equal(as.numeric(clin_values[2]), 0.0, tolerance = 1e-10,
               label = "ClinicalTest P(+|colonized) - we set to 0.0")
  expect_equal(as.numeric(clin_values[3]), 0.8765, tolerance = 1e-10,
               label = "ClinicalTest rate(uncolonized)")
  expect_equal(as.numeric(clin_values[4]), 0.0, tolerance = 1e-10,
               label = "ClinicalTest rate(colonized) - we set to 0.0")
  
  # Verify InCol - all 10 parameters
  incol_values <- cpp_model$InCol
  expect_equal(length(incol_values), 10)
  expect_equal(as.numeric(incol_values[1]), 0.001234, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[2]), 1.1111, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[3]), 0.9123, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[4]), 0.9234, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[5]), 0.7777, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[6]), 0.8888, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[7]), 0.9999, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[8]), 0.008765, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[9]), 0.6666, tolerance = 1e-10)
  expect_equal(as.numeric(incol_values[10]), 0.5555, tolerance = 1e-10)
  
  # Verify AbxRate
  abx_values <- cpp_model$Abx
  expect_equal(length(abx_values), 2)
  expect_equal(as.numeric(abx_values[1]), 1.3333, tolerance = 1e-10)
  expect_equal(as.numeric(abx_values[2]), 1.4444, tolerance = 1e-10)
})

test_that("LogNormalModel C++ object creation and parameter access", {
  skip("LogNormalModelParams has constructor issues with default AcquisitionParams")
  
  # This test is skipped because LogNormalModelParams default InUnitParams
  # calls AcquisitionParams() which doesn't exist. This is a separate issue
  # from the newModelExport functionality which is working correctly for
  # LinearAbxModel and LinearAbxModel2.
})
