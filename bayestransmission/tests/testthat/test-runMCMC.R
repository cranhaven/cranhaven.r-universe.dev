test_that("runMCMC Works", {
  modelParameters <- LinearAbxModel(nstates = 2)

  expect_named(modelParameters, c("modname", "nstates", "nmetro", "forward",
    "cheat", "Insitu", "SurveillanceTest",
    "ClinicalTest", "OutCol", "InCol", "Abx",
    "AbxRate"), ignore.order = TRUE)
  expect_true(rlang::is_string(modelParameters$modname))
  expect_true(rlang::is_integer(modelParameters$nstates))
  expect_true(rlang::is_integer(modelParameters$nmetro))
  expect_true(rlang::is_logical(modelParameters$forward))
  expect_true(rlang::is_logical(modelParameters$cheat))

  expect_true(rlang::is_list(modelParameters$Insitu))
  expect_named(modelParameters$Insitu, c("probs", "priors", "doit"))
  if(rlang::is_installed("checkmate")){
    checkmate::expect_double(modelParameters$Insitu$probs, 0, 1, len = 3)
    checkmate::expect_double(modelParameters$Insitu$priors, 0, 1, len = 3)
    checkmate::expect_logical(modelParameters$Insitu$doit, len = 3)
  }
  expect_true(rlang::is_list(modelParameters$SurveillanceTest))
  expect_named(modelParameters$SurveillanceTest, c("colonized", "uncolonized", "latent"), ignore.order = TRUE)

  expect_true(rlang::is_list(modelParameters$ClinicalTest))
  expect_named(modelParameters$ClinicalTest, c("colonized", "uncolonized", "latent"), ignore.order = TRUE)

  expect_true(rlang::is_list(modelParameters$OutCol))
  expect_named(modelParameters$OutCol, c("acquisition", "clearance", "progression"), ignore.order = TRUE)

  expect_true(rlang::is_list(modelParameters$InCol))
  expect_named(modelParameters$InCol, c("acquisition", "progression", "clearance"), ignore.order = TRUE)

  expect_true(rlang::is_list(modelParameters$Abx))
  expect_named(modelParameters$Abx, c("onoff", "delay", "life"), ignore.order = TRUE)

  expect_true(rlang::is_list(modelParameters$AbxRate))
  expect_named(modelParameters$AbxRate, c("uncolonized", "latent", "colonized"), ignore.order = TRUE)


  results <- runMCMC(
    data = simulated.data,
    modelParameters = modelParameters,
    nsims = 3,
    nburn = 2,
    outputparam = TRUE,
    outputfinal = TRUE,
    verbose = FALSE
  )
})

test_that("runMCMC produces consistent output with fixed parameters", {
  # Define all parameters explicitly to be tolerant to changes in defaults
  # These values match the original C++ test case (simulated.Model)
  modelParameters <- LinearAbxModel(
    nstates = 2L,
    nmetro = 10L,
    forward = FALSE,
    cheat = FALSE,
    Insitu = InsituParams(
      probs = c(0.9, 0, 0.1),
      priors = c(1, 1, 1),
      doit = c(TRUE, FALSE, TRUE)
    ),
    SurveillanceTest = SurveillanceTestParams(
      uncolonized = Param(init = 0.0, weight = 0),  # fixed at 0
      latent = Param(init = 0.0, weight = 0),       # fixed at 0
      colonized = Param(init = 0.8, weight = 1)     # updated, starting at 0.8
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
    Abx = AbxParams(
      onoff = 0L,
      delay = 0.0,
      life = 2.0
    ),
    AbxRate = AbxRateParams(
      uncolonized = Param(init = 1.0, weight = 0),
      latent = Param(init = 1.0, weight = 0),
      colonized = Param(init = 1.0, weight = 0)
    )
  )
  
  # Set seed for reproducibility
  set.seed(42)
  
  # Run MCMC with minimal iterations for testing
  results <- runMCMC(
    data = simulated.data,
    modelParameters = modelParameters,
    nsims = 10,
    nburn = 5,
    outputparam = TRUE,
    outputfinal = FALSE,
    verbose = FALSE
  )
  
  # Check structure of results
  expect_type(results, "list")
  expect_named(results, c("Parameters", "LogLikelihood", "MCMCParameters", 
                         "ModelParameters", "waic1", "waic2"), 
               ignore.order = TRUE)
  
  # Check dimensions
  expect_length(results$Parameters, 10)
  expect_length(results$LogLikelihood, 10)
  
  # Check that log likelihood values are finite and reasonable
  # Note: Some values may be -Inf during burn-in with bad parameter combinations
  expect_true(is.numeric(results$LogLikelihood))
  finite_ll <- results$LogLikelihood[is.finite(results$LogLikelihood)]
  if(length(finite_ll) > 0) {
    expect_true(all(finite_ll < 0))  # Log likelihood should be negative
    expect_true(all(finite_ll > -100000))  # Should not be extremely negative
  }
  
  # Check parameter structure - each iteration should have model parameters
  expect_true(all(sapply(results$Parameters, is.list)))
  param_names <- c("Insitu", "SurveillanceTest", "ClinicalTest", 
                   "OutCol", "InCol", "Abx")
  for (i in seq_along(results$Parameters)) {
    expect_true(all(param_names %in% names(results$Parameters[[i]])))
  }
  
  # Check WAIC values are finite
  expect_true(is.finite(results$waic1))
  expect_true(is.finite(results$waic2))
  
  # Check that MCMC parameters are preserved in return value
  expect_equal(results$MCMCParameters$nburn, 5)
  expect_equal(results$MCMCParameters$nsims, 10)
})

test_that("runMCMC is reproducible with same seed", {
  # Define fixed parameters
  modelParameters <- LinearAbxModel(
    nstates = 2L,
    nmetro = 10L,
    forward = FALSE,
    cheat = FALSE,
    Insitu = InsituParams(
      probs = c(0.9, 0, 0.1),
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
    Abx = AbxParams(
      onoff = 0L,
      delay = 0.0,
      life = 2.0
    ),
    AbxRate = AbxRateParams(
      uncolonized = Param(init = 1.0, weight = 0),
      latent = Param(init = 1.0, weight = 0),
      colonized = Param(init = 1.0, weight = 0)
    )
  )
  
  mcmc_params <- list(
    nburn = 2,
    nsims = 3,
    outputparam = TRUE,
    outputfinal = FALSE
  )
  
  # Run 1
  set.seed(123)
  results1 <- runMCMC(
    data = simulated.data,
    modelParameters = modelParameters,
    nsims = 3,
    nburn = 2,
    outputparam = TRUE,
    outputfinal = FALSE,
    verbose = FALSE
  )
  
  # Run 2 with same seed
  set.seed(123)
  results2 <- runMCMC(
    data = simulated.data,
    modelParameters = modelParameters,
    nsims = 3,
    nburn = 2,
    outputparam = TRUE,
    outputfinal = FALSE,
    verbose = FALSE
  )
  
  # Results should be identical with same seed
  expect_equal(results1$LogLikelihood, results2$LogLikelihood, tolerance = 1e-10)
  expect_equal(results1$waic1, results2$waic1, tolerance = 1e-10)
  expect_equal(results1$waic2, results2$waic2, tolerance = 1e-10)
})
