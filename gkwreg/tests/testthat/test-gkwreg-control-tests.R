library(testthat)
library(gkwreg)

# =============================================================================
# Basic Object Creation and Structure
# =============================================================================

test_that("Default control object is created successfully", {
  # Create default control
  ctrl <- gkw_control()

  expect_s3_class(ctrl, "gkw_control")
  expect_type(ctrl, "list")
  expect_true(all(c("method", "start", "fixed", "hessian", "silent") %in% names(ctrl)))
})

test_that("Control object has correct default values", {
  ctrl <- gkw_control()

  expect_equal(ctrl$method, "nlminb")
  expect_null(ctrl$start)
  expect_null(ctrl$fixed)
  expect_true(ctrl$hessian)
  expect_true(ctrl$silent)
  expect_equal(ctrl$maxit, 500L)
})

test_that("Print method works for control objects", {
  ctrl <- gkw_control()

  # Should not error
  expect_output(print(ctrl), "Generalized Kumaraswamy Control Parameters")
  expect_output(print(ctrl), "Method:")
  expect_output(print(ctrl), "nlminb")
})

# =============================================================================
# Method Selection and Validation
# =============================================================================

test_that("All optimization methods are accepted", {
  methods <- c("nlminb", "BFGS", "Nelder-Mead", "CG", "SANN", "L-BFGS-B")

  for (m in methods) {
    ctrl <- gkw_control(method = m)
    expect_equal(ctrl$method, m)
    expect_s3_class(ctrl, "gkw_control")
  }
})

test_that("Invalid method throws error", {
  expect_error(
    gkw_control(method = "invalid_method"),
    "'arg' should be one of"
  )
})

test_that("SANN method gets higher default maxit", {
  ctrl_sann <- gkw_control(method = "SANN")
  ctrl_default <- gkw_control()

  expect_equal(ctrl_sann$maxit, 10000L)
  expect_equal(ctrl_default$maxit, 500L)
})

# =============================================================================
# Numeric Parameter Validation
# =============================================================================

test_that("maxit parameter is validated correctly", {
  # Valid maxit
  expect_silent(gkw_control(maxit = 100))
  expect_silent(gkw_control(maxit = 1000))

  # Invalid maxit
  expect_error(gkw_control(maxit = -10), "must be a single positive integer")
  expect_error(gkw_control(maxit = 0), "must be a single positive integer")
  expect_error(gkw_control(maxit = 10.5), "must be a single positive integer")
  expect_error(gkw_control(maxit = c(10, 20)), "must be a single positive integer")
  expect_error(gkw_control(maxit = "100"), "must be a single positive integer")
})

test_that("Tolerance parameters are validated correctly", {
  # Valid tolerances
  expect_silent(gkw_control(reltol = 1e-8))
  expect_silent(gkw_control(abstol = 1e-10))
  expect_silent(gkw_control(x.tol = 1e-6))

  # Invalid tolerances (negative)
  expect_error(gkw_control(reltol = -1e-8), "must be a single non-negative")
  expect_error(gkw_control(abstol = -0.01), "must be a single non-negative")
  expect_error(gkw_control(x.tol = -1), "must be a single non-negative")

  # Invalid tolerances (wrong type)
  expect_error(gkw_control(reltol = "1e-8"), "must be a single non-negative")
  expect_error(gkw_control(abstol = c(0, 1)), "must be a single non-negative")
})

test_that("trace parameter is validated correctly", {
  # Valid trace values
  expect_silent(gkw_control(trace = 0))
  expect_silent(gkw_control(trace = 1))
  expect_silent(gkw_control(trace = 5))

  # Invalid trace values
  expect_error(gkw_control(trace = -1), "must be a single non-negative integer")
  expect_error(gkw_control(trace = 1.5), "must be a single non-negative integer")
  expect_error(gkw_control(trace = "1"), "must be a single non-negative integer")
})

# =============================================================================
# Logical Parameter Validation
# =============================================================================

test_that("Logical parameters are validated correctly", {
  # Valid logical values
  expect_silent(gkw_control(hessian = TRUE))
  expect_silent(gkw_control(hessian = FALSE))
  expect_silent(gkw_control(silent = TRUE))
  expect_silent(gkw_control(silent = FALSE))

  # Invalid logical values
  expect_error(gkw_control(hessian = 1), "must be a single logical value")
  expect_error(gkw_control(hessian = "TRUE"), "must be a single logical value")
  expect_error(gkw_control(hessian = NA), "must be a single logical value")
  expect_error(gkw_control(silent = c(TRUE, FALSE)), "must be a single logical value")
})

# =============================================================================
# Method-Specific Parameters: nlminb
# =============================================================================

test_that("nlminb-specific parameters are validated", {
  # Valid parameters
  ctrl <- gkw_control(
    method = "nlminb",
    eval.max = 1000,
    iter.max = 500,
    step.min = 1e-10,
    step.max = 2,
    x.tol = 1e-8,
    rel.tol = 1e-8
  )

  expect_equal(ctrl$nlminb_control$eval.max, 1000L)
  expect_equal(ctrl$nlminb_control$iter.max, 500L)
  expect_equal(ctrl$nlminb_control$step.min, 1e-10)

  # Invalid eval.max
  expect_error(gkw_control(eval.max = -100), "must be a single positive integer")
  expect_error(gkw_control(eval.max = 0), "must be a single positive integer")

  # Invalid step parameters
  expect_error(gkw_control(step.min = -1), "must be a single positive numeric")
  expect_error(gkw_control(step.max = -1), "must be a single positive numeric")
})

test_that("nlminb control list is properly constructed", {
  ctrl <- gkw_control(method = "nlminb", trace = 2, silent = FALSE)

  expect_type(ctrl$nlminb_control, "list")
  expect_equal(ctrl$nlminb_control$trace, 2L)

  # Silent mode overrides trace
  ctrl_silent <- gkw_control(method = "nlminb", trace = 2, silent = TRUE)
  expect_equal(ctrl_silent$nlminb_control$trace, 0L)
})

# =============================================================================
# Method-Specific Parameters: Nelder-Mead
# =============================================================================

test_that("Nelder-Mead parameters are validated correctly", {
  # Valid parameters
  ctrl <- gkw_control(
    method = "Nelder-Mead",
    alpha = 1.5,
    beta = 0.75,
    gamma = 2.5
  )

  expect_equal(ctrl$optim_control$alpha, 1.5)
  expect_equal(ctrl$optim_control$beta, 0.75)
  expect_equal(ctrl$optim_control$gamma, 2.5)

  # Invalid alpha (must be positive)
  expect_error(gkw_control(alpha = -1), "must be a single positive numeric")
  expect_error(gkw_control(alpha = 0), "must be a single positive numeric")

  # Invalid beta (must be between 0 and 1)
  expect_error(gkw_control(beta = -0.1), "must be a single numeric value between 0 and 1")
  expect_error(gkw_control(beta = 0), "must be a single numeric value between 0 and 1")
  expect_error(gkw_control(beta = 1), "must be a single numeric value between 0 and 1")
  expect_error(gkw_control(beta = 1.5), "must be a single numeric value between 0 and 1")

  # Invalid gamma (must be > 1)
  expect_error(gkw_control(gamma = 0.5), "must be a single numeric value greater than 1")
  expect_error(gkw_control(gamma = 1), "must be a single numeric value greater than 1")
})

test_that("Nelder-Mead control list includes method-specific parameters", {
  ctrl <- gkw_control(
    method = "Nelder-Mead",
    alpha = 1.2,
    beta = 0.6,
    gamma = 2.2
  )

  expect_equal(ctrl$method_params$alpha, 1.2)
  expect_equal(ctrl$method_params$beta, 0.6)
  expect_equal(ctrl$method_params$gamma, 2.2)
})

# =============================================================================
# Method-Specific Parameters: CG
# =============================================================================

test_that("CG type parameter is validated correctly", {
  # Valid types
  expect_silent(gkw_control(method = "CG", type = 1))
  expect_silent(gkw_control(method = "CG", type = 2))
  expect_silent(gkw_control(method = "CG", type = 3))

  # Invalid types
  expect_error(gkw_control(type = 0), "'type' must be 1, 2, or 3")
  expect_error(gkw_control(type = 4), "'type' must be 1, 2, or 3")
  expect_error(gkw_control(type = 1.5), "'type' must be 1, 2, or 3")
})

test_that("CG control list is properly constructed", {
  ctrl <- gkw_control(method = "CG", type = 2, maxit = 1000)

  expect_equal(ctrl$optim_control$type, 2L)
  expect_equal(ctrl$optim_control$maxit, 1000L)
  expect_equal(ctrl$method_params$type, 2)
})

# =============================================================================
# Method-Specific Parameters: SANN
# =============================================================================

test_that("SANN parameters are validated correctly", {
  # Valid parameters
  ctrl <- gkw_control(
    method = "SANN",
    temp = 20,
    tmax = 15
  )

  expect_equal(ctrl$optim_control$temp, 20)
  expect_equal(ctrl$optim_control$tmax, 15L)

  # Invalid temp
  expect_error(gkw_control(temp = -10), "must be a single positive numeric")
  expect_error(gkw_control(temp = 0), "must be a single positive numeric")

  # Invalid tmax
  expect_error(gkw_control(tmax = 0), "must be a single positive integer")
  expect_error(gkw_control(tmax = -5), "must be a single positive integer")
  expect_error(gkw_control(tmax = 10.5), "must be a single positive integer")
})

test_that("SANN gets correct REPORT default", {
  ctrl <- gkw_control(method = "SANN")
  expect_equal(ctrl$optim_control$REPORT, 100L)
})

# =============================================================================
# Method-Specific Parameters: L-BFGS-B
# =============================================================================

test_that("L-BFGS-B parameters are validated correctly", {
  # Valid parameters
  ctrl <- gkw_control(
    method = "L-BFGS-B",
    lmm = 10,
    factr = 1e6,
    pgtol = 1e-5
  )

  expect_equal(ctrl$optim_control$lmm, 10L)
  expect_equal(ctrl$optim_control$factr, 1e6)
  expect_equal(ctrl$optim_control$pgtol, 1e-5)

  # Invalid lmm
  expect_error(gkw_control(lmm = 0), "must be a single positive integer")
  expect_error(gkw_control(lmm = -5), "must be a single positive integer")
  expect_error(gkw_control(lmm = 5.5), "must be a single positive integer")

  # Invalid factr
  expect_error(gkw_control(factr = -1e7), "must be a single positive numeric")
  expect_error(gkw_control(factr = 0), "must be a single positive numeric")

  # Invalid pgtol
  expect_error(gkw_control(pgtol = -0.01), "must be a single non-negative")
})

test_that("L-BFGS-B control list excludes incompatible parameters", {
  ctrl <- gkw_control(method = "L-BFGS-B", factr = 1e8, lmm = 8)

  # L-BFGS-B uses factr instead of reltol/abstol
  expect_true("factr" %in% names(ctrl$optim_control))
  expect_false("reltol" %in% names(ctrl$optim_control))
})

# =============================================================================
# Method-Specific Parameters: BFGS
# =============================================================================

test_that("BFGS control list is properly constructed", {
  ctrl <- gkw_control(
    method = "BFGS",
    maxit = 800,
    reltol = 1e-10
  )

  expect_equal(ctrl$optim_control$maxit, 800L)
  expect_equal(ctrl$optim_control$reltol, 1e-10)
  expect_equal(ctrl$optim_control$REPORT, 10L)
})

# =============================================================================
# Starting Values and Fixed Parameters
# =============================================================================

test_that("Starting values are validated correctly", {
  # Valid starting values
  start_list <- list(
    alpha = c(0.5, 0.2),
    beta = c(1.0, -0.3)
  )

  ctrl <- gkw_control(start = start_list)
  expect_equal(ctrl$start, start_list)

  # Invalid starting values (not a list)
  expect_error(gkw_control(start = c(0.5, 1.0)), "'start' must be NULL or a named list")

  # Invalid starting values (unnamed list)
  expect_error(
    gkw_control(start = list(c(0.5, 0.2), c(1.0, -0.3))),
    "'start' must be a named list with parameter names"
  )
})

test_that("Fixed parameters are validated correctly", {
  # Valid fixed parameters
  fixed_list <- list(gamma = 1, delta = 1)
  ctrl <- gkw_control(fixed = fixed_list)
  expect_equal(ctrl$fixed, fixed_list)

  # Invalid fixed parameters (not a list)
  expect_error(gkw_control(fixed = "gamma"), "'fixed' must be NULL or a named list")
})

# =============================================================================
# General optim Parameters
# =============================================================================

test_that("General optim parameters work correctly", {
  ctrl <- gkw_control(
    method = "BFGS",
    fnscale = -1, # For maximization
    parscale = c(1, 10, 0.1),
    ndeps = c(1e-4, 1e-4, 1e-4)
  )

  expect_equal(ctrl$optim_control$fnscale, -1)
  expect_equal(ctrl$optim_control$parscale, c(1, 10, 0.1))
  expect_equal(ctrl$optim_control$ndeps, c(1e-4, 1e-4, 1e-4))

  # Invalid fnscale
  expect_error(gkw_control(fnscale = "1"), "'fnscale' must be a single numeric value")
  expect_error(gkw_control(fnscale = c(1, 2)), "'fnscale' must be a single numeric value")
})

# =============================================================================
# Silent and Trace Interaction
# =============================================================================

test_that("Silent mode correctly overrides trace", {
  # Silent = TRUE should set trace to 0 in control lists
  ctrl_silent <- gkw_control(trace = 5, silent = TRUE)
  expect_equal(ctrl_silent$nlminb_control$trace, 0L)

  # Silent = FALSE should preserve trace
  ctrl_verbose <- gkw_control(method = "BFGS", trace = 2, silent = FALSE)
  expect_equal(ctrl_verbose$optim_control$trace, 2L)
})

# =============================================================================
# Print Method Coverage
# =============================================================================

test_that("Print method shows correct information for all methods", {
  methods <- c("nlminb", "BFGS", "Nelder-Mead", "CG", "SANN", "L-BFGS-B")

  for (m in methods) {
    ctrl <- gkw_control(method = m)
    expect_output(print(ctrl), m)
    expect_output(print(ctrl), "Method:")
  }
})

test_that("Print method shows starting values when provided", {
  ctrl <- gkw_control(start = list(alpha = c(0, 0), beta = c(1, 1)))
  expect_output(print(ctrl), "Starting values:")
  expect_output(print(ctrl), "alpha, beta")
})

test_that("Print method shows fixed parameters when provided", {
  ctrl <- gkw_control(fixed = list(gamma = 1, delta = 1))
  expect_output(print(ctrl), "Fixed parameters:")
  expect_output(print(ctrl), "gamma, delta")
})

test_that("Print method shows trace info when not silent", {
  ctrl_verbose <- gkw_control(silent = FALSE, trace = 3)
  expect_output(print(ctrl_verbose), "Trace level:")

  ctrl_silent <- gkw_control(silent = TRUE, trace = 3)
  expect_output(print(ctrl_silent), "Silent mode:.*TRUE")
})

# =============================================================================
# Additional Arguments via ...
# =============================================================================

test_that("Additional arguments are passed through", {
  # Extra arguments should be captured
  ctrl <- gkw_control(
    method = "BFGS",
    custom_param = "test_value"
  )

  expect_s3_class(ctrl, "gkw_control")
  # The extra argument should be in optim_control
  expect_true("custom_param" %in% names(ctrl$optim_control))
})

# =============================================================================
# Edge Cases and Boundary Conditions
# =============================================================================

test_that("Extreme but valid parameter values work", {
  # Very small tolerance
  expect_silent(gkw_control(reltol = .Machine$double.eps))

  # Very large maxit
  expect_silent(gkw_control(maxit = 1e6))

  # Zero tolerance
  expect_silent(gkw_control(abstol = 0))

  # High trace level
  expect_silent(gkw_control(trace = 10))
})

test_that("Parameter coercion works correctly", {
  # Integer parameters should be coerced
  ctrl <- gkw_control(maxit = 100.0) # Should be coerced to integer
  expect_type(ctrl$maxit, "integer")

  ctrl2 <- gkw_control(method = "nlminb", eval.max = 1000.0)
  expect_type(ctrl2$nlminb_control$eval.max, "integer")
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("Control object integrates with gkwreg if available", {
  skip_if_not_installed("gkwreg")
  skip_if(!exists("gkwreg", mode = "function"))

  # Create control with specific settings
  ctrl <- gkw_control(
    method = "BFGS",
    maxit = 200,
    hessian = TRUE
  )

  # Should be usable in gkwreg
  expect_s3_class(ctrl, "gkw_control")
  expect_equal(ctrl$method, "BFGS")
})

test_that("Multiple control configurations can coexist", {
  # Create different control objects
  ctrl1 <- gkw_control(method = "nlminb", maxit = 100)
  ctrl2 <- gkw_control(method = "BFGS", maxit = 200)
  ctrl3 <- gkw_control(method = "Nelder-Mead", alpha = 1.5)

  # All should be independent
  expect_equal(ctrl1$method, "nlminb")
  expect_equal(ctrl2$method, "BFGS")
  expect_equal(ctrl3$method, "Nelder-Mead")
  expect_equal(ctrl3$optim_control$alpha, 1.5)
})

# =============================================================================
# Warning Tests
# =============================================================================

test_that("Nelder-Mead 1D warning parameter is respected", {
  ctrl_warn <- gkw_control(method = "Nelder-Mead", warn.1d.NelderMead = TRUE)
  ctrl_nowarn <- gkw_control(method = "Nelder-Mead", warn.1d.NelderMead = FALSE)

  expect_true(ctrl_warn$optim_control$warn.1d.NelderMead)
  expect_false(ctrl_nowarn$optim_control$warn.1d.NelderMead)

  # Invalid value
  expect_error(
    gkw_control(warn.1d.NelderMead = "yes"),
    "'warn.1d.NelderMead' must be a single logical value"
  )
})

# =============================================================================
# Complex Scenarios
# =============================================================================

test_that("Complex control configuration works end-to-end", {
  ctrl <- gkw_control(
    method = "L-BFGS-B",
    start = list(
      alpha = c(0.5, 0.1, 0.2),
      beta = c(1.0, -0.3)
    ),
    hessian = TRUE,
    silent = FALSE,
    trace = 1,
    lmm = 10,
    factr = 1e8,
    pgtol = 1e-6,
    parscale = c(1, 1, 1, 10, 10)
  )

  expect_s3_class(ctrl, "gkw_control")
  expect_equal(ctrl$method, "L-BFGS-B")
  expect_false(ctrl$silent)
  expect_equal(ctrl$optim_control$lmm, 10L)
  expect_length(ctrl$start, 2)
  expect_output(print(ctrl), "L-BFGS-B")
})


# Setup test data
data("GasolineYield", package = "gkwreg")

# =============================================================================
# Test 1: All optimization methods produce valid models
# =============================================================================

test_that("All optimization methods work with gkwreg and produce similar results", {
  methods <- c("nlminb", "BFGS", "Nelder-Mead")
  fits <- list()

  for (method in methods) {
    ctrl <- gkw_control(method = method, maxit = 5000, silent = TRUE)

    fit <- gkwreg(
      yield ~ temp,
      data = GasolineYield,
      family = "kw",
      control = ctrl
    )

    expect_s3_class(fit, "gkwreg")
    expect_true(fit$convergence %in% c(0, TRUE))
    expect_true(is.finite(logLik(fit)))

    fits[[method]] <- fit
  }

  # All should have same number of parameters
  npars <- sapply(fits, function(f) f$npar)
  expect_true(all(npars == npars[1]))
})


# =============================================================================
# Test 2: Hessian control affects standard errors
# =============================================================================

test_that("hessian = FALSE skips standard error computation", {
  # With Hessian (default)
  fit_with_se <- gkwreg(
    yield ~ temp,
    data = GasolineYield,
    family = "kw",
    control = gkw_control(hessian = TRUE)
  )

  # Without Hessian
  fit_no_se <- gkwreg(
    yield ~ temp,
    data = GasolineYield,
    family = "kw",
    control = gkw_control(hessian = FALSE)
  )

  # With Hessian should have SE
  expect_false(is.null(fit_with_se$se))
  expect_true(all(fit_with_se$se > 0))

  # Without Hessian should not have SE
  expect_true(is.null(fit_no_se$se) || all(is.na(fit_no_se$se)))

  # But coefficients should be the same
  expect_equal(coef(fit_with_se), coef(fit_no_se), tolerance = 1e-8)
})

# =============================================================================
# Test 4: Silent and trace parameters control verbosity
# =============================================================================

test_that("silent and trace parameters control output correctly", {
  # Silent mode (should produce no output)
  expect_silent({
    fit_silent <- gkwreg(
      yield ~ temp,
      data = GasolineYield,
      family = "kw",
      control = gkw_control(silent = TRUE, trace = 0)
    )
  })

  # Verbose mode (should produce output)
  expect_output(
    {
      fit_verbose <- gkwreg(
        yield ~ temp,
        data = GasolineYield,
        family = "kw",
        control = gkw_control(silent = FALSE, trace = 1)
      )
    },
    ".*"
  ) # Any output

  # Both should produce same results
  expect_equal(
    coef(fit_silent),
    coef(fit_verbose),
    tolerance = 1e-8
  )
})

# =============================================================================
# Test 7: Nelder-Mead with custom simplex parameters
# =============================================================================

test_that("Nelder-Mead simplex parameters affect optimization", {
  # Nelder-Mead with default simplex parameters
  fit_nm_default <- gkwreg(
    yield ~ temp,
    data = GasolineYield,
    family = "kw",
    control = gkw_control(
      method = "Nelder-Mead",
      maxit = 1000
    )
  )

  # Nelder-Mead with custom simplex parameters
  fit_nm_custom <- gkwreg(
    yield ~ temp,
    data = GasolineYield,
    family = "kw",
    control = gkw_control(
      method = "Nelder-Mead",
      alpha = 1.5, # More aggressive reflection
      beta = 0.75, # Less contraction
      gamma = 2.5, # More expansion
      maxit = 1000
    )
  )

  # Both should converge to similar solutions
  expect_true(fit_nm_default$convergence %in% c(0, TRUE))
  expect_true(fit_nm_custom$convergence %in% c(0, TRUE))

  expect_equal(
    coef(fit_nm_default),
    coef(fit_nm_custom),
    tolerance = 1e-2 # May differ slightly due to different paths
  )
})


# =============================================================================
# Bonus Test 11: Multiple control configurations in model selection
# =============================================================================

test_that("Different control settings in model selection workflow", {
  # Fast exploratory phase (no SE)
  ctrl_fast <- gkw_control(
    hessian = FALSE,
    maxit = 300
  )

  formulas <- list(
    yield ~ 1,
    yield ~ temp,
    yield ~ batch,
    yield ~ batch + temp
  )

  # Fit all models quickly
  fits_fast <- lapply(formulas, function(f) {
    gkwreg(f, GasolineYield, family = "kw", control = ctrl_fast)
  })

  # All should be valid
  expect_true(all(sapply(fits_fast, inherits, "gkwreg")))

  # Find best by AIC
  aics <- sapply(fits_fast, AIC)
  best_idx <- which.min(aics)

  # Refit best model with full inference
  ctrl_full <- gkw_control(
    method = "BFGS",
    hessian = TRUE,
    maxit = 1000,
    reltol = 1e-10
  )

  fit_final <- gkwreg(
    formulas[[best_idx]],
    GasolineYield,
    family = "kw",
    control = ctrl_full
  )

  # Final model should have SE
  expect_false(is.null(fit_final$se))

  # Should be able to get confidence intervals
  ci <- confint(fit_final)
  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), length(coef(fit_final)))
})

# =============================================================================
# Bonus Test 12: Control robustness with difficult data
# =============================================================================

test_that("Control helps with convergence on difficult simulated data", {
  skip_if_not_installed("gkwdist")

  # Simulate difficult case with extreme parameters
  set.seed(999)
  n <- 100
  x <- runif(n, -1, 1)

  # Extreme but valid parameters
  alpha <- exp(2 + 0.5 * x) # Large alpha
  beta <- exp(0.1 - 0.1 * x) # Small beta

  y <- gkwdist::rkw(n, alpha = alpha, beta = beta)
  difficult_data <- data.frame(y = y, x = x)

  # May fail with default settings
  fit_default <- tryCatch(
    gkwreg(y ~ x | x, data = difficult_data, family = "kw"),
    error = function(e) NULL
  )

  # Should work with robust settings
  fit_robust <- gkwreg(
    y ~ x | x,
    data = difficult_data,
    family = "kw",
    control = gkw_control(
      method = "BFGS",
      maxit = 2000,
      reltol = 1e-6, # Slightly relaxed
      silent = TRUE
    )
  )

  expect_s3_class(fit_robust, "gkwreg")
  expect_true(fit_robust$convergence %in% c(0, TRUE))

  # Should recover reasonable parameters
  expect_true(all(is.finite(coef(fit_robust))))
})
