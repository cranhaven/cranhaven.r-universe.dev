# tests/testthat/test-plot-gkwreg.R

# Test Suite for plot.gkwreg Method
# Author: Lopes, J. E.
# Description: Tests for the diagnostic plotting function covering plot selection,
#              residual types, customization options, and output validation

library(testthat)
library(gkwreg)
library(gkwdist)

# Setup: Generate test data and fit model
setup_plot_data <- function() {
  set.seed(36925)
  n <- 1000
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)

  # Generate Kumaraswamy response
  alpha <- exp(0.6 + 0.3 * x1)
  beta <- exp(1.1 - 0.2 * x2)
  y <- rkw(n, alpha = alpha, beta = beta)

  # Ensure strictly in (0, 1)
  y <- pmax(pmin(y, 1 - 1e-10), 1e-10)

  data <- data.frame(y = y, x1 = x1, x2 = x2)

  # Fit model
  fit <- gkwreg(y ~ x1 | x2, data = data, family = "kw")

  list(data = data, fit = fit, y = y)
}

setup <- setup_plot_data()

# =============================================================================
# PLOT.GKWREG TESTS
# =============================================================================

test_that("Test 1: Basic plot execution works without errors", {
  # Test that plot.gkwreg runs successfully with defaults
  # setup <- setup_plot_data()

  # Capture plot output to avoid displaying during tests
  pdf(NULL) # Suppress plot display
  on.exit(dev.off())

  # Should execute without error
  expect_no_error(
    plot(setup$fit, ask = FALSE)
  )

  # Should return object invisibly
  result <- plot(setup$fit, ask = FALSE)
  expect_s3_class(result, "gkwreg")
  expect_identical(result, setup$fit)
})

test_that("Test 2: which argument selects specific diagnostic plots", {
  # Test that which parameter controls plot selection
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Single plot
  expect_no_error(plot(setup$fit, which = 1, ask = FALSE))

  # Subset of plots
  expect_no_error(plot(setup$fit, which = c(2, 4, 6), ask = FALSE))

  # All plots
  expect_no_error(plot(setup$fit, which = 1:6, ask = FALSE))

  # Out of order
  expect_no_error(plot(setup$fit, which = c(5, 2, 1), ask = FALSE))
})

test_that("Test 3: Different residual types produce valid plots", {
  # Test that all residual types work correctly
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Quantile residuals
  expect_no_error(
    plot(setup$fit, which = 4, type = "quantile", ask = FALSE)
  )

  # Pearson residuals
  expect_no_error(
    plot(setup$fit, which = 4, type = "pearson", ask = FALSE)
  )

  # Deviance residuals
  expect_no_error(
    plot(setup$fit, which = 4, type = "deviance", ask = FALSE)
  )
})

test_that("Test 4: Caption customization with named list works", {
  # Test new named list interface for partial caption customization
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Named list customization
  custom_captions <- list(
    "1" = "Custom Title for Plot 1",
    "3" = "Custom Title for Plot 3"
  )

  expect_no_error(
    plot(setup$fit,
      which = c(1, 3, 5),
      caption = custom_captions, ask = FALSE
    )
  )

  # Vector customization (backward compatibility)
  caption_vector <- c(
    "Title 1", "Title 2", "Title 3",
    "Title 4", "Title 5", "Title 6"
  )

  expect_no_error(
    plot(setup$fit,
      which = 1:6,
      caption = caption_vector, ask = FALSE
    )
  )
})

test_that("Test 5: ggplot2 option produces different output type", {
  # Test that use_ggplot creates ggplot objects
  # setup <- setup_plot_data()

  # Skip if ggplot2 not available
  skip_if_not_installed("ggplot2")

  pdf(NULL)
  on.exit(dev.off())

  # Should work with ggplot2
  expect_no_error(
    result <- plot(setup$fit,
      which = 1,
      use_ggplot = TRUE, ask = FALSE
    )
  )

  # Test with multiple plots
  expect_no_error(
    plot(setup$fit,
      which = c(1, 2, 4),
      use_ggplot = TRUE, ask = FALSE
    )
  )
})

test_that("Test 6: save_diagnostics returns proper data structure", {
  # Test that diagnostic data can be extracted
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Get diagnostics
  diag <- plot(setup$fit,
    which = 1:6,
    save_diagnostics = TRUE, ask = FALSE
  )

  expect_type(diag, "list")

  # Check essential components
  expect_true("data" %in% names(diag))
  expect_true("model_info" %in% names(diag))

  # Data should be a data frame
  expect_s3_class(diag$data, "data.frame")
  expect_equal(nrow(diag$data), nobs(setup$fit))

  # Should contain key diagnostic measures
  expect_true("resid" %in% names(diag$data))
  expect_true("fitted" %in% names(diag$data))
  expect_true("cook_dist" %in% names(diag$data))
  expect_true("leverage" %in% names(diag$data))

  # Model info should contain metadata
  expect_true("n" %in% names(diag$model_info))
  expect_true("p" %in% names(diag$model_info))
  expect_equal(diag$model_info$n, nobs(setup$fit))
})

test_that("Test 7: Half-normal plot parameters control simulation", {
  # Test nsim and level parameters for half-normal plot
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Different simulation sizes
  expect_no_error(
    plot(setup$fit, which = 5, nsim = 50, ask = FALSE)
  )

  expect_no_error(
    plot(setup$fit, which = 5, nsim = 200, ask = FALSE)
  )

  # Different confidence levels
  expect_no_error(
    plot(setup$fit, which = 5, level = 0.80, ask = FALSE)
  )

  expect_no_error(
    plot(setup$fit, which = 5, level = 0.95, ask = FALSE)
  )

  # Extract diagnostics to verify envelope data
  diag_90 <- plot(setup$fit,
    which = 5, level = 0.90,
    save_diagnostics = TRUE, ask = FALSE
  )
  diag_95 <- plot(setup$fit,
    which = 5, level = 0.95,
    save_diagnostics = TRUE, ask = FALSE
  )

  # Both should have half_normal data
  expect_true("half_normal" %in% names(diag_90))
  expect_true("half_normal" %in% names(diag_95))

  # 95% envelope should be wider than 90%
  if (!is.null(diag_90$half_normal) && !is.null(diag_95$half_normal)) {
    range_90 <- max(diag_90$half_normal$upper) - min(diag_90$half_normal$lower)
    range_95 <- max(diag_95$half_normal$upper) - min(diag_95$half_normal$lower)
    expect_true(range_95 >= range_90)
  }
})

test_that("Test 8: sample_size reduces data for large datasets", {
  # Test that sample_size parameter works for efficiency
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Use sample size smaller than dataset
  sample_n <- 50

  diag_sampled <- plot(setup$fit,
    which = 1:6,
    sample_size = sample_n,
    save_diagnostics = TRUE, ask = FALSE
  )

  # Should use sampled data
  expect_equal(nrow(diag_sampled$data), sample_n)
  expect_true(diag_sampled$model_info$n <= nobs(setup$fit))

  # Should still produce valid diagnostics
  expect_true(all(is.finite(diag_sampled$data$resid)))
  expect_true(all(is.finite(diag_sampled$data$fitted)))
})

test_that("Test 9: Invalid inputs produce appropriate errors or warnings", {
  # Test error handling for invalid arguments
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Invalid level for half-normal plot
  expect_error(
    plot(setup$fit, which = 5, level = 1.5, ask = FALSE),
    regexp = "level|between|0.*1"
  )

  expect_error(
    plot(setup$fit, which = 5, level = -0.1, ask = FALSE),
    regexp = "level|between|0.*1"
  )

  # Invalid nsim
  expect_error(
    plot(setup$fit, which = 5, nsim = -10, ask = FALSE),
    regexp = "nsim|positive|integer"
  )
})

test_that("Test 10: Plots work across different distribution families", {
  # Test that diagnostic plots work for all supported families
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  families <- c("kw", "beta", "ekw")

  for (fam in families) {
    fit <- gkwreg(y ~ x1, data = setup$data, family = fam)

    # Should produce plots without error
    expect_no_error(
      plot(fit, which = c(1, 4, 6), ask = FALSE)
    )

    # Diagnostics should be extractable
    diag <- plot(fit,
      which = 1:6,
      save_diagnostics = TRUE, ask = FALSE
    )

    expect_s3_class(diag$data, "data.frame")
    expect_true(all(is.finite(diag$data$resid)))
  }

  # Test family argument override
  fit_kw <- gkwreg(y ~ x1, data = setup$data, family = "kw")

  expect_no_error(
    plot(fit_kw, which = 6, family = "beta", ask = FALSE)
  )
})

# =============================================================================
# ADDITIONAL VALIDATION TESTS
# =============================================================================

test_that("Plot handles models with different formula specifications", {
  # Test plots with various model complexities
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Intercept only
  fit_int <- gkwreg(y ~ 1, data = setup$data, family = "kw")
  expect_no_error(plot(fit_int, which = c(1, 6), ask = FALSE))

  # With interactions
  fit_inter <- gkwreg(y ~ x1 * x2, data = setup$data, family = "kw")
  expect_no_error(plot(fit_inter, which = c(4, 6), ask = FALSE))

  # Different predictors per parameter
  fit_diff <- gkwreg(y ~ x1 | x2, data = setup$data, family = "kw")
  expect_no_error(plot(fit_diff, which = 1:6, ask = FALSE))
})

test_that("Plot respects ask parameter behavior", {
  # Test ask parameter logic
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # ask = FALSE should never prompt
  expect_no_error(
    plot(setup$fit, which = 1:6, ask = FALSE)
  )

  # ask = NULL should auto-detect (won't prompt in non-interactive)
  expect_no_error(
    plot(setup$fit, which = 1:6, ask = NULL)
  )
})

test_that("Plot customization via ... works for base graphics", {
  # Test that additional graphical parameters are accepted
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Custom point appearance
  expect_no_error(
    plot(setup$fit,
      which = 1, pch = 16, col = "blue",
      cex = 0.8, ask = FALSE
    )
  )

  # Multiple customizations
  expect_no_error(
    plot(setup$fit,
      which = 6, pch = 21, col = "red",
      bg = "yellow", lwd = 2, ask = FALSE
    )
  )
})

test_that("Subtitle customization works correctly", {
  # Test main and sub.caption arguments
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  # Custom subtitle
  expect_no_error(
    plot(setup$fit,
      which = 1,
      sub.caption = "Custom Model Description", ask = FALSE
    )
  )

  # No subtitle
  expect_no_error(
    plot(setup$fit, which = 1, sub.caption = "", ask = FALSE)
  )

  # With main title
  expect_no_error(
    plot(setup$fit, which = 1, main = "Diagnostics:", ask = FALSE)
  )
})

test_that("Cook's distance and leverage plots identify outliers", {
  # Test that influence diagnostics are calculated
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  diag <- plot(setup$fit,
    which = c(2, 3),
    save_diagnostics = TRUE, ask = FALSE
  )

  # Should have Cook's distance
  expect_true(all(diag$data$cook_dist >= 0))

  # Should have leverage values
  expect_true(all(diag$data$leverage >= 0))
  expect_true(all(diag$data$leverage <= 1))

  # Should have thresholds
  expect_true("cook_threshold" %in% names(diag$model_info))
  expect_true("leverage_threshold" %in% names(diag$model_info))
})

test_that("Plot handles very small datasets gracefully", {
  # Test with minimal data
  set.seed(111)
  n_small <- 20
  x <- rnorm(n_small)
  y <- rkw(n_small, alpha = 2, beta = 2)
  y <- pmax(pmin(y, 1 - 1e-10), 1e-10)

  small_data <- data.frame(y = y, x = x)
  fit_small <- gkwreg(y ~ x, data = small_data, family = "kw")

  pdf(NULL)
  on.exit(dev.off())

  expect_no_error(
    plot(fit_small, which = 1:6, ask = FALSE)
  )

  diag <- plot(fit_small, save_diagnostics = TRUE, ask = FALSE)
  expect_equal(nrow(diag$data), n_small)
})

test_that("Half-normal plot includes envelope when requested", {
  # Test that plot 5 generates envelope data
  # setup <- setup_plot_data()

  pdf(NULL)
  on.exit(dev.off())

  diag <- plot(setup$fit,
    which = 5, nsim = 100,
    save_diagnostics = TRUE, ask = FALSE
  )

  expect_true("half_normal" %in% names(diag))

  if (!is.null(diag$half_normal)) {
    expect_true("observed" %in% names(diag$half_normal))
    expect_true("lower" %in% names(diag$half_normal))
    expect_true("upper" %in% names(diag$half_normal))

    # Envelope should bound expected values
    expect_true(all(diag$half_normal$lower <= diag$half_normal$upper))
  }
})
