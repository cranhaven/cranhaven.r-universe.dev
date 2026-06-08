# tests/testthat/test-plot.R
# Smoke tests for et_plot_* functions — verify they return ggplot objects
# and do not error with mock data, without requiring a real brms fit.

library(ggplot2)

# ── Helper: minimal mock decomposition data.frame ───────────────────────────

.mock_decomp <- function(n_obs = 5) {
  data.frame(
    obs_id       = seq_len(n_obs),
    total_var    = runif(n_obs, 0.3, 0.5),
    param_var    = runif(n_obs, 0.05, 0.15),
    env_var      = runif(n_obs, 0.02, 0.08),
    residual_var = rep(0.22, n_obs)
  )
}

.mock_shelf_life <- function(n_obs = 5) {
  sl <- data.frame(
    obs_id          = seq_len(n_obs),
    time            = 2020 + seq_len(n_obs) - 1L,
    ci_width        = runif(n_obs, 0.8, 1.4),
    plausible_range = 3.5,
    ratio           = runif(n_obs, 0.2, 0.4),
    informative     = TRUE,
    stringsAsFactors = FALSE
  )
  attr(sl, "threshold") <- 1.0
  attr(sl, "horizon")   <- list(
    value            = NA_real_,
    type             = "lower_bound",
    last_informative = 2024,
    description      = "All periods informative."
  )
  structure(sl, class = c("et_shelf_life", "data.frame"))
}

.mock_calibration <- function() {
  data.frame(
    ci_level           = c(0.50, 0.80, 0.90, 0.95),
    nominal            = c(0.50, 0.80, 0.90, 0.95),
    observed_coverage  = c(0.60, 0.80, 1.00, 1.00),
    n_obs              = 5L,
    calibration_error  = c(0.10, 0.00, 0.10, 0.05),
    stringsAsFactors   = FALSE
  )
}

# ── et_plot_decomposition ────────────────────────────────────────────────────

test_that("et_plot_decomposition returns a ggplot object (proportional)", {
  p <- et_plot_decomposition(.mock_decomp(), proportional = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("et_plot_decomposition returns a ggplot object (absolute)", {
  p <- et_plot_decomposition(.mock_decomp(), proportional = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("et_plot_decomposition handles grouped decomp with group_col", {
  d <- rbind(
    cbind(group = "A", .mock_decomp(3)),
    cbind(group = "B", .mock_decomp(3))
  )
  p <- et_plot_decomposition(d, group_col = "group")
  expect_s3_class(p, "ggplot")
})

# ── et_plot_shelf_life ───────────────────────────────────────────────────────

test_that("et_plot_shelf_life returns a ggplot object", {
  p <- et_plot_shelf_life(.mock_shelf_life())
  expect_s3_class(p, "ggplot")
})

test_that("et_plot_shelf_life with show_ratio = FALSE works", {
  p <- et_plot_shelf_life(.mock_shelf_life(), show_ratio = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("et_plot_shelf_life with grouped et_shelf_life adds colour", {
  sl_g <- cbind(group = rep(c("A","B"), each = 3),
                .mock_shelf_life(6))
  attr(sl_g, "threshold") <- 1.0
  class(sl_g) <- c("et_shelf_life", "data.frame")
  p <- et_plot_shelf_life(sl_g)
  expect_s3_class(p, "ggplot")
})

# ── et_plot_calibration ──────────────────────────────────────────────────────

test_that("et_plot_calibration returns a ggplot object (single group)", {
  p <- et_plot_calibration(.mock_calibration())
  expect_s3_class(p, "ggplot")
})

test_that("et_plot_calibration returns a ggplot object (grouped)", {
  cal <- rbind(
    cbind(group = "A", .mock_calibration()),
    cbind(group = "B", .mock_calibration())
  )
  p <- et_plot_calibration(cal)
  expect_s3_class(p, "ggplot")
})
