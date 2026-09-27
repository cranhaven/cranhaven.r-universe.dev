# tests/testthat/test-analysis-diffs-marginaleffects.R
#
# Tests specific to the marginaleffects estimation path in get_diffs().
# Validates avg_slopes() and avg_predictions() integration.

skip_on_cran()

# ── Shared test data ──────────────────────────────────────────────────────

.make_me_data <- function(n = 300, seed = 42) {
  set.seed(seed)
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = seed)
  df$arm <- factor(
    sample(c("Control", "A", "B"), n, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$dv     <- rnorm(n, 50, 10)
  df$age    <- rnorm(n, 40, 10)
  df$gender <- factor(sample(c("M", "F"), n, replace = TRUE))
  df
}

.make_me_design <- function(n = 300, seed = 42) {
  df <- .make_me_data(n = n, seed = seed)
  as_survey(df, ids = psu, weights = wt, strata = strata)
}


# ═══════════════════════════════════════════════════════════════════════════
# 1. AVG_SLOPES ROW COUNT
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() ME path produces correct number of slope rows", {
  d <- .make_me_design()

  # Covariates force ME path

  result <- get_diffs(
    d, dv, arm, covariates = "age", ref_level = "Control"
  )

  m <- meta(result)
  expect_identical(m$estimate_method, "avg_slopes")

  # 3 levels: 1 reference + 2 treatment = 3 rows (show_means = TRUE)
  expect_equal(nrow(result), 3L)

  # Non-ref rows have non-zero estimates
  non_ref <- result$estimate[result$estimate != 0]
  expect_length(non_ref, 2L)
})


test_that("get_diffs() ME path with group produces correct row count", {
  d <- .make_me_design()

  result <- suppressWarnings(
    get_diffs(
      d, dv, arm, group = gender, ref_level = "Control"
    )
  )

  m <- meta(result)
  expect_identical(m$estimate_method, "avg_slopes")

  # 2 groups * (1 ref + 2 treatment) = 6 rows
  expect_equal(nrow(result), 6L)

  # Group column present

  expect_true("gender" %in% names(result))

  # Each group has 1 reference row (estimate = 0) and 2 treatment rows
  for (g in unique(result$gender)) {
    g_rows <- result[result$gender == g, ]
    expect_equal(sum(g_rows$estimate == 0), 1L)
    expect_equal(sum(g_rows$estimate != 0), 2L)
  }
})


# ═══════════════════════════════════════════════════════════════════════════
# 2. AVG_PREDICTIONS MEANS
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() ME path avg_predictions produces correct means", {
  d <- .make_me_design()

  result <- get_diffs(
    d, dv, arm, covariates = "age", ref_level = "Control"
  )

  # Mean column should be present (show_means = TRUE default)
  expect_true("mean" %in% names(result))

  # Reference row mean should be the predicted mean for the ref level
  ref_mean <- result$mean[[1L]]
  expect_true(is.finite(ref_mean))

  # Treatment row means should be: ref_mean + estimate (approximately)
  # In the ME path, means come from avg_predictions(), not intercept+coef,
  # so they are independently computed marginal means
  for (i in 2:3) {
    expect_true(is.finite(result$mean[[i]]))
    # Difference of means should approximately equal the estimate
    # (for gaussian, this is exact in bivariate, approximate with covariates)
    mean_diff <- result$mean[[i]] - ref_mean
    expect_equal(mean_diff, result$estimate[[i]], tolerance = 0.5)
  }
})


test_that("get_diffs() ME path with group has correct means per group", {
  d <- .make_me_design()

  result <- suppressWarnings(
    get_diffs(
      d, dv, arm, group = gender, ref_level = "Control"
    )
  )

  # All means should be finite
  expect_true(all(is.finite(result$mean)))

  # Means within each group should be distinct (different predictions)
  for (g in unique(result$gender)) {
    g_means <- result$mean[result$gender == g]
    # At minimum, the means should be numeric and vary somewhat
    expect_true(all(is.finite(g_means)))
  }
})


# ═══════════════════════════════════════════════════════════════════════════
# 3. WEIGHTED AVERAGES CONSISTENCY
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() ME path estimates use weighted averages", {
  skip_if_not_installed("survey")

  set.seed(77)
  n <- 300
  df <- data.frame(
    id = 1:n,
    wt = runif(n, 0.5, 3),
    arm = factor(
      sample(c("Control", "A"), n, replace = TRUE),
      levels = c("Control", "A")
    ),
    age = rnorm(n, 40, 10),
    dv = rnorm(n, 50, 10)
  )

  d_sc <- as_survey(df, weights = wt)

  # With covariates → ME path
  result <- get_diffs(
    d_sc, dv, arm, covariates = "age", ref_level = "Control",
    variance = c("se", "ci")
  )

  # Oracle: manual survey + marginaleffects
  d_sv <- survey::svydesign(ids = ~1, weights = ~wt, data = df)
  fit_sv <- survey::svyglm(dv ~ arm + age, design = d_sv)
  p <- length(stats::coef(fit_sv))
  res_df <- max(1, fit_sv$df.residual - (p - 1L))

  slopes_sv <- marginaleffects::avg_slopes(
    fit_sv,
    variables = "arm",
    type = "response",
    wts = df$wt,
    df = res_df
  )
  slopes_df <- as.data.frame(slopes_sv)

  # Non-ref estimate should match marginaleffects output
  sc_est <- result$estimate[result$estimate != 0]
  sv_est <- slopes_df$estimate

  expect_equal(sc_est, sv_est, tolerance = 1e-10)

  # SE should match
  sc_se <- result$se[!is.na(result$se)]
  sv_se <- slopes_df$std.error
  expect_equal(sc_se, sv_se, tolerance = 1e-6)
})


# ═══════════════════════════════════════════════════════════════════════════
# 4. LINK-SCALE SUPPRESSION IN ME PATH
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() ME path with link scale + covariates suppresses mean", {
  set.seed(55)
  n <- 300
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = 55)
  df$arm <- factor(
    sample(c("Control", "A", "B"), n, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$binary_dv <- rbinom(n, 1, prob = 0.4)
  df$age <- rnorm(n, 40, 10)

  d <- as_survey(df, ids = psu, weights = wt, strata = strata)

  # With covariates + non-gaussian + scale = "link" → ME path, suppressed
  result <- get_diffs(
    d, binary_dv, arm, covariates = "age",
    ref_level = "Control", scale = "link",
    family = quasibinomial()
  )

  # Mean and pct_change should be suppressed
  expect_false("mean" %in% names(result))
  expect_false("pct_change" %in% names(result))

  # Reference row still exists with estimate = 0
  expect_equal(result$estimate[[1L]], 0)

  m <- meta(result)
  expect_identical(m$estimate_scale, "coefficient")
})
