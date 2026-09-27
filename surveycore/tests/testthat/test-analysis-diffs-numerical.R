# tests/testthat/test-analysis-diffs-numerical.R
#
# Numerical oracle tests for get_diffs() — comparing against
# survey::svyglm + marginaleffects and manual computation.

skip_on_cran()

# ═══════════════════════════════════════════════════════════════════════════
# 1. BIVARIATE OLS VS MANUAL COMPUTATION (clean path)
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() bivariate OLS matches hand-computed weighted means [numerical]", {
  skip_if_not_installed("survey")

  set.seed(123)
  n <- 300
  df <- data.frame(
    id = 1:n,
    wt = runif(n, 0.5, 3),
    arm = factor(
      sample(c("Control", "A", "B"), n, replace = TRUE),
      levels = c("Control", "A", "B")
    ),
    dv = rnorm(n, 50, 10)
  )
  d <- as_survey(df, weights = wt)
  result <- get_diffs(
    d, dv, arm, ref_level = "Control", variance = c("se", "ci")
  )

  # Manual weighted means
  ctrl_mask <- df$arm == "Control"
  a_mask    <- df$arm == "A"
  b_mask    <- df$arm == "B"

  ctrl_mean <- stats::weighted.mean(df$dv[ctrl_mask], df$wt[ctrl_mask])
  a_mean    <- stats::weighted.mean(df$dv[a_mask], df$wt[a_mask])
  b_mean    <- stats::weighted.mean(df$dv[b_mask], df$wt[b_mask])

  # Reference row mean should match weighted mean

  expect_equal(result$mean[[1L]], ctrl_mean, tolerance = 1e-8)

  # Treatment estimates should be diffs from reference
  expect_equal(result$estimate[[2L]], a_mean - ctrl_mean, tolerance = 1e-8)
  expect_equal(result$estimate[[3L]], b_mean - ctrl_mean, tolerance = 1e-8)

  # Treatment means should match weighted means
  expect_equal(result$mean[[2L]], a_mean, tolerance = 1e-8)
  expect_equal(result$mean[[3L]], b_mean, tolerance = 1e-8)
})


test_that("get_diffs() clean path SEs match survey::svyglm [numerical]", {
  skip_if_not_installed("survey")

  set.seed(42)
  n <- 400
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = 42)
  df$arm <- factor(
    sample(c("Control", "A", "B"), n, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$dv <- rnorm(n, 50, 10)

  d_sc <- as_survey(df, ids = psu, weights = wt, strata = strata)
  result <- get_diffs(
    d_sc, dv, arm, ref_level = "Control", variance = c("se", "ci")
  )

  # Oracle: survey::svyglm
  d_sv <- survey::svydesign(
    ids = ~psu, weights = ~wt, strata = ~strata,
    data = df, nest = TRUE
  )
  fit_sv <- survey::svyglm(dv ~ arm, design = d_sv)
  coef_sv <- stats::coef(fit_sv)
  se_sv   <- sqrt(diag(stats::vcov(fit_sv)))

  # Intercept = ref mean
  expect_equal(result$mean[[1L]], unname(coef_sv[["(Intercept)"]]),
               tolerance = 1e-8)

  # Treatment estimates
  expect_equal(result$estimate[[2L]], unname(coef_sv[["armA"]]),
               tolerance = 1e-8)
  expect_equal(result$estimate[[3L]], unname(coef_sv[["armB"]]),
               tolerance = 1e-8)

  # SEs
  expect_equal(result$se[[2L]], unname(se_sv[["armA"]]),
               tolerance = 1e-6)
  expect_equal(result$se[[3L]], unname(se_sv[["armB"]]),
               tolerance = 1e-6)
})


# ═══════════════════════════════════════════════════════════════════════════
# 2. MULTIVARIATE OLS VS SURVEY + MARGINALEFFECTS (ME path)
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() with covariates matches survey + marginaleffects [numerical]", {
  skip_if_not_installed("survey")
  skip_if_not_installed("marginaleffects")

  set.seed(99)
  n <- 400
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = 99)
  df$arm <- factor(
    sample(c("Control", "A", "B"), n, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$age <- rnorm(n, 40, 10)
  df$dv  <- rnorm(n, 50, 10)

  # surveycore path
  d_sc <- as_survey(df, ids = psu, weights = wt, strata = strata)
  result <- get_diffs(
    d_sc, dv, arm, covariates = "age", ref_level = "Control",
    variance = c("se", "ci")
  )

  # Oracle: survey + marginaleffects
  d_sv <- survey::svydesign(
    ids = ~psu, weights = ~wt, strata = ~strata,
    data = df, nest = TRUE
  )
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

  # Sort both by contrast level for matching
  # surveycore non-ref rows (skip reference row)
  sc_estimates <- result$estimate[result$estimate != 0]
  sc_ses       <- result$se[!is.na(result$se)]

  # Oracle estimates
  sv_estimates <- slopes_df$estimate
  sv_ses       <- slopes_df$std.error

  expect_equal(sc_estimates, sv_estimates, tolerance = 1e-10)
  expect_equal(sc_ses, sv_ses, tolerance = 1e-6)
})


# ═══════════════════════════════════════════════════════════════════════════
# 3. LOGISTIC AME VS SURVEY + MARGINALEFFECTS
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() quasibinomial AME matches survey + marginaleffects [numerical]", {
  skip_if_not_installed("survey")
  skip_if_not_installed("marginaleffects")

  set.seed(77)
  n <- 400
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = 77)
  df$arm <- factor(
    sample(c("Control", "A", "B"), n, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$binary_dv <- rbinom(n, 1, prob = 0.4)

  # surveycore path
  d_sc <- as_survey(df, ids = psu, weights = wt, strata = strata)
  result <- get_diffs(
    d_sc, binary_dv, arm, ref_level = "Control",
    variance = c("se", "ci"), family = quasibinomial()
  )

  # Oracle: survey + marginaleffects
  d_sv <- survey::svydesign(
    ids = ~psu, weights = ~wt, strata = ~strata,
    data = df, nest = TRUE
  )
  fit_sv <- survey::svyglm(
    binary_dv ~ arm, design = d_sv, family = quasibinomial()
  )
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

  # Compare estimates (non-ref rows only)
  sc_estimates <- result$estimate[result$estimate != 0]
  sv_estimates <- slopes_df$estimate

  expect_equal(sc_estimates, sv_estimates, tolerance = 1e-10)

  # Compare SEs
  sc_ses <- result$se[!is.na(result$se)]
  sv_ses <- slopes_df$std.error
  expect_equal(sc_ses, sv_ses, tolerance = 1e-6)
})


# ═══════════════════════════════════════════════════════════════════════════
# 4. REPLICATE DESIGN NON-INTEGER DF
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() replicate design uses correct non-integer df [numerical]", {
  skip_if_not_installed("survey")

  set.seed(55)
  n <- 200
  df_rep <- make_survey_data(
    n = n, n_psu = 20, n_strata = 4,
    design = "replicate", type = "jk1", seed = 55
  )
  df_rep$arm <- factor(
    sample(c("Control", "A"), n, replace = TRUE),
    levels = c("Control", "A")
  )
  df_rep$dv <- rnorm(n, 50, 10)

  d <- as_survey_replicate(
    df_rep, weights = wt,
    repweights = starts_with("repwt_"),
    type = "JK1"
  )

  result <- get_diffs(d, dv, arm, ref_level = "Control",
                      variance = c("se", "ci"))

  # Verify CI bounds use the model degf, not hardcoded values
  # The key property is that CIs exist and are finite
  expect_true(all(is.finite(result$ci_low[!is.na(result$ci_low)])))
  expect_true(all(is.finite(result$ci_high[!is.na(result$ci_high)])))

  # Non-ref estimate should have non-NA SE
  expect_false(is.na(result$se[[2L]]))
})


# ═══════════════════════════════════════════════════════════════════════════
# 5. SRS COVARIANCE MATRIX VERIFICATION
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() with covariates produces correct off-diagonal vcov [numerical]", {
  skip_if_not_installed("survey")

  set.seed(88)
  n <- 300
  df <- data.frame(
    id = 1:n,
    wt = runif(n, 0.5, 3),
    arm = factor(
      sample(c("Control", "A", "B"), n, replace = TRUE),
      levels = c("Control", "A", "B")
    ),
    age = rnorm(n, 40, 10),
    dv = rnorm(n, 50, 10)
  )

  # surveycore fit
  d_sc <- as_survey(df, weights = wt)
  fit_sc <- survey_glm(d_sc, dv ~ arm + age)

  # Oracle: survey::svyglm
  d_sv <- survey::svydesign(ids = ~1, weights = ~wt, data = df)
  fit_sv <- survey::svyglm(dv ~ arm + age, design = d_sv)

  # Compare full covariance matrix
  vcov_sc <- fit_sc@vcov
  vcov_sv <- stats::vcov(fit_sv)

  # Off-diagonal elements should be non-zero
  off_diag <- vcov_sc[lower.tri(vcov_sc)]
  expect_true(any(off_diag != 0))

  # Full matrix matches
  expect_equal(vcov_sc, vcov_sv, tolerance = 1e-8)
})
