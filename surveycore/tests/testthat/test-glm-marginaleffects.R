# tests/testthat/test-glm-marginaleffects.R
#
# Tests for marginaleffects extension interface — PR 5 scope
#
# Spec reference: plans/spec-phase-2.md §7.7
# All 10 test items from spec §7.7. Every block gated with
# skip_if_not_installed("marginaleffects").

skip_on_cran()

library(surveycore)

# ── Shared test fixtures ───────────────────────────────────────────────────────

.me_taylor_gaussian <- function(seed = 1L) {
  df  <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = seed)
  d   <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  survey_glm(d, y1 ~ y2)
}

.me_taylor_binomial <- function(seed = 1L) {
  df       <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = seed)
  df$y_bin <- as.integer(df$y1 > median(df$y1))
  d        <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  survey_glm(d, y_bin ~ y2, family = binomial("logit"))
}

# Helper: strip fit_ to simulate post-deserialization state
.me_strip_fit <- function(fit) {
  fit@fit_ <- NULL
  fit
}

# ---------------------------------------------------------------------------
# Item 1: get_coef() returns fit@coefficients (expect_identical)
# ---------------------------------------------------------------------------

test_that("get_coef() returns fit@coefficients exactly", {
  skip_if_not_installed("marginaleffects")
  fit    <- .me_taylor_gaussian()
  result <- marginaleffects::get_coef(fit)
  expect_identical(result, fit@coefficients)
})

# ---------------------------------------------------------------------------
# Item 2: get_vcov() returns fit@vcov (expect_identical)
# ---------------------------------------------------------------------------

test_that("get_vcov() returns fit@vcov exactly", {
  skip_if_not_installed("marginaleffects")
  fit    <- .me_taylor_gaussian()
  result <- marginaleffects::get_vcov(fit)
  expect_identical(result, fit@vcov)
})

# ---------------------------------------------------------------------------
# Item 3: set_coef() — S7 @coefficients property updated
# ---------------------------------------------------------------------------

test_that("set_coef() updates @coefficients in the returned object", {
  skip_if_not_installed("marginaleffects")
  fit       <- .me_taylor_gaussian()
  new_coefs <- fit@coefficients * 2
  modified  <- marginaleffects::set_coef(fit, new_coefs)
  expect_identical(modified@coefficients, new_coefs)
})

# ---------------------------------------------------------------------------
# Item 4: set_coef() — fit_$coefficients patched
# ---------------------------------------------------------------------------

test_that("set_coef() patches fit_$coefficients in the returned object", {
  skip_if_not_installed("marginaleffects")
  fit       <- .me_taylor_gaussian()
  new_coefs <- fit@coefficients * 2
  modified  <- marginaleffects::set_coef(fit, new_coefs)
  expect_identical(modified@fit_$coefficients, new_coefs)
})

# ---------------------------------------------------------------------------
# Item 5: set_coef() — original object is unmodified (copy-on-modify)
# ---------------------------------------------------------------------------

test_that("set_coef() does not modify the original fit object", {
  skip_if_not_installed("marginaleffects")
  fit            <- .me_taylor_gaussian()
  original_coefs <- fit@coefficients
  new_coefs      <- fit@coefficients * 2
  marginaleffects::set_coef(fit, new_coefs)
  expect_identical(fit@coefficients, original_coefs)
})

# ---------------------------------------------------------------------------
# Item 6: get_predict() — correct structure and row count
# ---------------------------------------------------------------------------

test_that("get_predict() returns data frame with rowid and estimate; nrow matches newdata", {
  skip_if_not_installed("marginaleffects")
  fit     <- .me_taylor_gaussian()
  newdata <- fit@fit_$model[1:10, , drop = FALSE]
  result  <- marginaleffects::get_predict(fit, newdata = newdata)
  expect_s3_class(result, "data.frame")
  expect_true("rowid"    %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_equal(nrow(result), 10L)
  expect_type(result$rowid,    "integer")
  expect_type(result$estimate, "double")
})

# ---------------------------------------------------------------------------
# Item 7: avg_slopes() — Gaussian — AME matches OLS coefficient within 1e-6
# ---------------------------------------------------------------------------

test_that("avg_slopes() Gaussian: AME for continuous predictor matches coefficient within 1e-6", {
  skip_if_not_installed("marginaleffects")
  fit    <- .me_taylor_gaussian()
  slopes <- marginaleffects::avg_slopes(fit, variables = "y2")
  ame    <- slopes$estimate
  coef   <- coef(fit)[["y2"]]
  expect_equal(ame, coef, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Item 8: avg_slopes() — binomial — on probability scale, differs from logit coef
# ---------------------------------------------------------------------------

test_that("avg_slopes() binomial: AME is on probability scale and differs from log-odds coef", {
  skip_if_not_installed("marginaleffects")
  fit    <- .me_taylor_binomial()
  slopes <- marginaleffects::avg_slopes(fit, variables = "y2")
  ame    <- slopes$estimate
  logit_coef <- coef(fit)[["y2"]]
  # AME should be in (-1, 1) — probability scale
  expect_true(all(abs(ame) < 1))
  # AME differs from log-odds coefficient (they live on different scales)
  expect_false(isTRUE(all.equal(ame, logit_coef, tolerance = 1e-6)))
})

# ---------------------------------------------------------------------------
# Item 9: avg_predictions() — binomial — estimates in [0, 1]
# ---------------------------------------------------------------------------

test_that("avg_predictions() binomial: estimate values are in [0, 1]", {
  skip_if_not_installed("marginaleffects")
  fit   <- .me_taylor_binomial()
  preds <- marginaleffects::avg_predictions(fit)
  expect_s3_class(preds, "data.frame")
  expect_true("estimate" %in% names(preds))
  expect_true(all(preds$estimate >= 0))
  expect_true(all(preds$estimate <= 1))
})

# ---------------------------------------------------------------------------
# Item 10: get_predict() with fit_ = NULL errors with surveycore_error_predict_no_fit
# ---------------------------------------------------------------------------

test_that("get_predict() with fit_ = NULL errors with surveycore_error_predict_no_fit", {
  skip_if_not_installed("marginaleffects")
  fit     <- .me_strip_fit(.me_taylor_gaussian())
  newdata <- make_survey_data(n = 5L, seed = 99L)
  expect_error(
    marginaleffects::get_predict(fit, newdata = newdata),
    class = "surveycore_error_predict_no_fit"
  )
})
