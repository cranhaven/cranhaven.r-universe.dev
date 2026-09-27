# tests/testthat/test-glm-methods.R
#
# Tests for S3 methods on survey_glm_fit — PR 3 scope (feature/glm-methods)
# Item 17 (print(clean(fit)) snapshot) deferred to PR 4.
#
# Spec reference: plans/spec-phase-2.md §V, §9.2
# Error classes:  plans/error-messages.md (surveycore_error_predict_no_fit,
#                 surveycore_error_invalid_conf_level)

library(surveycore)

# ── Shared fixtures ────────────────────────────────────────────────────────────

.glm_taylor <- function(seed = 1L) {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = seed)
  as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
}

.glm_srs <- function(seed = 1L) {
  df <- make_survey_data(n = 200L, seed = seed)
  as_survey(df, weights = wt)
}

# Basic gaussian fit used across most tests
.fit_gaussian <- function() {
  survey_glm(.glm_taylor(), y1 ~ y2 + y3)
}

# Binomial fit used for residuals / response-scale tests
.fit_binomial <- function() {
  d <- .glm_taylor()
  survey_glm(d, y3 ~ y2, family = binomial())
}

# Helper: replace fit_ with NULL (simulates post-deserialization state)
.strip_fit <- function(fit) {
  fit@fit_ <- NULL
  fit
}

# ---------------------------------------------------------------------------
# Item 1: print()
# ---------------------------------------------------------------------------

test_that("print.survey_glm_fit() returns invisible(x)", {
  fit <- .fit_gaussian()
  result <- withVisible(print(fit))
  expect_false(result$visible)
  expect_true(S7::S7_inherits(result$value, survey_glm_fit))
})

test_that("print.survey_glm_fit() snapshot matches expected format", {
  fit <- .fit_gaussian()
  expect_snapshot(print(fit))
})

# ---------------------------------------------------------------------------
# Item 2: summary()
# ---------------------------------------------------------------------------

test_that("summary.survey_glm_fit() returns survey_glm_summary class", {
  fit <- .fit_gaussian()
  s <- summary(fit)
  expect_true(inherits(s, "survey_glm_summary"))
  expect_type(s, "list")
})

test_that("survey_glm_summary has all required fields", {
  fit <- .fit_gaussian()
  s <- summary(fit)
  expected_fields <- c(
    "coefficients",
    "deviance",
    "null_deviance",
    "df_residual",
    "df_null",
    "dispersion",
    "family",
    "call",
    "design_type",
    "degf"
  )
  expect_true(all(expected_fields %in% names(s)))
})

test_that("survey_glm_summary$coefficients is a p x 4 named matrix", {
  fit <- .fit_gaussian()
  s <- summary(fit)
  coef_mat <- s$coefficients
  p <- length(fit@coefficients)
  expect_true(is.matrix(coef_mat))
  expect_identical(dim(coef_mat), c(p, 4L))
  expect_identical(
    colnames(coef_mat),
    c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  )
  expect_identical(rownames(coef_mat), names(fit@coefficients))
})

# Item 2a: print(summary(fit)) snapshot
test_that("print.survey_glm_summary() returns invisible(x)", {
  fit <- .fit_gaussian()
  s <- summary(fit)
  result <- withVisible(print(s))
  expect_false(result$visible)
  expect_identical(result$value, s)
})

test_that("print.survey_glm_summary() snapshot matches expected format", {
  fit <- .fit_gaussian()
  expect_snapshot(print(summary(fit)))
})

# Item 2b: summary() with fit_ = NULL
test_that("summary() with fit_ = NULL errors with surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    summary(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, summary(fit_null))
})

# ---------------------------------------------------------------------------
# Item 3: coef()
# ---------------------------------------------------------------------------

test_that("coef() returns fit@coefficients (identical)", {
  fit <- .fit_gaussian()
  expect_identical(coef(fit), fit@coefficients)
})

test_that("coef() returns a named numeric vector", {
  fit <- .fit_gaussian()
  expect_type(coef(fit), "double")
  expect_false(is.null(names(coef(fit))))
})

# ---------------------------------------------------------------------------
# Item 4: vcov()
# ---------------------------------------------------------------------------

test_that("vcov() returns fit@vcov (identical)", {
  fit <- .fit_gaussian()
  expect_identical(vcov(fit), fit@vcov)
})

test_that("vcov() row and column names match coefficient names", {
  fit <- .fit_gaussian()
  coef_names <- names(fit@coefficients)
  expect_identical(rownames(vcov(fit)), coef_names)
  expect_identical(colnames(vcov(fit)), coef_names)
})

# ---------------------------------------------------------------------------
# Item 5: predict(newdata = NULL) — response scale
# ---------------------------------------------------------------------------

test_that("predict(fit) returns fitted values on response scale", {
  fit <- .fit_gaussian()
  preds <- predict(fit)
  expect_type(preds, "double")
  expect_equal(length(preds), length(fit@fitted_values))
  # predict.glm() returns a named vector; @fitted_values is unnamed
  expect_equal(unname(preds), fit@fitted_values)
})

# Item 5a: predict(type = "link") — differs from response for binomial
test_that("predict(fit, type = 'link') returns link-scale fitted values", {
  fit_bin <- .fit_binomial()
  resp_preds <- predict(fit_bin, type = "response")
  link_preds <- predict(fit_bin, type = "link")
  # For binomial logit: link values are log-odds, response values are probs
  expect_true(any(resp_preds != link_preds))
  # Response is in (0, 1) for binomial
  expect_true(all(resp_preds > 0 & resp_preds < 1))
})

# ---------------------------------------------------------------------------
# Item 6: predict(newdata = df)
# ---------------------------------------------------------------------------

test_that("predict(fit, new_data = df) returns correct length vector", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  nd <- d@data[1:10, ]
  preds <- predict(fit, new_data = nd)
  expect_type(preds, "double")
  expect_equal(length(preds), 10L)
})

# Item 6a: predict(type = "terms") returns matrix
test_that("predict(fit, type = 'terms') returns a matrix", {
  fit <- .fit_gaussian()
  terms_preds <- predict(fit, type = "terms")
  expect_true(is.matrix(terms_preds))
  # Number of columns equals number of non-intercept terms
  expect_gt(ncol(terms_preds), 0L)
})

# ---------------------------------------------------------------------------
# Item 7: predict() with NULL fit_
# ---------------------------------------------------------------------------

test_that("predict() with fit_ = NULL errors with surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    predict(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, predict(fit_null))
})

# ---------------------------------------------------------------------------
# Item 8: fitted()
# ---------------------------------------------------------------------------

test_that("fitted() returns object@fitted_values", {
  fit <- .fit_gaussian()
  expect_identical(fitted(fit), fit@fitted_values)
})

test_that("fitted() values numerically equal predict(fit) values", {
  fit <- .fit_gaussian()
  # predict.glm() returns a named vector; fitted() returns unnamed @fitted_values
  expect_equal(unname(predict(fit)), fitted(fit))
})

# ---------------------------------------------------------------------------
# Item 9: residuals(type = "response")
# ---------------------------------------------------------------------------

test_that("residuals(type = 'response') returns y - fitted_values", {
  fit <- .fit_gaussian()
  resids <- residuals(fit, type = "response")
  y <- stats::model.response(stats::model.frame(fit@fit_))
  expect_equal(resids, as.numeric(y - fit@fitted_values))
})

# ---------------------------------------------------------------------------
# Item 10: residuals(type = "working")
# ---------------------------------------------------------------------------

test_that("residuals(type = 'working') returns fit@residuals", {
  fit <- .fit_gaussian()
  expect_equal(residuals(fit, type = "working"), fit@residuals)
})

# ---------------------------------------------------------------------------
# Item 11: residuals(type = "pearson")
# ---------------------------------------------------------------------------

test_that("residuals(type = 'pearson') delegates to fit_ for binomial (differs from working)", {
  fit_bin <- .fit_binomial()
  pearson <- residuals(fit_bin, type = "pearson")
  working <- residuals(fit_bin, type = "working")
  # Pearson and working residuals differ for binomial family
  expect_true(any(pearson != working))
})

# ---------------------------------------------------------------------------
# Item 12: residuals(type = "deviance")
# ---------------------------------------------------------------------------

test_that("residuals(type = 'deviance') delegates to fit_ for binomial (differs from response)", {
  fit_bin <- .fit_binomial()
  deviance_r <- residuals(fit_bin, type = "deviance")
  response_r <- residuals(fit_bin, type = "response")
  expect_true(any(deviance_r != response_r))
})

# ---------------------------------------------------------------------------
# Item 13: residuals(type = "partial")
# ---------------------------------------------------------------------------

test_that("residuals(type = 'partial') returns a matrix with one column per predictor", {
  fit <- .fit_gaussian()
  partial <- residuals(fit, type = "partial")
  expect_true(is.matrix(partial))
  # Number of columns = number of non-intercept predictors
  n_pred <- length(fit@coefficients) - 1L
  expect_equal(ncol(partial), n_pred)
})

# ---------------------------------------------------------------------------
# Item 14: residuals(type = "response") with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("residuals(type = 'response') with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    residuals(fit_null, type = "response"),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, residuals(fit_null, type = "response"))
})

# Item 14a: residuals(type = "pearson") with fit_ = NULL
test_that("residuals(type = 'pearson') with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    residuals(fit_null, type = "pearson"),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, residuals(fit_null, type = "pearson"))
})

# ---------------------------------------------------------------------------
# Item 15: residuals(type = "deviance") with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("residuals(type = 'deviance') with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    residuals(fit_null, type = "deviance"),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, residuals(fit_null, type = "deviance"))
})

# ---------------------------------------------------------------------------
# Item 16: residuals(type = "partial") with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("residuals(type = 'partial') with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    residuals(fit_null, type = "partial"),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, residuals(fit_null, type = "partial"))
})

# Item 16a: residuals(type = "response") binomial — values in [-1, 1]
test_that("residuals(type = 'response') binomial uses model frame response (0/1)", {
  fit_bin <- .fit_binomial()
  resids <- residuals(fit_bin, type = "response")
  # Response is 0/1 so residuals are in [-1, 1]
  expect_true(all(resids >= -1 & resids <= 1))
})

# ---------------------------------------------------------------------------
# Item 17: print(clean(fit)) — deferred to PR 4
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Item 18: confint() happy path
# ---------------------------------------------------------------------------

test_that("confint() returns matrix with correct row and column names", {
  fit <- .fit_gaussian()
  ci <- confint(fit)
  expect_true(is.matrix(ci))
  expect_identical(rownames(ci), names(fit@coefficients))
  expect_identical(colnames(ci), c("2.5 %", "97.5 %"))
})

test_that("confint() bounds match manual calculation", {
  fit <- .fit_gaussian()
  ci <- confint(fit)
  se <- sqrt(diag(fit@vcov))
  p <- length(fit@coefficients)
  df <- max(1, fit@degf - (p - 1L))
  hw <- stats::qt(0.975, df = df) * se
  expected_low <- fit@coefficients - hw
  expected_high <- fit@coefficients + hw
  expect_equal(ci[, "2.5 %"], expected_low, tolerance = 1e-12)
  expect_equal(ci[, "97.5 %"], expected_high, tolerance = 1e-12)
})

test_that("confint(parm = ...) subsets by name correctly", {
  fit <- .fit_gaussian()
  full_ci <- confint(fit)
  sub_ci <- confint(fit, parm = "(Intercept)")
  expect_identical(sub_ci, full_ci["(Intercept)", , drop = FALSE])
})

test_that("confint() with non-default level uses correct quantile", {
  fit <- .fit_gaussian()
  ci_90 <- confint(fit, level = 0.90)
  ci_95 <- confint(fit, level = 0.95)
  # Wider CI at higher confidence level
  width_90 <- ci_90[, 2L] - ci_90[, 1L]
  width_95 <- ci_95[, 2L] - ci_95[, 1L]
  expect_true(all(width_95 > width_90))
})

# ---------------------------------------------------------------------------
# Item 19: confint() invalid level
# ---------------------------------------------------------------------------

test_that("confint() with level outside (0,1) errors surveycore_error_invalid_conf_level", {
  fit <- .fit_gaussian()
  expect_error(
    confint(fit, level = 1.5),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_snapshot(error = TRUE, confint(fit, level = 1.5))
})

# ---------------------------------------------------------------------------
# Item 20: formula()
# ---------------------------------------------------------------------------

test_that("formula() returns object@formula (identical)", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  expect_identical(formula(fit), y1 ~ y2 + y3)
})

# ---------------------------------------------------------------------------
# Item 21: terms()
# ---------------------------------------------------------------------------

test_that("terms() returns same object as terms(fit@fit_)", {
  fit <- .fit_gaussian()
  expect_equal(terms(fit), stats::terms(fit@fit_))
})

# ---------------------------------------------------------------------------
# Item 22: terms() with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("terms() with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    terms(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, terms(fit_null))
})

# ---------------------------------------------------------------------------
# Item 23: model.matrix()
# ---------------------------------------------------------------------------

test_that("model.matrix() returns matrix with correct dimensions", {
  fit <- .fit_gaussian()
  mm <- model.matrix(fit)
  expect_true(is.matrix(mm))
  expect_equal(nrow(mm), length(fit@fitted_values))
  expect_equal(ncol(mm), length(fit@coefficients))
})

# ---------------------------------------------------------------------------
# Item 24: model.matrix() with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("model.matrix() with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    model.matrix(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, model.matrix(fit_null))
})

# ---------------------------------------------------------------------------
# Item 25: model.frame()
# ---------------------------------------------------------------------------

test_that("model.frame() returns data frame with model formula columns", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  mf <- model.frame(fit)
  expect_true(is.data.frame(mf))
  expect_true(all(c("y1", "y2", "y3") %in% names(mf)))
})

# ---------------------------------------------------------------------------
# Item 26: model.frame() with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("model.frame() with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    model.frame(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, model.frame(fit_null))
})

# ---------------------------------------------------------------------------
# Item 27: deviance()
# ---------------------------------------------------------------------------

test_that("deviance() returns object@deviance", {
  fit <- .fit_gaussian()
  expect_identical(deviance(fit), fit@deviance)
})

# ---------------------------------------------------------------------------
# Item 28: df.residual()
# ---------------------------------------------------------------------------

test_that("df.residual() returns object@df_residual", {
  fit <- .fit_gaussian()
  expect_identical(df.residual(fit), fit@df_residual)
})

# ---------------------------------------------------------------------------
# Item 29: nobs()
# ---------------------------------------------------------------------------

test_that("nobs() returns length(object@fitted_values)", {
  fit <- .fit_gaussian()
  expect_identical(nobs(fit), length(fit@fitted_values))
})

# ---------------------------------------------------------------------------
# Item 30: hatvalues()
# ---------------------------------------------------------------------------

test_that("hatvalues() returns numeric vector matching hatvalues(fit@fit_)", {
  fit <- .fit_gaussian()
  hv <- hatvalues(fit)
  expect_type(hv, "double")
  expect_equal(length(hv), length(fit@fitted_values))
  expect_equal(hv, stats::hatvalues(fit@fit_))
})

# ---------------------------------------------------------------------------
# Item 31: hatvalues() with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("hatvalues() with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    hatvalues(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, hatvalues(fit_null))
})

# ---------------------------------------------------------------------------
# Item 32: logLik()
# ---------------------------------------------------------------------------

test_that("logLik() returns same value as logLik(fit@fit_)", {
  fit <- .fit_gaussian()
  expect_equal(logLik(fit), stats::logLik(fit@fit_))
})

# ---------------------------------------------------------------------------
# Item 33: logLik() with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("logLik() with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    logLik(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, logLik(fit_null))
})

# ---------------------------------------------------------------------------
# Item 34: AIC() / BIC() happy path
# ---------------------------------------------------------------------------

test_that("AIC() returns same value as AIC(fit@fit_)", {
  fit <- .fit_gaussian()
  expect_equal(AIC(fit), stats::AIC(fit@fit_))
})

test_that("BIC() returns same value as BIC(fit@fit_)", {
  fit <- .fit_gaussian()
  expect_equal(BIC(fit), stats::BIC(fit@fit_))
})

# ---------------------------------------------------------------------------
# Item 35: AIC() / BIC() with fit_ = NULL
# ---------------------------------------------------------------------------

test_that("AIC() with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    AIC(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, AIC(fit_null))
})

test_that("BIC() with fit_ = NULL errors surveycore_error_predict_no_fit", {
  fit_null <- .strip_fit(.fit_gaussian())
  expect_error(
    BIC(fit_null),
    class = "surveycore_error_predict_no_fit"
  )
  expect_snapshot(error = TRUE, BIC(fit_null))
})

# ---------------------------------------------------------------------------
# Item 36: update() happy path
# ---------------------------------------------------------------------------

test_that("update(fit, family = poisson()) returns new survey_glm_fit with Poisson family", {
  d <- .glm_taylor()
  # Use a non-negative response for Poisson
  d@data$y_count <- as.integer(abs(round(d@data$y1)))
  fit_gauss <- survey_glm(d, y_count ~ y2)
  fit_pois <- update(fit_gauss, family = poisson())
  expect_true(S7::S7_inherits(fit_pois, survey_glm_fit))
  expect_identical(fit_pois@family$family, "poisson")
})

# ---------------------------------------------------------------------------
# Item 17: clean() happy path (added in PR 4 — feature/glm-clean)
# ---------------------------------------------------------------------------

test_that("print(clean(fit)) snapshot matches expected format and class is correct", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit)
  # Class hierarchy
  expect_identical(
    class(result),
    c("survey_glm_tidy", "survey_result", "tbl_df", "tbl", "data.frame")
  )
  # Invariants
  test_glm_tidy_invariants(result)
  # Print snapshot — inherits print.survey_result() from Phase 1
  expect_snapshot(print(result))
})


# ---------------------------------------------------------------------------
# B4: .glm_design_type_label() for survey_nonprob
# ---------------------------------------------------------------------------

test_that("print.survey_glm_fit() shows 'Non-probability' for survey_nonprob design", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "bootstrap",
    seed = 7L
  )
  # as_survey_nonprob requires repweights
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  d_np <- as_survey_nonprob(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols)
  )
  fit_np <- survey_glm(d_np, y1 ~ y2)
  out <- capture.output(print(fit_np))
  design_line <- grep("Design:", out, value = TRUE)
  expect_length(design_line, 1L)
  expect_true(grepl("Non-probability", design_line))
  expect_false(grepl("Calibrated", design_line))
})


# ---------------------------------------------------------------------------
# B5: confint() error text uses "level" not "conf_level"
# ---------------------------------------------------------------------------

test_that("confint() invalid level error references 'level' arg name", {
  fit <- .fit_gaussian()
  expect_error(
    confint(fit, level = 1.5),
    class = "surveycore_error_invalid_conf_level"
  )
  # Snapshot must contain "level" not "conf_level" in the error message
  expect_snapshot(error = TRUE, confint(fit, level = 1.5))
})


# ---------------------------------------------------------------------------
# B6: update.survey_glm_fit wrong error class when @call is NULL
# ---------------------------------------------------------------------------

test_that("update() with @call = NULL errors with surveycore_error_update_no_call", {
  fit <- .fit_gaussian()
  fit_no_call <- fit
  fit_no_call@call <- NULL
  expect_error(
    update(fit_no_call),
    class = "surveycore_error_update_no_call"
  )
  expect_snapshot(error = TRUE, update(fit_no_call))
})
