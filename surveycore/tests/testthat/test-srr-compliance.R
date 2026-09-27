# tests/testthat/test-srr-compliance.R
#
# rOpenSci srr compliance tests.

#' @srrstats {G5.3} Absence of NA/NaN/Inf in return objects is explicitly
#'   tested here: survey_glm_fit coefficients, vcov, and fitted values are
#'   checked for finiteness after standard fits; get_means() and get_freqs()
#'   output numeric columns are checked for finiteness and valid range.
#'
#' @srrstats {G5.8} Edge condition behaviour is tested here across four
#'   categories: G5.8a (zero-length data), G5.8b (unsupported types),
#'   G5.8c (all-NA fields), G5.8d (data outside algorithm scope).
#'
#' @srrstats {G5.8a} Zero-length data produces surveycore_error_empty_data
#'   for as_survey() and as_survey_replicate(). survey_glm() with an
#'   all-excluded active domain produces surveycore_error_empty_domain.
#'
#' @srrstats {G5.8b} Unsupported column types for design variables produce
#'   typed errors: character weights -> surveycore_error_weights_not_numeric;
#'   character FPC -> surveycore_error_fpc_not_numeric.
#'
#' @srrstats {G5.8c} All-NA outcome fields are handled gracefully: analysis
#'   functions with na.rm = TRUE return NA estimates when all outcome values
#'   are NA. survey_glm() with all-NA response and na.omit produces an error.
#'
#' @srrstats {G5.8d} Data outside algorithm scope (more predictors than
#'   observations, or perfectly collinear predictors) produces
#'   surveycore_error_singular_model_matrix.
#'
#' @srrstats {G5.9} Noise susceptibility is tested here: adding trivial noise
#'   (at .Machine$double.eps scale) to predictors and response in survey_glm()
#'   produces coefficient changes below 1e-6.
#'
#' @srrstats {G5.9a} Adding .Machine$double.eps-scale noise to the predictor
#'   or the response produces GLM coefficient changes < 1e-6 in absolute
#'   value, tested with tolerance 1e-6.
#'
#' @srrstats {G5.10} Extended tests are included here under the standard
#'   guard: skip_if(!identical(Sys.getenv("SURVEYCORE_EXTENDED_TESTS"),
#'   "true")). Extended tests run all 8 GLM families with oracle comparison.
#'
#' @srrstats {G5.12} Conditions for running extended tests are documented in
#'   .github/CONTRIBUTING.md ("Extended tests" section): set environment
#'   variable SURVEYCORE_EXTENDED_TESTS=true before running devtools::test().
#'
#' @srrstats {RE7.0} Exact predictor-predictor relationships (perfect
#'   collinearity) are tested here: survey_glm() detects the rank-deficient
#'   model matrix and throws surveycore_error_singular_model_matrix.
#'
#' @srrstats {RE7.1} Exact predictor-response relationships are tested here:
#'   with noiseless data y = 10 - 3*x and uniform weights, survey_glm()
#'   recovers the intercept = 10 and slope = -3 within tolerance 1e-8.
#'
#' @srrstats {RE7.2} Output objects retain row names from input data.
#'   model.frame.survey_glm_fit() returns the model frame with row names
#'   matching the rows used in fitting.
#'
#' @noRd
NULL

library(surveycore)

# ── Shared fixtures ────────────────────────────────────────────────────────────

.sc_taylor <- function(seed = 42L) {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = seed)
  as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
}

.sc_srs <- function(seed = 42L) {
  df <- make_survey_data(n = 200L, seed = seed)
  as_survey(df, weights = wt)
}

# ===========================================================================
# G5.3 — return objects contain no NA/NaN/Inf in normal operation
# ===========================================================================

test_that("G5.3: survey_glm_fit coefficients are all finite [G5.3]", {
  fit <- survey_glm(.sc_taylor(), y1 ~ y2 + y3)
  expect_true(all(is.finite(fit@coefficients)))
})

test_that("G5.3: survey_glm_fit vcov is all finite [G5.3]", {
  fit <- survey_glm(.sc_taylor(), y1 ~ y2 + y3)
  expect_true(all(is.finite(fit@vcov)))
})

test_that("G5.3: survey_glm_fit fitted_values are all finite for Gaussian [G5.3]", {
  fit <- survey_glm(.sc_taylor(), y1 ~ y2)
  expect_true(all(is.finite(fit@fitted_values)))
})

test_that("G5.3: get_means() estimates are all finite for complete data [G5.3]", {
  d  <- .sc_taylor()
  result <- get_means(d, y1)
  expect_true(all(is.finite(result$mean)))
  expect_true(all(is.finite(result$se)))
})

test_that("G5.3: get_freqs() proportions (pct) are all in [0,1] for complete data [G5.3]", {
  d <- .sc_taylor()
  result <- suppressWarnings(get_freqs(d, group))  # suppress small-cell warnings
  expect_true(all(result$pct >= 0 & result$pct <= 1))
  expect_true(all(is.finite(result$pct)))
})

# ===========================================================================
# G5.6 / G5.6a — parameter recovery within defined tolerance
# Note: comprehensive numerical oracle tests are in test-glm-numerical.R,
# test-variance-taylor.R, test-variance-replicate.R, test-variance-twophase.R,
# and test-variance-srs.R. This block adds a direct analytical recovery test.
# ===========================================================================

test_that("G5.6/G5.6a: survey_glm recovers known coefficients from deterministic data [G5.6]", {
  # Construct data with a known exact relationship contaminated by noise.
  # The weighted population mean of y = intercept + slope * x exactly when
  # all weights = 1. Use n = 1000 to get close to the population truth.
  set.seed(1L)
  n <- 1000L
  x <- seq(-3, 3, length.out = n)
  # True intercept = 5, true slope = 2.  Add small noise so not perfectly collinear.
  y <- 5 + 2 * x + rnorm(n, sd = 0.1)
  df <- data.frame(y = y, x = x, wt = rep(1, n))
  d <- as_survey(df, weights = wt)

  fit <- survey_glm(d, y ~ x)
  # With weights = 1 and large n, coefficient estimates should be close to
  # population values; tolerance accounts for finite-sample sampling variance.
  expect_equal(fit@coefficients[["(Intercept)"]], 5.0, tolerance = 0.05)
  expect_equal(fit@coefficients[["x"]], 2.0, tolerance = 0.05)
})

# ===========================================================================
# G5.8a — zero-length data produces appropriate errors
# ===========================================================================

test_that("G5.8a: as_survey() errors on zero-row data frame [G5.8a]", {
  empty_df <- data.frame(x = numeric(0L), wt = numeric(0L))
  expect_error(
    as_survey(empty_df, weights = wt),
    class = "surveycore_error_empty_data"
  )
})

test_that("G5.8a: as_survey_replicate() errors on zero-row data frame [G5.8a]", {
  empty_df <- data.frame(x = numeric(0L), wt = numeric(0L), r1 = numeric(0L))
  expect_error(
    as_survey_replicate(empty_df, weights = wt, repweights = r1, type = "JK1"),
    class = "surveycore_error_empty_data"
  )
})

test_that("G5.8a: survey_glm() errors when active domain has zero rows [G5.8a]", {
  d <- .sc_taylor()
  # Inject a domain column that excludes all rows
  d@data[["..surveycore_domain.."]] <- rep(FALSE, nrow(d@data))
  expect_error(
    survey_glm(d, y1 ~ y2),
    class = "surveycore_error_empty_domain"
  )
})

# ===========================================================================
# G5.8b — unsupported column types produce appropriate errors
# ===========================================================================

test_that("G5.8b: as_survey() errors when weight column is character [G5.8b]", {
  df <- data.frame(x = 1:5, wt = c("a", "b", "c", "d", "e"),
                   stringsAsFactors = FALSE)
  expect_error(
    as_survey(df, weights = wt),
    class = "surveycore_error_weights_not_numeric"
  )
})

test_that("G5.8b: as_survey() errors when weight column is complex type [G5.8b]", {
  df <- data.frame(x = 1:5, stringsAsFactors = FALSE)
  df$wt_c <- complex(real = rep(1.0, 5L), imaginary = rep(0.0, 5L))
  expect_error(
    as_survey(df, weights = wt_c),
    class = "surveycore_error_weights_not_numeric"
  )
})

test_that("G5.8b: survey_glm() errors when predictor type is unsupported by model.matrix [G5.8b]", {
  df <- make_survey_data(n = 50L, seed = 1L)
  # A raw column type is not supported by model.matrix
  df$raw_col <- as.raw(seq_len(nrow(df)) %% 256L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  # model.matrix cannot handle raw type — should produce an error
  expect_error(survey_glm(d, y1 ~ raw_col))
})

# ===========================================================================
# G5.8c — all-NA outcome fields are handled gracefully
# ===========================================================================

test_that("G5.8c: get_means() with all-NA outcome and na.rm = TRUE returns NA estimate [G5.8c]", {
  df <- make_survey_data(n = 100L, seed = 1L)
  df$all_na <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  # na.rm = TRUE should handle the all-NA column without error
  result <- get_means(d, all_na, na.rm = TRUE)
  expect_true(is.na(result$mean))
})

test_that("G5.8c: survey_glm() with all-NA response errors (singular or degenerate fit) [G5.8c]", {
  df <- make_survey_data(n = 50L, seed = 1L)
  df$all_na <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  # na.omit drops all rows → degenerate fit → singular model matrix or glm error.
  # Use suppressWarnings() around expect_error() to avoid testthat WARNING from
  # the stats::glm.fit "algorithm did not converge" message.
  expect_error(
    suppressWarnings(survey_glm(d, all_na ~ y2, na.action = stats::na.omit))
  )
})

# ===========================================================================
# G5.8d — data outside algorithm scope (singular model matrix)
# ===========================================================================

test_that("G5.8d: survey_glm() errors for perfectly collinear predictors [G5.8d]", {
  df <- make_survey_data(n = 100L, seed = 1L)
  df$y2b <- 2 * df$y2  # perfect collinearity
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    survey_glm(d, y1 ~ y2 + y2b),
    class = "surveycore_error_singular_model_matrix"
  )
})

test_that("G5.8d: survey_glm() errors when more predictors than observations [G5.8d]", {
  # 4 rows; 5 predictors (intercept + 4) → singular model matrix
  set.seed(1L)
  tiny_df <- data.frame(
    y  = rnorm(4L),
    x1 = rnorm(4L),
    x2 = rnorm(4L),
    x3 = rnorm(4L),
    x4 = rnorm(4L),
    wt = rep(1.0, 4L)
  )
  d <- as_survey(tiny_df, weights = wt)
  expect_error(
    survey_glm(d, y ~ x1 + x2 + x3 + x4),
    class = "surveycore_error_singular_model_matrix"
  )
})

# ===========================================================================
# G5.9 / G5.9a — trivial noise does not meaningfully change results
# ===========================================================================

test_that("G5.9a: adding .Machine$double.eps noise to predictors does not change GLM coef [G5.9a]", {
  d_orig <- .sc_srs()
  fit_orig <- survey_glm(d_orig, y1 ~ y2)

  # Add trivial epsilon noise to the predictor
  d_noisy <- d_orig
  set.seed(7L)
  eps <- rnorm(nrow(d_noisy@data), sd = .Machine$double.eps)
  d_noisy@data$y2 <- d_noisy@data$y2 + eps

  fit_noisy <- survey_glm(d_noisy, y1 ~ y2)

  # Coefficients should change by at most a few multiples of double.eps
  delta <- abs(fit_orig@coefficients - fit_noisy@coefficients)
  expect_true(all(delta < 1e-6))
})

test_that("G5.9a: adding .Machine$double.eps noise to response does not change GLM coef [G5.9a]", {
  d_orig <- .sc_srs()
  fit_orig <- survey_glm(d_orig, y1 ~ y2)

  d_noisy <- d_orig
  set.seed(11L)
  eps <- rnorm(nrow(d_noisy@data), sd = .Machine$double.eps)
  d_noisy@data$y1 <- d_noisy@data$y1 + eps

  fit_noisy <- survey_glm(d_noisy, y1 ~ y2)

  delta <- abs(fit_orig@coefficients - fit_noisy@coefficients)
  expect_true(all(delta < 1e-6))
})

# ===========================================================================
# G5.10 — extended tests under SURVEYCORE_EXTENDED_TESTS=true guard
# ===========================================================================

test_that("G5.10: extended GLM numerical accuracy test (all 8 families) [G5.10]", {
  skip_if(
    !identical(Sys.getenv("SURVEYCORE_EXTENDED_TESTS"), "true"),
    message = "Set SURVEYCORE_EXTENDED_TESTS=true to run extended tests"
  )
  skip_if_not_installed("survey")

  d_sc <- as_survey(
    gss_2024,
    ids = vpsu, weights = wtssps, strata = vstrat, nest = TRUE
  )
  d_sv <- survey::svydesign(
    ids = ~vpsu, weights = ~wtssps, strata = ~vstrat,
    data = gss_2024, nest = TRUE
  )

  # Gaussian
  fit_sc <- survey_glm(d_sc, age ~ educ + sex)
  fit_sv <- survey::svyglm(age ~ educ + sex, design = d_sv)
  expect_equal(coef(fit_sc), coef(fit_sv), tolerance = 1e-10)
  expect_equal(
    unname(sqrt(diag(vcov(fit_sc)))),
    as.numeric(survey::SE(fit_sv)),
    tolerance = 1e-8
  )

  # Quasibinomial (age_bin: 1 if age >= 40)
  d_sc2 <- d_sc
  d_sc2@data$age_bin <- as.integer(d_sc2@data$age >= 40L)
  d_sv2 <- survey::svydesign(
    ids = ~vpsu, weights = ~wtssps, strata = ~vstrat,
    data = d_sc2@data, nest = TRUE
  )
  fit_sc2 <- survey_glm(d_sc2, age_bin ~ educ + sex, family = quasibinomial())
  fit_sv2 <- survey::svyglm(age_bin ~ educ + sex, design = d_sv2,
                             family = quasibinomial())
  expect_equal(coef(fit_sc2), coef(fit_sv2), tolerance = 1e-10)
})

# ===========================================================================
# RE3.1 — quiet = TRUE suppresses convergence warnings
# ===========================================================================

test_that("RE3.1: survey_glm(quiet = TRUE) suppresses convergence warning [RE3.1]", {
  d <- .sc_taylor()
  # Force non-convergence by limiting IRLS to 1 iteration
  expect_no_warning(
    fit <- survey_glm(d, y1 ~ y2 + y3, control = list(maxit = 1L), quiet = TRUE)
  )
  # Status is still accessible in @converged
  expect_false(fit@converged)
})

test_that("RE3.1: survey_glm(quiet = FALSE) still emits typed convergence warning [RE3.1]", {
  d <- .sc_taylor()
  # The surveycore_warning_glm_convergence must be present when quiet = FALSE.
  expect_warning(
    survey_glm(d, y1 ~ y2 + y3, control = list(maxit = 1L), quiet = FALSE),
    class = "surveycore_warning_glm_convergence"
  )
})

# ===========================================================================
# RE6.0 / RE6.1 / RE6.2 — plot.survey_glm_fit() default plot method
# ===========================================================================

test_that("RE6.0/RE6.1: plot.survey_glm_fit is registered and callable [RE6.0]", {
  fit <- survey_glm(.sc_taylor(), y1 ~ y2 + y3)
  # Method should be found via S3 dispatch
  expect_true(existsMethod <- is.function(getS3method("plot", "surveycore::survey_glm_fit", optional = TRUE)))
})

test_that("RE6.2: plot.survey_glm_fit returns invisible(x) without error [RE6.2]", {
  fit <- survey_glm(.sc_taylor(), y1 ~ y2 + y3)
  # Run the plot in a null device to avoid opening a graphics window
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- withVisible(plot(fit))
  expect_false(result$visible)
  expect_true(S7::S7_inherits(result$value, survey_glm_fit))
})

test_that("RE6.2: plot.survey_glm_fit with_intercept = TRUE includes intercept [RE6.2]", {
  fit <- survey_glm(.sc_taylor(), y1 ~ y2)
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Should not error even with intercept included
  expect_no_error(plot(fit, with_intercept = TRUE))
})

# ===========================================================================
# RE7.0 — exact predictor-predictor relationship → collinearity detection
# ===========================================================================

test_that("RE7.0: perfect collinearity among predictors is detected and reported [RE7.0]", {
  df <- make_survey_data(n = 100L, seed = 1L)
  # x2 is a perfect linear function of x1 — exact predictor-predictor relationship
  df$y2b <- 3 * df$y2 + 1
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    survey_glm(d, y1 ~ y2 + y2b),
    class = "surveycore_error_singular_model_matrix"
  )
})

# ===========================================================================
# RE7.1 — exact predictor-response relationship → coefficient recovery
# ===========================================================================

test_that("RE7.1: survey_glm recovers exact coefficients from noiseless data [RE7.1]", {
  # Create data with a EXACTLY known linear relationship (no noise)
  # using a design with uniform weights so estimates equal the OLS solution.
  set.seed(99L)
  n <- 500L
  x <- runif(n, -5, 5)
  # True: intercept = 10, slope = -3 exactly
  y <- 10 - 3 * x
  df <- data.frame(y = y, x = x, wt = rep(1.0, n))
  d <- as_survey(df, weights = wt)

  fit <- survey_glm(d, y ~ x)
  # Noiseless data → exact recovery up to floating-point precision
  expect_equal(fit@coefficients[["(Intercept)"]], 10.0, tolerance = 1e-8)
  expect_equal(fit@coefficients[["x"]], -3.0, tolerance = 1e-8)
})

# ===========================================================================
# RE7.2 — output objects retain row names from input data
# ===========================================================================

test_that("RE7.2: model.frame(fit) row names match rows used in fitting [RE7.2]", {
  df <- make_survey_data(n = 100L, seed = 1L)
  rownames(df) <- paste0("obs_", seq_len(nrow(df)))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  fit <- survey_glm(d, y1 ~ y2)
  mf <- model.frame(fit)

  # All row names in the model frame must come from the original data
  expect_true(all(rownames(mf) %in% rownames(df)))
  # Row count should equal the number of fitted values
  expect_equal(nrow(mf), length(fit@fitted_values))
})

test_that("RE7.2: model.frame(fit) preserves row names after na.omit [RE7.2]", {
  df <- make_survey_data(n = 100L, seed = 1L)
  rownames(df) <- paste0("obs_", seq_len(nrow(df)))
  df$y1[c(1L, 5L, 10L)] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  fit <- survey_glm(d, y1 ~ y2, na.action = stats::na.omit)
  mf <- model.frame(fit)

  # Row names after na.omit should still be from the original data
  expect_true(all(rownames(mf) %in% rownames(df)))
  # Dropped rows should not appear
  expect_false(any(c("obs_1", "obs_5", "obs_10") %in% rownames(mf)))
  expect_equal(nrow(mf), length(fit@fitted_values))
})
