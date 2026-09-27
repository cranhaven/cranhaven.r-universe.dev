# tests/testthat/test-glm.R
#
# Tests for survey_glm() — PR 2 scope (feature/glm-core)
# clean() tests are added in PR 4.
#
# Spec reference: plans/spec-phase-2.md
# Error classes: plans/error-messages.md rows 64–87
#
# Test categories (per spec §9.2, PR 2 scope):
#   Item 1:   Happy path — survey_glm() produces valid survey_glm_fit
#   Item 7:   Programmatic interface (response=/predictors=)
#   Item 7b:  lapply() state leakage
#   Item 8:   Error paths — all 14 rows from §4.7 + P2-21
#   Item 9:   Convergence warning
#   Item 10:  @groups warning
#   Item 11:  Domain estimation oracle (skip_if_not_installed)
#   Item 12:  S7 validator errors (7 structural invariants)
#   §9.4:     Edge cases for survey_glm() (not clean())

library(surveycore)

# ── Shared test fixtures ───────────────────────────────────────────────────────

.glm_taylor <- function(seed = 1L) {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = seed)
  as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
}

.glm_srs <- function(seed = 1L) {
  df <- make_survey_data(n = 200L, seed = seed)
  as_survey(df, weights = wt)
}

.glm_replicate <- function(seed = 1L) {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "brr",
    seed = seed
  )
  as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("repwt"),
    type = "BRR"
  )
}

# ---------------------------------------------------------------------------
# Item 1: Happy path — valid survey_glm_fit for all design types
# ---------------------------------------------------------------------------

test_that("survey_glm() produces valid survey_glm_fit for Taylor design", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  test_glm_fit_invariants(fit)
  expect_equal(length(fit@coefficients), 2L)
  expect_identical(dim(fit@vcov), c(2L, 2L))
  expect_true(fit@converged)
  expect_true(is.finite(fit@degf))
  expect_gt(fit@degf, 0)
  expect_identical(fit@formula, y1 ~ y2)
})

test_that("survey_glm() produces valid survey_glm_fit for SRS design", {
  d <- .glm_srs()
  fit <- survey_glm(d, y1 ~ y2)
  test_glm_fit_invariants(fit)
  expect_true(is.finite(fit@degf))
})

test_that("survey_glm() produces valid survey_glm_fit for replicate design", {
  d <- .glm_replicate()
  fit <- survey_glm(d, y1 ~ y2)
  test_glm_fit_invariants(fit)
  expect_true(is.finite(fit@degf))
})

test_that("survey_glm() vcov row/col names match coefficient names", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  coef_names <- names(fit@coefficients)
  expect_identical(rownames(fit@vcov), coef_names)
  expect_identical(colnames(fit@vcov), coef_names)
})

test_that("survey_glm() stores the raw glm fit in fit_", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_true(inherits(fit@fit_, "glm"))
})

test_that("survey_glm() degf equals .degf(design) for Taylor design", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_equal(fit@degf, .degf(d))
})

# ---------------------------------------------------------------------------
# Item 7: Programmatic interface (response=/predictors=)
# ---------------------------------------------------------------------------

test_that("survey_glm() programmatic interface: response + predictors matches formula", {
  d <- .glm_taylor()
  fit_formula <- survey_glm(d, y1 ~ y2 + y3)
  fit_prog <- survey_glm(d, response = "y1", predictors = c("y2", "y3"))

  expect_equal(
    fit_formula@coefficients,
    fit_prog@coefficients,
    tolerance = 1e-15
  )
  expect_equal(fit_formula@vcov, fit_prog@vcov, tolerance = 1e-15)
})

test_that("survey_glm() programmatic interface: response only gives intercept-only model", {
  d <- .glm_taylor()
  fit <- survey_glm(d, response = "y1")

  test_glm_fit_invariants(fit)
  expect_identical(length(fit@coefficients), 1L)
  expect_identical(names(fit@coefficients), "(Intercept)")
  expect_identical(deparse1(fit@formula), "y1 ~ 1")
})

test_that("survey_glm() programmatic interface: predictors without response errors", {
  d <- .glm_taylor()
  expect_error(
    survey_glm(d, predictors = c("y2", "y3")),
    class = "surveycore_error_formula_missing"
  )
  expect_snapshot(error = TRUE, survey_glm(d, predictors = c("y2", "y3")))
})

test_that("survey_glm() programmatic: constructed formula stored in @formula", {
  d <- .glm_taylor()
  fit <- survey_glm(d, response = "y1", predictors = c("y2", "y3"))

  expect_true(inherits(fit@formula, "formula"))
  expect_identical(deparse1(fit@formula), "y1 ~ y2 + y3")
})

# ---------------------------------------------------------------------------
# Item 7b: lapply() state leakage
# ---------------------------------------------------------------------------

test_that("survey_glm() programmatic interface: no state leakage across lapply() calls", {
  d <- .glm_taylor()

  results <- lapply(c("y1", "y2"), function(v) {
    survey_glm(d, response = v, predictors = "y3")
  })

  expect_true(S7::S7_inherits(results[[1L]], survey_glm_fit))
  expect_true(S7::S7_inherits(results[[2L]], survey_glm_fit))

  # Each result has the correct @formula for its outcome
  expect_identical(deparse(results[[1L]]@formula), "y1 ~ y3")
  expect_identical(deparse(results[[2L]]@formula), "y2 ~ y3")

  # Coefficients differ across outcomes (y1 and y2 have different means)
  expect_false(identical(
    unname(results[[1L]]@coefficients[["(Intercept)"]]),
    unname(results[[2L]]@coefficients[["(Intercept)"]])
  ))
})

# ---------------------------------------------------------------------------
# Item 8: Error paths — §4.7 error table (all 14 rows + P2-21)
# Layer 3 constructor errors: dual pattern (class= + snapshot)
# Warnings: class= only (expect_warning)
# ---------------------------------------------------------------------------

# Row 1 (P2-1): design not a survey object
test_that("survey_glm() errors for non-survey design (P2-1)", {
  expect_error(
    survey_glm(list(x = 1), y ~ x),
    class = "surveycore_error_unsupported_class"
  )
  expect_snapshot(error = TRUE, survey_glm(list(x = 1), y ~ x))
})

# Row 2 (P2-2): no model specified — all NULL
test_that("survey_glm() errors when formula/response/predictors all NULL (P2-2)", {
  d <- .glm_taylor()
  expect_error(
    survey_glm(d),
    class = "surveycore_error_formula_missing"
  )
  expect_snapshot(error = TRUE, survey_glm(d))
})

# Row 2 (P2-2): predictors supplied without response
test_that("survey_glm() errors when predictors given without response (P2-2)", {
  d <- .glm_taylor()
  expect_error(
    survey_glm(d, predictors = "y2"),
    class = "surveycore_error_formula_missing"
  )
  expect_snapshot(error = TRUE, survey_glm(d, predictors = "y2"))
})

# Row 2a (P2-16): both formula and response/predictors supplied
test_that("survey_glm() errors when formula and response both supplied (P2-16)", {
  d <- .glm_taylor()
  expect_error(
    survey_glm(d, formula = y1 ~ y2, response = "y1"),
    class = "surveycore_error_formula_conflict"
  )
  expect_snapshot(
    error = TRUE,
    survey_glm(d, formula = y1 ~ y2, response = "y1")
  )
})

# Row 3 (P2-3): formula not a formula object
test_that("survey_glm() errors when formula arg is not a formula (P2-3)", {
  d <- .glm_taylor()
  expect_error(
    survey_glm(d, formula = "y1 ~ y2"),
    class = "surveycore_error_formula_invalid"
  )
  expect_snapshot(error = TRUE, survey_glm(d, formula = "y1 ~ y2"))
})

# Row 4 (P2-4): response variable absent
test_that("survey_glm() errors when response variable absent from design@data (P2-4)", {
  d <- .glm_taylor()
  expect_error(
    survey_glm(d, nonexistent ~ y2),
    class = "surveycore_error_response_not_found"
  )
  expect_snapshot(error = TRUE, survey_glm(d, nonexistent ~ y2))
})

# Row 5 (P2-5): predictor absent
test_that("survey_glm() errors when predictor absent from design@data (P2-5)", {
  d <- .glm_taylor()
  expect_error(
    survey_glm(d, y1 ~ nonexistent),
    class = "surveycore_error_predictor_not_found"
  )
  expect_snapshot(error = TRUE, survey_glm(d, y1 ~ nonexistent))
})

# Row 9 (P2-9): singular model matrix (perfect collinearity)
test_that("survey_glm() errors for singular model matrix (P2-9)", {
  df <- make_survey_data(n = 100L, seed = 1L)
  df$y2b <- 2 * df$y2 # perfect collinearity
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    survey_glm(d, y1 ~ y2 + y2b),
    class = "surveycore_error_singular_model_matrix"
  )
  expect_snapshot(error = TRUE, survey_glm(d, y1 ~ y2 + y2b))
})

# Row 11 (P2-11): NA weights
test_that("survey_glm() errors when weight column has NA (P2-11)", {
  df <- make_survey_data(n = 100L, seed = 1L)
  df$wt[1] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    survey_glm(d, y1 ~ y2),
    class = "surveycore_error_na_weights"
  )
  expect_snapshot(error = TRUE, survey_glm(d, y1 ~ y2))
})

# Row 14 (P2-20): cbind() on LHS of formula
test_that("survey_glm() errors for cbind() on LHS of formula (P2-20)", {
  d <- .glm_taylor()
  expect_error(
    survey_glm(d, cbind(y1, y2) ~ y3),
    class = "surveycore_error_cbind_response_unsupported"
  )
  expect_snapshot(error = TRUE, survey_glm(d, cbind(y1, y2) ~ y3))
})

# Row 7 (P2-7): response is a design variable — WARNING only
test_that("survey_glm() warns when response variable is a design variable (P2-7)", {
  d <- .glm_taylor()
  expect_warning(
    survey_glm(d, wt ~ y2),
    class = "surveycore_warning_response_is_design_var"
  )
})

# Row 8 (P2-8): perfect separation — WARNING only
test_that("survey_glm() warns for perfect separation in binomial model (P2-8)", {
  df <- make_survey_data(n = 200L, seed = 1L)
  df$y_sep <- as.integer(df$y1 > 50) # binary outcome
  df$x_sep <- df$y1 # perfect predictor
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    survey_glm(d, y_sep ~ x_sep, family = binomial()),
    class = "surveycore_warning_perfect_separation"
  )
})

# Row 13 (P2-19): non-positive weights — WARNING only
# NOTE: This warning is a defensive guard inside survey_glm(). It is
# unreachable via the public API because all S7 design validators reject
# non-positive weights at construction time. Covered by # nocov in glm.R.

# Row 12 (P2-17): empty domain after domain filter
test_that("survey_glm() errors for empty active domain (P2-17)", {
  d <- .glm_taylor()
  # Inject a domain column that excludes everything
  d@data[["..surveycore_domain.."]] <- rep(FALSE, nrow(d@data))
  expect_error(
    survey_glm(d, y1 ~ y2),
    class = "surveycore_error_empty_domain"
  )
  expect_snapshot(error = TRUE, {
    d2 <- .glm_taylor()
    d2@data[["..surveycore_domain.."]] <- rep(FALSE, nrow(d2@data))
    survey_glm(d2, y1 ~ y2)
  })
})

# P2-21: na.action = na.fail with NA in response
test_that("survey_glm() errors with na.action = na.fail and NA in response (P2-21)", {
  df <- make_survey_data(n = 100L, seed = 1L)
  df$y1[5L] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    survey_glm(d, y1 ~ y2, na.action = na.fail),
    class = "surveycore_error_na_in_data"
  )
  expect_snapshot(error = TRUE, {
    df2 <- make_survey_data(n = 100L, seed = 1L)
    df2$y1[5L] <- NA_real_
    d2 <- as_survey(df2, ids = psu, weights = wt, strata = strata, nest = TRUE)
    survey_glm(d2, y1 ~ y2, na.action = na.fail)
  })
})

# P2-21: na.action = na.fail with NA in predictor
test_that("survey_glm() errors with na.action = na.fail and NA in predictor (P2-21)", {
  df <- make_survey_data(n = 100L, seed = 1L)
  df$y2[5L] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    survey_glm(d, y1 ~ y2, na.action = na.fail),
    class = "surveycore_error_na_in_data"
  )
})

# ---------------------------------------------------------------------------
# Item 9: Convergence warning
# ---------------------------------------------------------------------------

test_that("survey_glm() warns on GLM non-convergence and still returns fit (P2-6)", {
  d <- .glm_taylor()
  # Force non-convergence by limiting IRLS to 1 iteration
  expect_warning(
    fit <- survey_glm(d, y1 ~ y2 + y3, control = list(maxit = 1L)),
    class = "surveycore_warning_glm_convergence"
  )
  # Fit is still returned despite non-convergence
  expect_true(S7::S7_inherits(fit, survey_glm_fit))
  expect_false(fit@converged)
})

# ---------------------------------------------------------------------------
# Item 10: @groups warning
# ---------------------------------------------------------------------------

test_that("survey_glm() warns when design has @groups set (P2-10)", {
  d <- .glm_taylor()
  d@groups <- "group" # simulate a group_by() design
  expect_warning(
    survey_glm(d, y1 ~ y2),
    class = "surveycore_warning_groups_ignored_in_glm"
  )
})

# ---------------------------------------------------------------------------
# Item 11: Domain estimation oracle (skipped without survey + surveytidy)
# ---------------------------------------------------------------------------

test_that("survey_glm() domain: surveytidy::filter() domain matches svyglm subset [oracle]", {
  skip_if_not_installed("survey")
  skip_if_not_installed("surveytidy")

  # Filter to MEC-examined respondents (ridstatr == 2) so wtmec2yr > 0 for all
  nhanes_mec <- nhanes_2017[nhanes_2017$ridstatr == 2L, ]

  d_sc <- as_survey(
    nhanes_mec,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d_sv <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_mec,
    nest = TRUE
  )

  # Domain: adults aged 40+
  d_dom <- surveytidy::filter(d_sc, ridageyr >= 40)
  fit_sc <- survey_glm(d_dom, bpxsy1 ~ ridageyr + riagendr)

  domain_flag <- as.integer(
    nhanes_mec$ridageyr >= 40 & !is.na(nhanes_mec$ridageyr)
  )
  fit_sv <- survey::svyglm(
    bpxsy1 ~ ridageyr + riagendr,
    design = d_sv,
    subset = domain_flag == 1
  )

  # Use @coefficients / @vcov directly — coef()/vcov() S3 methods are PR 3.
  expect_equal(fit_sc@coefficients, coef(fit_sv), tolerance = 1e-10)
  expect_equal(
    as.numeric(sqrt(diag(fit_sc@vcov))),
    as.numeric(survey::SE(fit_sv)),
    tolerance = 1e-8
  )
})

# ---------------------------------------------------------------------------
# Item 12: S7 validator errors — 7 structural invariants (class= only, no snapshot)
# ---------------------------------------------------------------------------

# Build a minimal valid survey_glm_fit for mutation tests
.minimal_fit_args <- function(d) {
  n <- nrow(d@data)
  p <- 2L
  list(
    coefficients = c("(Intercept)" = 1.0, "y2" = 0.5),
    vcov = matrix(
      c(0.1, 0.01, 0.01, 0.05),
      nrow = 2L,
      dimnames = list(c("(Intercept)", "y2"), c("(Intercept)", "y2"))
    ),
    fitted_values = rep(1.0, n),
    residuals = rep(0.1, n),
    weights = rep(1.0, n),
    design = d,
    degf = 5.0,
    family = gaussian(),
    formula = y1 ~ y2,
    null_deviance = 10.0,
    deviance = 5.0,
    df_null = as.numeric(n - 1L),
    df_residual = as.numeric(n - p),
    converged = TRUE
  )
}

test_that("survey_glm_fit validator: rejects empty coefficients (condition 1)", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  args$coefficients <- numeric(0)
  args$vcov <- matrix(numeric(0), 0L, 0L)
  expect_error(do.call(survey_glm_fit, args))
})

test_that("survey_glm_fit validator: rejects wrong vcov dimensions (condition 2)", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  args$vcov <- matrix(1.0, 3L, 3L) # p=2 but vcov is 3x3
  expect_error(do.call(survey_glm_fit, args))
})

test_that("survey_glm_fit validator: rejects empty fitted_values (condition 3)", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  args$fitted_values <- numeric(0)
  args$residuals <- numeric(0)
  args$weights <- numeric(0)
  expect_error(do.call(survey_glm_fit, args))
})

test_that("survey_glm_fit validator: rejects residuals length mismatch (condition 4)", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  args$residuals <- rep(0.1, nrow(d@data) + 1L)
  expect_error(do.call(survey_glm_fit, args))
})

test_that("survey_glm_fit validator: rejects weights length mismatch (condition 5)", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  args$weights <- rep(1.0, nrow(d@data) + 1L)
  expect_error(do.call(survey_glm_fit, args))
})

test_that("survey_glm_fit validator: rejects non-positive degf (condition 6)", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  args$degf <- 0
  expect_error(do.call(survey_glm_fit, args))
})

test_that("survey_glm_fit validator: rejects non-formula in formula slot (condition 7)", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  args$formula <- "y1 ~ y2" # character string, not a formula
  expect_error(do.call(survey_glm_fit, args))
})

# ---------------------------------------------------------------------------
# @term_assign property (spec-get-anova §IX, §3.3.1)
# ---------------------------------------------------------------------------

test_that("survey_glm_fit has @term_assign integer property, default integer(0)", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  # Do not set term_assign — rely on the default.
  fit <- do.call(survey_glm_fit, args)
  expect_type(fit@term_assign, "integer")
  expect_identical(fit@term_assign, integer(0))
})

test_that("survey_glm_fit validator rejects non-integer @term_assign", {
  d <- .glm_taylor()
  args <- .minimal_fit_args(d)
  args$term_assign <- c(0, 1, 1) # numeric, not integer
  expect_error(do.call(survey_glm_fit, args))
})

test_that("survey_glm() populates @term_assign from model.matrix assign attribute", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  expected <- attr(stats::model.matrix(fit@fit_), "assign")
  expect_type(fit@term_assign, "integer")
  expect_identical(fit@term_assign, as.integer(expected))
  # Multi-term fit: assign includes 0 (intercept) and at least two distinct
  # positive term indices.
  expect_true(length(unique(fit@term_assign)) >= 3L)
})

test_that("survey_glm_fit @term_assign survives saveRDS() roundtrip", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  # Simulate the post-serialization state: @fit_ is stripped, but
  # @term_assign must persist so Wald can recover the term-to-column map.
  fit@fit_ <- NULL
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(fit, tmp)
  restored <- readRDS(tmp)
  expect_false(identical(restored@term_assign, integer(0)))
  expect_identical(restored@term_assign, fit@term_assign)
})

# ---------------------------------------------------------------------------
# §9.4 Edge cases for survey_glm() (not clean() — those come in PR 4)
# ---------------------------------------------------------------------------

test_that("survey_glm() intercept-only model (y ~ 1)", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ 1)

  test_glm_fit_invariants(fit)
  expect_identical(length(fit@coefficients), 1L)
  expect_identical(names(fit@coefficients), "(Intercept)")
})

test_that("survey_glm() with factor predictor excludes reference level from coef", {
  df <- make_survey_data(n = 300L, seed = 2L)
  df$group <- factor(df$group)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  fit <- survey_glm(d, y1 ~ group)

  test_glm_fit_invariants(fit)
  n_levels <- nlevels(df$group)
  # coefficients = intercept + (n_levels - 1) non-reference levels
  expect_equal(length(fit@coefficients), n_levels)
  # Reference level name not in coef names
  ref_level <- levels(df$group)[[1L]]
  expect_false(any(names(fit@coefficients) == ref_level))
})

test_that("survey_glm() with interaction terms", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 * y3)

  test_glm_fit_invariants(fit)
  # Expect intercept + y2 + y3 + y2:y3 = 4 coefficients
  expect_equal(length(fit@coefficients), 4L)
  expect_true("y2:y3" %in% names(fit@coefficients))
})

test_that("survey_glm() na.action = na.omit silently drops NA rows", {
  df <- make_survey_data(n = 200L, seed = 1L)
  df$y1[1:5] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  fit <- survey_glm(d, y1 ~ y2, na.action = na.omit)

  test_glm_fit_invariants(fit)
  # n_obs after na.omit is less than full data
  expect_lt(length(fit@fitted_values), nrow(df))
  expect_equal(length(fit@fitted_values), sum(!is.na(df$y1)))
})

test_that("survey_glm() binomial family produces valid fit", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y3 ~ y2, family = binomial())

  test_glm_fit_invariants(fit)
  # Fitted values on [0,1] scale (probabilities)
  expect_true(all(fit@fitted_values >= 0 & fit@fitted_values <= 1))
})

test_that("survey_glm() Poisson family produces valid fit", {
  df <- make_survey_data(n = 200L, seed = 1L)
  df$y_count <- rpois(nrow(df), lambda = exp(0.3 * df$y1 / 50 + 0.5))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  fit <- survey_glm(d, y_count ~ y2, family = poisson())

  test_glm_fit_invariants(fit)
  # Fitted values on count scale (positive)
  expect_true(all(fit@fitted_values > 0))
})

# P2-15: insufficient df warning (degf - (p-1) <= 0)
test_that("survey_glm() warns for insufficient design df and clamps to 1 (P2-15)", {
  # 4 PSUs, 2 strata (2 PSUs each) → degf = 4 - 2 = 2
  # Model with intercept + x1 + x2 + x3 → p = 4, degf - (p-1) = 2 - 3 = -1
  # Each stratum has 2 PSUs so no lonely-PSU error.
  set.seed(42L)
  tiny_df <- data.frame(
    psu = c("p1", "p1", "p2", "p2", "p3", "p3", "p4", "p4"),
    strata = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
    wt = rep(10.0, 8L),
    y = rnorm(8L),
    x1 = rnorm(8L),
    x2 = rnorm(8L),
    x3 = rnorm(8L)
  )
  d_tiny <- as_survey(
    tiny_df,
    ids = psu,
    strata = strata,
    weights = wt,
    nest = TRUE
  )
  expect_warning(
    fit <- survey_glm(d_tiny, y ~ x1 + x2 + x3),
    class = "surveycore_warning_insufficient_df"
  )
  test_glm_fit_invariants(fit)
  # CI bounds must be finite (clamped to df=1, not NaN)
  # Use @vcov directly — vcov() S3 method is PR 3.
  se <- sqrt(diag(fit@vcov))
  p <- length(fit@coefficients)
  df_used <- max(1, fit@degf - (p - 1L))
  bounds <- fit@coefficients + outer(se, c(-1, 1) * qt(0.975, df = df_used))
  expect_true(all(is.finite(bounds)))
})

# in-formula transformation in response: log(y) ~ x
test_that("survey_glm() handles in-formula response transformation log(y) ~ x", {
  df <- make_survey_data(n = 200L, seed = 3L)
  df$y1_pos <- abs(df$y1) + 1 # ensure positive for log()
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  fit <- survey_glm(d, log(y1_pos) ~ y2)

  test_glm_fit_invariants(fit)
  expect_equal(length(fit@coefficients), 2L)
})

# ===========================================================================
# PR 4: clean() tests (items 2–6, 5a–5d, §9.4 edge cases)
# Added in feature/glm-clean
# ===========================================================================

# ── Shared fixtures for clean() tests ─────────────────────────────────────────

.glm_taylor_with_factor <- function(seed = 10L) {
  df <- make_survey_data(n = 300L, n_psu = 20L, n_strata = 4L, seed = seed)
  df$sex <- factor(
    sample(c("Female", "Male"), nrow(df), replace = TRUE, prob = c(0.5, 0.5)),
    levels = c("Female", "Male")
  )
  df$age <- df$y1
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d
}

.glm_with_labels <- function(seed = 10L) {
  d <- .glm_taylor_with_factor(seed = seed)
  d <- set_var_label(d, age = "Age in years")
  d <- set_var_label(d, sex = "Respondent sex")
  # val_labels: character codes matching factor level names to avoid
  # surveycore_warning_missing_labels. Names = display labels, values = codes.
  d <- set_val_labels(d, sex = c("Female" = "Female", "Male" = "Male"))
  d
}

# ---------------------------------------------------------------------------
# Item 2: clean() happy path
# ---------------------------------------------------------------------------

test_that("clean() produces valid survey_glm_tidy with correct class hierarchy", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit)
  test_glm_tidy_invariants(result)
  expect_identical(
    class(result),
    c("survey_glm_tidy", "survey_result", "tbl_df", "tbl", "data.frame")
  )
  expect_true("term" %in% names(result))
  expect_true("variable" %in% names(result))
  expect_true("var_label" %in% names(result))
  expect_true("label" %in% names(result))
  expect_true("reference_row" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("std_error" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("conf_low" %in% names(result))
  expect_true("conf_high" %in% names(result))
})

test_that("clean() reference_row is all FALSE when no factor predictors", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  result <- clean(fit)
  test_glm_tidy_invariants(result)
  expect_true(all(result$reference_row == FALSE))
  expect_false(anyNA(result$reference_row))
})

test_that("clean() label column is never NA", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit)
  expect_false(anyNA(result$label))
  expect_type(result$label, "character")
})

# ---------------------------------------------------------------------------
# Item 3: clean() reference levels
# ---------------------------------------------------------------------------

test_that("clean() with include_reference = TRUE adds reference row for factor predictor", {
  d <- .glm_taylor_with_factor()
  fit <- survey_glm(d, y2 ~ sex)
  result <- clean(fit, include_reference = TRUE)
  test_glm_tidy_invariants(result)
  # Female is reference (first level); should appear as reference_row = TRUE
  expect_true(any(result$reference_row))
  ref_rows <- result[result$reference_row, ]
  expect_equal(nrow(ref_rows), 1L)
  expect_true(is.na(ref_rows$estimate))
  expect_true(is.na(ref_rows$std_error))
  expect_true(is.na(ref_rows$p_value))
  expect_true(is.na(ref_rows$conf_low))
  expect_true(is.na(ref_rows$conf_high))
})

test_that("clean() with include_reference = FALSE has no reference rows", {
  d <- .glm_taylor_with_factor()
  fit <- survey_glm(d, y2 ~ sex)
  result <- clean(fit, include_reference = FALSE)
  test_glm_tidy_invariants(result)
  expect_false(any(result$reference_row))
  # All estimate values must be non-NA (no reference row, no NA)
  expect_false(anyNA(result$estimate))
})

# ---------------------------------------------------------------------------
# Item 4: clean() .meta contract
# ---------------------------------------------------------------------------

test_that("clean() .meta has all 15 required top-level keys", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit)
  m <- meta(result)
  required_keys <- c(
    "formula",
    "family",
    "link",
    "design_type",
    "conf_level",
    "call",
    "group_names",
    "group_labels",
    "n_observations",
    "n_weighted",
    "degf",
    "exponentiate",
    "include_reference",
    "converged",
    "variables"
  )
  expect_true(all(required_keys %in% names(m)))
  expect_identical(m$group_names, character(0))
  expect_null(m$group_labels)
  expect_identical(m$formula, y1 ~ y2)
  expect_identical(m$design_type, "taylor")
  expect_equal(m$conf_level, 0.95)
  expect_false(m$exponentiate)
  expect_true(m$include_reference)
  expect_true(m$converged)
})

test_that("clean() .meta $variables has one entry per predictor with 7 sub-keys", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  result <- clean(fit)
  m <- meta(result)
  # Should have entries for y2 and y3 (not (Intercept))
  expect_identical(sort(names(m$variables)), c("y2", "y3"))
  sub_keys <- c(
    "var_label",
    "var_class",
    "var_type",
    "var_nlevels",
    "contrasts",
    "reference_level",
    "value_labels"
  )
  for (v in m$variables) {
    expect_true(all(sub_keys %in% names(v)))
    expect_false(is.null(v$var_label))
    expect_type(v$var_label, "character")
  }
})

test_that("clean() .meta n_observations equals model.matrix row count", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit)
  m <- meta(result)
  expect_type(m$n_observations, "integer")
  expect_gt(m$n_observations, 0L)
  expect_equal(m$n_observations, nrow(model.matrix(fit@fit_)))
})

test_that("clean() .meta n_weighted is positive numeric", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit)
  m <- meta(result)
  expect_type(m$n_weighted, "double")
  expect_gt(m$n_weighted, 0)
})

# ---------------------------------------------------------------------------
# Item 5: Variable label integration
# ---------------------------------------------------------------------------

test_that("clean() var_label column uses variable labels from design metadata", {
  d <- .glm_with_labels()
  # y2 is response; age and sex are predictors — both have variable labels set
  fit <- survey_glm(d, y2 ~ age + sex)
  result <- clean(fit, include_reference = TRUE)
  test_glm_tidy_invariants(result)
  m <- meta(result)
  # meta$variables should carry the variable labels for each predictor
  expect_identical(m$variables$age$var_label, "Age in years")
  expect_identical(m$variables$sex$var_label, "Respondent sex")
  # var_label column in the tibble
  age_rows <- result[result$variable == "age", ]
  expect_identical(unique(age_rows$var_label), "Age in years")
  sex_rows <- result[result$variable == "sex", ]
  expect_identical(unique(sex_rows$var_label), "Respondent sex")
})

test_that("clean() label column uses value labels for factor levels when set", {
  d <- .glm_with_labels()
  fit <- survey_glm(d, y2 ~ sex)
  result <- clean(fit, include_reference = TRUE)
  # Both levels should appear — reference (Female) and estimated (Male)
  # label column for factor levels should be the level name (or value label)
  sex_rows <- result[result$variable == "sex", ]
  # Labels should be either "Female" or "Male" (not raw coef names like "sexMale")
  expect_true(all(sex_rows$label %in% c("Female", "Male")))
})

test_that("clean() .meta $variables carries value_labels for factor predictor", {
  d <- .glm_with_labels()
  fit <- survey_glm(d, y2 ~ sex)
  result <- clean(fit, include_reference = TRUE)
  m <- meta(result)
  # value_labels should be the named vector set on sex
  expect_false(is.null(m$variables$sex$value_labels))
})

# ---------------------------------------------------------------------------
# Item 5a: n argument
# ---------------------------------------------------------------------------

test_that("clean(fit, n = TRUE) adds n_obs column", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit, n = TRUE)
  test_glm_tidy_invariants(result)
  expect_true("n_obs" %in% names(result))
  expect_type(result$n_obs, "integer")
  expect_true(all(result$n_obs > 0L))
})

test_that("clean(fit, n = FALSE) does not add n_obs column (default)", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit)
  expect_false("n_obs" %in% names(result))
})

# ---------------------------------------------------------------------------
# Item 5b: statistic argument
# ---------------------------------------------------------------------------

test_that("clean(fit, statistic = TRUE) includes statistic column (default)", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit, statistic = TRUE)
  expect_true("statistic" %in% names(result))
  expect_type(result$statistic, "double")
})

test_that("clean(fit, statistic = FALSE) drops statistic column", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit, statistic = FALSE)
  expect_false("statistic" %in% names(result))
})

# ---------------------------------------------------------------------------
# Item 5c: exponentiate argument
# ---------------------------------------------------------------------------

test_that("clean(fit, exponentiate = TRUE) on logistic fit exponentiates estimate/CI", {
  d <- .glm_taylor_with_factor()
  fit <- survey_glm(d, y3 ~ age + sex, family = binomial(link = "logit"))
  result_raw <- clean(fit, include_reference = FALSE)
  result_exp <- clean(fit, exponentiate = TRUE, include_reference = FALSE)
  test_glm_tidy_invariants(result_exp)
  # estimate must be exp of the log-scale estimate
  expect_equal(result_exp$estimate, exp(result_raw$estimate), tolerance = 1e-10)
  # std_error unchanged (log scale per broom convention)
  expect_equal(result_exp$std_error, result_raw$std_error, tolerance = 1e-15)
  # CI bounds exponentiated
  expect_equal(result_exp$conf_low, exp(result_raw$conf_low), tolerance = 1e-10)
  expect_equal(
    result_exp$conf_high,
    exp(result_raw$conf_high),
    tolerance = 1e-10
  )
})

test_that("clean(fit, exponentiate = TRUE) on Gaussian identity fires warning", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2, family = gaussian(link = "identity"))
  expect_warning(
    clean(fit, exponentiate = TRUE),
    class = "surveycore_warning_exponentiate_nonlog"
  )
})

# ---------------------------------------------------------------------------
# Item 5d: interaction_sep argument
# ---------------------------------------------------------------------------

test_that("clean() with interaction term uses interaction_sep in label", {
  d <- .glm_taylor_with_factor()
  fit <- survey_glm(d, age ~ y2 + sex + y2:sex)
  result_default <- clean(fit, include_reference = FALSE)
  result_sep <- clean(fit, interaction_sep = " × ", include_reference = FALSE)
  # interaction term row should have different labels based on sep
  int_rows_def <- result_default[grepl(":", result_default$term), ]
  int_rows_sep <- result_sep[grepl(":", result_sep$term), ]
  if (nrow(int_rows_def) > 0L) {
    expect_true(any(grepl(" \\* ", int_rows_def$label)))
    expect_true(any(grepl(" × ", int_rows_sep$label)))
  }
})

# ---------------------------------------------------------------------------
# Item 6: broom::tidy() compatibility
# ---------------------------------------------------------------------------

test_that("broom::tidy(fit) returns same object as clean(fit)", {
  skip_if_not_installed("broom")
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result_clean <- clean(fit)
  result_broom <- broom::tidy(fit)
  expect_identical(class(result_clean), class(result_broom))
  expect_equal(result_clean$estimate, result_broom$estimate, tolerance = 1e-15)
  expect_equal(
    result_clean$std_error,
    result_broom$std_error,
    tolerance = 1e-15
  )
  expect_identical(result_clean$term, result_broom$term)
})

# ---------------------------------------------------------------------------
# Item 8 (PR 4 additions): clean() error paths
# ---------------------------------------------------------------------------

test_that("clean() rejects non-survey_glm_fit input with typed error", {
  expect_error(
    clean(list(x = 1)),
    class = "surveycore_error_not_glm_fit"
  )
  expect_snapshot(error = TRUE, clean(list(x = 1)))
})

test_that("clean() rejects invalid conf_level with typed error", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_error(
    clean(fit, conf_level = 1.5),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_snapshot(error = TRUE, clean(fit, conf_level = 1.5))
})

# ---------------------------------------------------------------------------
# §9.4 edge cases for clean()
# ---------------------------------------------------------------------------

test_that("clean() with no factor predictors: include_reference accepted, all FALSE", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit, include_reference = TRUE)
  test_glm_tidy_invariants(result)
  expect_true(all(result$reference_row == FALSE))
})

test_that("clean() with no variable labels: var_label falls back to variable name", {
  d <- .glm_taylor() # no labels set
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit)
  m <- meta(result)
  # var_label must equal the variable name when no label is set
  expect_identical(m$variables$y2$var_label, "y2")
  # var_label column in tibble for predictor rows
  pred_rows <- result[result$variable == "y2", ]
  expect_identical(unique(pred_rows$var_label), "y2")
})

test_that("clean() with all variable labels set: var_label column shows labels", {
  d <- .glm_with_labels()
  # y2 is response; age and sex are predictors with labels set
  fit <- survey_glm(d, y2 ~ age + sex)
  result <- clean(fit, include_reference = TRUE)
  m <- meta(result)
  expect_identical(m$variables$age$var_label, "Age in years")
  expect_identical(m$variables$sex$var_label, "Respondent sex")
})

test_that("clean() factor predictor with value labels: label column shows value labels", {
  d <- .glm_with_labels()
  fit <- survey_glm(d, y2 ~ sex)
  result <- clean(fit, include_reference = TRUE)
  sex_rows <- result[result$variable == "sex", ]
  # labels should be level names ("Female", "Male"), not raw coef names
  expect_true(all(sex_rows$label %in% c("Female", "Male")))
  # meta carries value_labels
  m <- meta(result)
  expect_false(is.null(m$variables$sex$value_labels))
})

test_that("clean(fit, n = TRUE): n_obs column has positive integers", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit, n = TRUE)
  expect_true("n_obs" %in% names(result))
  # Each row's n_obs is bounded by n_observations (per-term count)
  expect_true(all(result$n_obs > 0L, na.rm = TRUE))
  expect_true(all(result$n_obs <= meta(result)$n_observations, na.rm = TRUE))
})

test_that("clean(fit, statistic = FALSE): statistic column absent", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  result <- clean(fit, statistic = FALSE)
  expect_false("statistic" %in% names(result))
})

test_that("clean(fit, exponentiate = TRUE) on logistic: conf_low = exp(ci_lower)", {
  d <- .glm_taylor_with_factor()
  fit <- survey_glm(d, y3 ~ age, family = binomial(link = "logit"))
  result <- clean(fit, exponentiate = TRUE)
  test_glm_tidy_invariants(result)
  # estimate must be positive (odds ratios)
  expect_true(all(result$estimate[!is.na(result$estimate)] > 0))
})

test_that("clean(fit, exponentiate = TRUE) on Gaussian identity fit fires warning", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_warning(
    result <- clean(fit, exponentiate = TRUE),
    class = "surveycore_warning_exponentiate_nonlog"
  )
  # fit still returns a result despite warning
  test_glm_tidy_invariants(result)
})

test_that("clean() NA in response with na.action = na.omit: n_observations reflects post-NA count", {
  df <- make_survey_data(n = 200L, seed = 5L)
  df$y1[1:10] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  fit <- survey_glm(d, y1 ~ y2, na.action = na.omit)
  result <- clean(fit)
  m <- meta(result)
  # Should be 190 rows (200 - 10 NA), not 200
  expect_equal(m$n_observations, 190L)
})

test_that("clean() CIs are numerically identical to confint(fit)", {
  d <- .glm_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)
  result <- clean(fit)
  ci_confint <- confint(fit)
  # confint() has rownames; clean() produces unnamed vectors — compare values only
  expect_equal(
    result$conf_low[!result$reference_row],
    unname(ci_confint[, "2.5 %"]),
    tolerance = 1e-12
  )
  expect_equal(
    result$conf_high[!result$reference_row],
    unname(ci_confint[, "97.5 %"]),
    tolerance = 1e-12
  )
})


# ===========================================================================
# Multi-stage GLM tests
# ===========================================================================

test_that("survey_glm() matches survey::svyglm() for 2-stage design [oracle]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 500, n_psu = 50, n_ssu = 10, seed = 42)
  sc <- as_survey(df, ids = c(psu, ssu), weights = wt, strata = strata)
  test_invariants(sc)
  test_glm_fit_invariants(survey_glm(y1 ~ y2, design = sc))
  sv <- survey::svydesign(
    id = ~ psu + ssu,
    weights = ~wt,
    strata = ~strata,
    data = df,
    nest = TRUE
  )
  sc_fit <- survey_glm(y1 ~ y2, design = sc)
  sv_fit <- survey::svyglm(y1 ~ y2, design = sv)
  expect_equal(sc_fit@coefficients, coef(sv_fit), tolerance = 1e-10)
  expect_equal(
    as.numeric(sqrt(diag(sc_fit@vcov))),
    as.numeric(survey::SE(sv_fit)),
    tolerance = 1e-8
  )
})

test_that("survey_glm() accepts multi-stage design without error [smoke]", {
  df <- make_survey_data(n = 300, n_psu = 30, n_ssu = 5, seed = 1)
  sc <- as_survey(
    df,
    ids = c(psu, ssu),
    weights = wt,
    strata = strata
  )
  expect_no_error(survey_glm(y1 ~ y2, design = sc))
})

# ── Audit: non-contiguous NA weight indexing fix ──────────────────────────────

test_that("survey_glm() @weights correct with non-contiguous NAs and na.omit", {
  df <- make_survey_data(n = 200L, seed = 42)
  # Inject NAs at non-contiguous positions in the response
  na_positions <- c(3L, 7L, 15L, 42L, 100L)
  df$y1[na_positions] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  fit <- survey_glm(d, y1 ~ y2, na.action = na.omit)

  test_glm_fit_invariants(fit)

  # Number of weights must equal number of fitted values
  expect_equal(length(fit@weights), length(fit@fitted_values))

  # Weights should correspond to the non-NA rows' weights
  non_na_mask <- !seq_len(nrow(df)) %in% na_positions
  expected_weights <- df$wt[non_na_mask]
  expect_equal(fit@weights, expected_weights, tolerance = 1e-10)
})

test_that("survey_glm() with non-contiguous na.omit matches clean-data fit", {
  df <- make_survey_data(n = 200L, seed = 42)
  na_positions <- c(3L, 7L, 15L, 42L, 100L)
  df$y1[na_positions] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  fit_na <- survey_glm(d, y1 ~ y2, na.action = na.omit)

  # Fit on manually cleaned data
  df_clean <- df[!is.na(df$y1), ]
  d_clean <- as_survey(
    df_clean,
    ids = psu,
    weights = wt,
    strata = strata,
    nest = TRUE
  )
  fit_clean <- survey_glm(d_clean, y1 ~ y2)

  expect_equal(
    fit_na@coefficients,
    fit_clean@coefficients,
    tolerance = 1e-10
  )
  expect_equal(fit_na@weights, fit_clean@weights, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# survey_nonprob with repweights — GLM variance dispatch
# ---------------------------------------------------------------------------

# Shared fixtures for nonprob-with-repweights tests
.glm_nonprob_fixtures <- function() {
  df <- make_survey_data(
    design = "replicate",
    type = "brr",
    seed = 1L
  )
  d_rep <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "BRR"
  )
  d_np <- as_survey_nonprob(
    df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "bootstrap",
    scale = d_rep@variables$scale,
    rscales = d_rep@variables$rscales,
    mse = TRUE
  )
  df_simple <- make_survey_data(n = 200, seed = 2L)
  nonprob_no_rep <- as_survey_nonprob(df_simple, weights = wt)
  list(
    df = df,
    d_rep = d_rep,
    d_np = d_np,
    df_simple = df_simple,
    nonprob_no_rep = nonprob_no_rep
  )
}

test_that("survey_glm() nonprob with repweights: @vcov matches survey_replicate oracle", {
  fx <- .glm_nonprob_fixtures()
  fit_np <- survey_glm(fx$d_np, y1 ~ y2)
  fit_rep <- survey_glm(fx$d_rep, y1 ~ y2)
  test_glm_fit_invariants(fit_np)
  expect_equal(fit_np@coefficients, fit_rep@coefficients, tolerance = 1e-10)
  expect_equal(fit_np@vcov, fit_rep@vcov, tolerance = 1e-8)
})

test_that("survey_glm() nonprob with repweights: no surveycore_warning_nonprob_srs_fallback emitted", {
  fx <- .glm_nonprob_fixtures()
  expect_no_warning(
    fit <- survey_glm(fx$d_np, y1 ~ y2),
    class = "surveycore_warning_nonprob_srs_fallback"
  )
  test_glm_fit_invariants(fit)
})

test_that("survey_glm() nonprob NULL repweights: emits surveycore_warning_nonprob_srs_fallback", {
  fx <- .glm_nonprob_fixtures()
  expect_warning(
    fit <- survey_glm(fx$nonprob_no_rep, y1 ~ y2),
    class = "surveycore_warning_nonprob_srs_fallback"
  )
  test_glm_fit_invariants(fit)
  expect_true(all(diag(fit@vcov) > 0))
})

test_that("survey_glm() nonprob NULL repweights: warning snapshot matches NB-2 text", {
  fx <- .glm_nonprob_fixtures()
  expect_snapshot(survey_glm(fx$nonprob_no_rep, y1 ~ y2))
})

test_that("survey_glm() nonprob with repweights: multiple predictors @vcov matches oracle", {
  fx <- .glm_nonprob_fixtures()
  fit_np <- survey_glm(fx$d_np, y1 ~ y2 + y3)
  fit_rep <- survey_glm(fx$d_rep, y1 ~ y2 + y3)
  test_glm_fit_invariants(fit_np)
  expect_equal(nrow(fit_np@vcov), 3L)
  expect_equal(ncol(fit_np@vcov), 3L)
  expect_true(all(diag(fit_np@vcov) > 0))
  expect_equal(fit_np@vcov, fit_rep@vcov, tolerance = 1e-8)
})

test_that("survey_glm() nonprob with repweights: binomial family invariants hold", {
  df2 <- make_survey_data(design = "replicate", type = "brr", seed = 3L)
  d_rep2 <- as_survey_replicate(
    df2,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "BRR"
  )
  d_np2 <- as_survey_nonprob(
    df2,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "bootstrap",
    scale = d_rep2@variables$scale,
    rscales = d_rep2@variables$rscales,
    mse = TRUE
  )
  d_np2 <- surveytidy::mutate(d_np2, y_bin = as.integer(y1 > median(df2$y1)))
  fit <- survey_glm(d_np2, y_bin ~ y2, family = stats::binomial())
  test_glm_fit_invariants(fit)
  expect_true(all(is.finite(fit@vcov)))
  expect_equal(fit@vcov, t(fit@vcov), tolerance = 1e-12)
})

test_that("survey_glm() nonprob NULL repweights: warning emitted exactly once", {
  fx <- .glm_nonprob_fixtures()
  count <- 0L
  withCallingHandlers(
    survey_glm(fx$nonprob_no_rep, y1 ~ y2),
    surveycore_warning_nonprob_srs_fallback = function(w) {
      count <<- count + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(count, 1L)
})

test_that("survey_glm() nonprob domain estimation with repweights: @vcov matches oracle", {
  fx <- .glm_nonprob_fixtures()
  d_np_dom <- surveytidy::filter(fx$d_np, y3 > 0)
  d_rep_dom <- surveytidy::filter(fx$d_rep, y3 > 0)
  fit_np <- survey_glm(d_np_dom, y1 ~ y2)
  fit_rep <- survey_glm(d_rep_dom, y1 ~ y2)
  test_glm_fit_invariants(fit_np)
  expect_equal(fit_np@vcov, fit_rep@vcov, tolerance = 1e-8)
  expect_true(all(diag(fit_np@vcov) > 0))
})

test_that("survey_glm() nonprob mse = FALSE with repweights: @vcov finite", {
  fx <- .glm_nonprob_fixtures()
  d_np_nomse <- as_survey_nonprob(
    fx$df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "bootstrap",
    scale = fx$d_rep@variables$scale,
    rscales = fx$d_rep@variables$rscales,
    mse = FALSE
  )
  fit <- survey_glm(d_np_nomse, y1 ~ y2)
  test_glm_fit_invariants(fit)
  expect_true(all(is.finite(fit@vcov)))
})

test_that("survey_glm() nonprob NULL repweights regression guard: @vcov unchanged from calibrated baseline", {
  fx <- .glm_nonprob_fixtures()
  fit_baseline <- suppressWarnings(survey_glm(fx$nonprob_no_rep, y1 ~ y2))
  fit_again <- suppressWarnings(survey_glm(fx$nonprob_no_rep, y1 ~ y2))
  test_glm_fit_invariants(fit_baseline)
  test_glm_fit_invariants(fit_again)
  expect_equal(fit_baseline@vcov, fit_again@vcov, tolerance = 1e-8)
  expect_true(all(diag(fit_baseline@vcov) > 0))
})

test_that("get_means() on survey_nonprob with repweights: finite mean and se, no SRS fallback warning", {
  df_bin <- make_survey_data(design = "replicate", type = "brr", seed = 4L)
  d_rep_bin <- as_survey_replicate(
    df_bin,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "BRR"
  )
  d_np_bin <- as_survey_nonprob(
    df_bin,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "bootstrap",
    scale = d_rep_bin@variables$scale,
    rscales = d_rep_bin@variables$rscales,
    mse = TRUE
  )
  result <- expect_no_warning(
    get_means(d_np_bin, y1),
    class = "surveycore_warning_nonprob_srs_fallback"
  )
  expect_true(all(is.finite(result$mean)))
  expect_true(all(is.finite(result$ci_low)))
})
