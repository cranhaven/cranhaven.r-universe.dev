# tests/testthat/test-glm-anova-dispatch.R
#
# Polymorphic-input dispatch for get_anova(). The kernel remains tested in
# test-glm-anova.R; this file exercises the new `object` parameter and the
# list / design / formula dispatch branches introduced by the
# get-anova-polymorphic feature.
#
# Plan: plans/impl-get-anova-polymorphic.md
# Error classes: A-21..A-25 in plans/error-messages.md

library(surveycore)

# ── Shared fixtures ──────────────────────────────────────────────────────────

.disp_taylor <- function(seed = 1L, n = 300L) {
  df <- make_survey_data(n = n, n_psu = 30L, n_strata = 5L, seed = seed)
  df$grp <- factor(df$group)
  as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
}

# ═══════════════════════════════════════════════════════════════════════════
# Cycle A — list-of-fits dispatch
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_anova(list(fit1, fit2)) returns a 1-row survey_anova tibble", {
  d <- .disp_taylor()
  fit_s <- survey_glm(d, y1 ~ y2)
  fit_b <- survey_glm(d, y1 ~ y2 + y3)

  r <- get_anova(list(fit_s, fit_b))

  expect_s3_class(r, "survey_anova")
  expect_equal(nrow(r), 1L)
  expect_identical(
    names(r),
    c("term", "statistic", "df", "ddf", "deff", "p_value", "stars")
  )
  expect_true(is.finite(r$statistic))
  expect_true(r$p_value >= 0 && r$p_value <= 1)
})

test_that("get_anova(list(fit1, fit2)) matches S3 anova(fit1, fit2)", {
  d <- .disp_taylor()
  fit_s <- survey_glm(d, y1 ~ y2)
  fit_b <- survey_glm(d, y1 ~ y2 + y3)

  r_list <- get_anova(list(fit_s, fit_b))
  r_s3 <- anova(fit_s, fit_b)

  expect_equal(r_list$statistic, r_s3$statistic, tolerance = 1e-10)
  expect_equal(r_list$p_value, r_s3$p_value, tolerance = 1e-8)
  expect_identical(r_list$term, r_s3$term)
})

test_that("get_anova(list(fit1, fit2, fit3)) returns 2 chained rows", {
  d <- .disp_taylor()
  fit_1 <- survey_glm(d, y1 ~ y2)
  fit_2 <- survey_glm(d, y1 ~ y2 + y3)
  fit_3 <- survey_glm(d, y1 ~ y2 + y3 + grp)

  r <- get_anova(list(fit_1, fit_2, fit_3))

  expect_s3_class(r, "survey_anova")
  expect_equal(nrow(r), 2L)

  # Row 1: fit_1 vs fit_2; Row 2: fit_2 vs fit_3. Each chained row must equal
  # its standalone pair. Unclass to drop the column-level `label` attribute
  # before comparing (attr survives `$` but is dropped by `[[`).
  r_pair_12 <- get_anova(list(fit_1, fit_2))
  r_pair_23 <- get_anova(list(fit_2, fit_3))
  expect_equal(r$statistic[[1L]], r_pair_12$statistic[[1L]], tolerance = 1e-10)
  expect_equal(r$statistic[[2L]], r_pair_23$statistic[[1L]], tolerance = 1e-10)
})

test_that("get_anova(list of k = 4 fits) returns 3 chained rows", {
  d <- .disp_taylor()
  # Add a non-collinear 4th predictor so the k=4 model isn't singular.
  set.seed(1L)
  d@data$extra <- rnorm(nrow(d@data))
  fit_1 <- survey_glm(d, y1 ~ y2)
  fit_2 <- survey_glm(d, y1 ~ y2 + y3)
  fit_3 <- survey_glm(d, y1 ~ y2 + y3 + grp)
  fit_4 <- survey_glm(d, y1 ~ y2 + y3 + grp + extra)

  r <- get_anova(list(fit_1, fit_2, fit_3, fit_4))
  expect_equal(nrow(r), 3L)
})

test_that("get_anova(list(fit)) unwraps silently to single-model anova", {
  d <- .disp_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)

  r_direct <- get_anova(fit)
  r_list <- get_anova(list(fit))

  expect_s3_class(r_list, "survey_anova")
  expect_equal(r_list$statistic, r_direct$statistic, tolerance = 1e-10)
  expect_equal(r_list$p_value, r_direct$p_value, tolerance = 1e-8)
  expect_identical(r_list$term, r_direct$term)
})

test_that("get_anova(list()) errors with surveycore_error_anova_empty_model_list", {
  expect_error(
    get_anova(list()),
    class = "surveycore_error_anova_empty_model_list"
  )
})

test_that("get_anova(list of mixed classes) errors with surveycore_error_anova_object_invalid", {
  d <- .disp_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_error(
    get_anova(list(fit, "not a fit", fit)),
    class = "surveycore_error_anova_object_invalid"
  )
})

# ═══════════════════════════════════════════════════════════════════════════
# Cycle B — design + formula dispatch
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_anova(design, y ~ x1 + x2) equals get_anova(survey_glm(design, y ~ x1 + x2))", {
  d <- .disp_taylor()
  fit <- survey_glm(d, y1 ~ y2 + y3)

  r_oracle <- get_anova(fit)
  r_design <- get_anova(d, y1 ~ y2 + y3)

  expect_s3_class(r_design, "survey_anova")
  expect_equal(r_design$statistic, r_oracle$statistic, tolerance = 1e-10)
  expect_equal(r_design$p_value, r_oracle$p_value, tolerance = 1e-8)
  expect_identical(r_design$term, r_oracle$term)
})

test_that("get_anova(design, response = 'y', predictors = c('x1', 'x2')) matches formula path", {
  d <- .disp_taylor()
  r_formula <- get_anova(d, y1 ~ y2 + y3)
  r_rp <- get_anova(d, response = "y1", predictors = c("y2", "y3"))

  expect_s3_class(r_rp, "survey_anova")
  expect_equal(r_rp$statistic, r_formula$statistic, tolerance = 1e-10)
  expect_identical(r_rp$term, r_formula$term)
})

test_that("get_anova(design) without formula errors with surveycore_error_anova_formula_missing", {
  d <- .disp_taylor()
  expect_error(
    get_anova(d),
    class = "surveycore_error_anova_formula_missing"
  )
})

test_that("get_anova(design, ...) forwards additional args to survey_glm()", {
  d <- .disp_taylor()
  # Use quiet = TRUE (a known survey_glm arg) to confirm dots reach survey_glm
  # without raising check_dots_empty.
  expect_no_error(get_anova(d, y1 ~ y2 + y3, quiet = TRUE))
})

test_that("get_anova(fit, extra = 1) errors via rlang check_dots_empty", {
  d <- .disp_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_error(
    get_anova(fit, extra = 1),
    class = "rlib_error_dots_nonempty"
  )
})

test_that("get_anova(list(fit1, fit2), extra = 1) errors via rlang check_dots_empty", {
  d <- .disp_taylor()
  fit_s <- survey_glm(d, y1 ~ y2)
  fit_b <- survey_glm(d, y1 ~ y2 + y3)
  expect_error(
    get_anova(list(fit_s, fit_b), extra = 1),
    class = "rlib_error_dots_nonempty"
  )
})

test_that("get_anova(fit, formula = y ~ x) errors with surveycore_error_anova_formula_unexpected", {
  d <- .disp_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_error(
    get_anova(fit, formula = y1 ~ y2),
    class = "surveycore_error_anova_formula_unexpected"
  )
})

test_that("get_anova(list(fit1, fit2), predictors = 'y2') errors with surveycore_error_anova_formula_unexpected", {
  d <- .disp_taylor()
  fit_s <- survey_glm(d, y1 ~ y2)
  fit_b <- survey_glm(d, y1 ~ y2 + y3)
  expect_error(
    get_anova(list(fit_s, fit_b), predictors = "y2"),
    class = "surveycore_error_anova_formula_unexpected"
  )
})

test_that("get_anova(<character>) errors with surveycore_error_anova_object_invalid", {
  expect_error(
    get_anova("not a fit"),
    class = "surveycore_error_anova_object_invalid"
  )
})

test_that("get_anova(<numeric>) errors with surveycore_error_anova_object_invalid", {
  expect_error(
    get_anova(1:5),
    class = "surveycore_error_anova_object_invalid"
  )
})

# ═══════════════════════════════════════════════════════════════════════════
# null-argument interactions (Q7, Q8)
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_anova(list(fit1, fit2), null = 0) warns with surveycore_warning_anova_null_ignored", {
  d <- .disp_taylor()
  fit_s <- survey_glm(d, y1 ~ y2)
  fit_b <- survey_glm(d, y1 ~ y2 + y3)
  expect_warning(
    r <- get_anova(list(fit_s, fit_b), null = 0, method = "Wald"),
    class = "surveycore_warning_anova_null_ignored"
  )
  # null should be dropped — compare against same call without null
  r_no_null <- suppressWarnings(
    get_anova(list(fit_s, fit_b), method = "Wald")
  )
  expect_equal(r$statistic, r_no_null$statistic, tolerance = 1e-10)
})

test_that("get_anova(list(fit1, fit2), null = 'foo') errors on type before warning", {
  d <- .disp_taylor()
  fit_s <- survey_glm(d, y1 ~ y2)
  fit_b <- survey_glm(d, y1 ~ y2 + y3)
  # Non-numeric null must error with the existing null-type error regardless
  # of object mode (Q7 validation order).
  expect_error(
    get_anova(list(fit_s, fit_b), null = "foo", method = "Wald"),
    class = "surveycore_error_null_length_mismatch"
  )
})

test_that("get_anova(design, y ~ x1, null = 0) forwards null to single-model path", {
  d <- .disp_taylor()
  fit <- survey_glm(d, y1 ~ y2)

  r_design <- get_anova(d, y1 ~ y2, method = "Wald", null = 0)
  r_fit <- get_anova(fit, method = "Wald", null = 0)

  expect_equal(r_design$statistic, r_fit$statistic, tolerance = 1e-10)
})

# ═══════════════════════════════════════════════════════════════════════════
# S3 anova() delegator regression — must still work after model2 removal
# ═══════════════════════════════════════════════════════════════════════════

test_that("anova(fit1, fit2) S3 delegator still produces a 1-row result", {
  d <- .disp_taylor()
  fit_s <- survey_glm(d, y1 ~ y2)
  fit_b <- survey_glm(d, y1 ~ y2 + y3)
  r <- anova(fit_s, fit_b)
  expect_s3_class(r, "survey_anova")
  expect_equal(nrow(r), 1L)
})

# ═══════════════════════════════════════════════════════════════════════════
# Layer 3 snapshot tests — CLI message text for A-21..A-25
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_anova(<character>) snapshot matches (A-21)", {
  expect_snapshot(error = TRUE, get_anova("not a fit"))
})

test_that("get_anova(list mixed classes) snapshot matches (A-21)", {
  d <- .disp_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_snapshot(error = TRUE, get_anova(list(fit, "not a fit", fit)))
})

test_that("get_anova(list()) snapshot matches (A-22)", {
  expect_snapshot(error = TRUE, get_anova(list()))
})

test_that("get_anova(design) without formula snapshot matches (A-23)", {
  d <- .disp_taylor()
  expect_snapshot(error = TRUE, get_anova(d))
})

test_that("get_anova(fit, formula = ...) snapshot matches (A-24)", {
  d <- .disp_taylor()
  fit <- survey_glm(d, y1 ~ y2)
  expect_snapshot(error = TRUE, get_anova(fit, formula = y1 ~ y2))
})

test_that("get_anova(list(fit1, fit2), null = 0) warning snapshot matches (A-25)", {
  d <- .disp_taylor()
  fit_s <- survey_glm(d, y1 ~ y2)
  fit_b <- survey_glm(d, y1 ~ y2 + y3)
  expect_snapshot(
    r <- get_anova(list(fit_s, fit_b), null = 0, method = "Wald")
  )
})
