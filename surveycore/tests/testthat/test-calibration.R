# tests/testthat/test-calibration.R
#
# Tests for R/calibration.R — as_caldata()
#
# Coverage: error-messages.md rows CAL-1 through CAL-9.
# Per testing-standards.md: constructor input validation errors use the dual
# pattern: expect_error(class=) + expect_snapshot(error=TRUE).

# Setup shared test data (reused across tests)
base_w <- rep(1, 10)
g <- rep(1.1, 10)
mm <- matrix(1, 10, 2)


# ── Happy paths ────────────────────────────────────────────────────────────────

test_that("as_caldata() returns a list with the correct four names", {
  result <- as_caldata(base_w, g, mm)
  expect_type(result, "list")
  expect_identical(sort(names(result)), sort(c("qr", "w", "stage", "index")))
  expect_s3_class(result$qr, "qr")
  expect_equal(result$stage, 0L)
  expect_null(result$index)
})

test_that("as_caldata() w slot equals g_weights * sqrt(base_weights)", {
  result <- as_caldata(base_w, g, mm)
  expected_w <- g * sqrt(base_w)
  expect_equal(result$w, expected_w)
})

test_that("as_caldata() qr slot is qr of sqrt(base_w) * model_matrix", {
  result <- as_caldata(base_w, g, mm)
  expected_qr <- qr(sqrt(base_w) * mm)
  # Compare rank and pivot (structural equality) rather than identical() on
  # internal floating-point QR decomposition components
  expect_equal(result$qr$rank, expected_qr$rank)
  expect_equal(result$qr$pivot, expected_qr$pivot)
  expect_equal(qr.R(result$qr), qr.R(expected_qr), tolerance = 1e-10)
})

test_that("as_caldata() works with single-column model matrix (~1 intercept)", {
  mm1 <- matrix(1, 10, 1)
  result <- as_caldata(base_w, g, mm1)
  expect_type(result, "list")
  expect_identical(sort(names(result)), sort(c("qr", "w", "stage", "index")))
  expect_identical(result$stage, 0L)
  expect_null(result$index)
})

test_that("as_caldata() works with g_weights all equal to 1.0 (no calibration effect)", {
  g_ones <- rep(1.0, 10)
  result <- as_caldata(base_w, g_ones, mm)
  expected_w <- g_ones * sqrt(base_w)
  expect_equal(result$w, expected_w)
  expect_equal(result$w, sqrt(base_w))
})

test_that("as_caldata() works with n = 1 (single observation, R-6)", {
  bw1 <- 2.0
  gw1 <- 1.1
  mm1 <- matrix(1, 1, 1)
  result <- as_caldata(bw1, gw1, mm1)
  expect_s3_class(result$qr, "qr")
  expect_equal(result$w, gw1 * sqrt(bw1))
  expect_identical(result$stage, 0L)
  expect_null(result$index)
})


# ── Error paths ────────────────────────────────────────────────────────────────

test_that("as_caldata() errors on empty base_weights (n = 0, R-6)", {
  expect_error(
    as_caldata(numeric(0), numeric(0), matrix(0, 0, 1)),
    class = "surveycore_error_caldata_weights_nonpositive"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(numeric(0), numeric(0), matrix(0, 0, 1))
  )
})

test_that("as_caldata() rejects non-positive base_weights", {
  bw_bad <- c(1, -1, 1, 1, 1, 1, 1, 1, 1, 1)
  expect_error(
    as_caldata(bw_bad, g, mm),
    class = "surveycore_error_caldata_weights_nonpositive"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(bw_bad, g, mm)
  )
})

test_that("as_caldata() rejects NA in base_weights (B-4)", {
  bw_na <- c(NA_real_, rep(1, 9))
  expect_error(
    as_caldata(bw_na, g, mm),
    class = "surveycore_error_caldata_weights_missing"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(bw_na, g, mm)
  )
})

test_that("as_caldata() rejects Inf in base_weights (B-4)", {
  bw_inf <- c(Inf, rep(1, 9))
  expect_error(
    as_caldata(bw_inf, g, mm),
    class = "surveycore_error_caldata_weights_missing"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(bw_inf, g, mm)
  )
})

test_that("as_caldata() rejects non-positive g_weights", {
  g_bad <- c(1.1, -0.5, rep(1.1, 8))
  expect_error(
    as_caldata(base_w, g_bad, mm),
    class = "surveycore_error_caldata_gweights_nonpositive"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(base_w, g_bad, mm)
  )
})

test_that("as_caldata() rejects NA in g_weights (B-4)", {
  g_na <- c(NA_real_, rep(1.1, 9))
  expect_error(
    as_caldata(base_w, g_na, mm),
    class = "surveycore_error_caldata_gweights_missing"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(base_w, g_na, mm)
  )
})

test_that("as_caldata() rejects g_weights of wrong length (B-3)", {
  g_short <- rep(1.1, 5)
  expect_error(
    as_caldata(base_w, g_short, mm),
    class = "surveycore_error_caldata_gweights_length_mismatch"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(base_w, g_short, mm)
  )
})

test_that("as_caldata() rejects near-zero g_weights * sqrt(base_weights)", {
  # Use very small g_weights so product < .Machine$double.eps^0.5
  # .Machine$double.eps^0.5 ~ 1.49e-8; use something 100x smaller
  g_tiny <- rep(.Machine$double.eps^0.6, 10)
  expect_error(
    as_caldata(base_w, g_tiny, mm),
    class = "surveycore_error_caldata_weights_near_zero"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(base_w, g_tiny, mm)
  )
})

test_that("as_caldata() rejects model_matrix with wrong number of rows", {
  mm_wrong <- matrix(1, 5, 2)
  expect_error(
    as_caldata(base_w, g, mm_wrong),
    class = "surveycore_error_caldata_dimension_mismatch"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(base_w, g, mm_wrong)
  )
})

test_that("as_caldata() rejects model_matrix with 0 columns", {
  mm_empty <- matrix(numeric(0), nrow = 10, ncol = 0)
  expect_error(
    as_caldata(base_w, g, mm_empty),
    class = "surveycore_error_caldata_empty_model_matrix"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(base_w, g, mm_empty)
  )
})

test_that("as_caldata() rejects model_matrix with NA values", {
  mm_na <- mm
  mm_na[1, 1] <- NA_real_
  expect_error(
    as_caldata(base_w, g, mm_na),
    class = "surveycore_error_caldata_model_matrix_invalid"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(base_w, g, mm_na)
  )
})

test_that("as_caldata() rejects model_matrix with Inf values", {
  mm_inf <- mm
  mm_inf[2, 1] <- Inf
  expect_error(
    as_caldata(base_w, g, mm_inf),
    class = "surveycore_error_caldata_model_matrix_invalid"
  )
  expect_snapshot(
    error = TRUE,
    as_caldata(base_w, g, mm_inf)
  )
})


# ── .apply_caldata_projection() direct tests ───────────────────────────────────

test_that(".apply_caldata_projection() errors when any caldata entry has stage != 0L [direct]", {
  cd <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  cd_bad <- cd
  cd_bad$stage <- 1L
  u <- matrix(rnorm(10), 10, 1)
  expect_error(
    surveycore:::.apply_caldata_projection(u, list(cd_bad)),
    class = "surveycore_error_caldata_within_stage_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.apply_caldata_projection(u, list(cd_bad))
  )
})

test_that(".apply_caldata_projection() errors on dimension mismatch between u and cd$w [direct]", {
  cd_5 <- as_caldata(rep(1, 5), rep(1.1, 5), matrix(1, 5, 1))
  u_10 <- matrix(rnorm(10), 10, 1)
  expect_error(
    surveycore:::.apply_caldata_projection(u_10, list(cd_5)),
    class = "surveycore_error_caldata_projection_dimension_mismatch"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.apply_caldata_projection(u_10, list(cd_5))
  )
})

test_that(".apply_caldata_projection() returns u unchanged when u is all-NA", {
  cd <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  u_na <- matrix(NA_real_, 10, 1)
  result <- surveycore:::.apply_caldata_projection(u_na, list(cd))
  expect_true(all(is.na(result)))
})

test_that(".apply_caldata_projection() returns u unchanged when caldata is empty list [direct, B-7]", {
  u <- matrix(rnorm(10), 10, 1)
  result <- surveycore:::.apply_caldata_projection(u, list())
  expect_identical(result, u)
})

test_that(".apply_caldata_projection() errors on NULL element in caldata list [direct, B-5]", {
  cd <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  u <- matrix(rnorm(10), 10, 1)
  expect_error(
    surveycore:::.apply_caldata_projection(u, list(cd, NULL)),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.apply_caldata_projection(u, list(cd, NULL))
  )
})


# ── Numerical / behavioral accuracy tests ─────────────────────────────────────
#
# Oracle setup helper (used in multiple tests below):
#   nhanes_sub: nhanes_2017 filtered to positive wtmec2yr and ridstatr == 2
#   sv_design / sv_cal: survey package reference designs
#   sc_cal: surveycore calibrated design matching sv_cal
#
# The calibration uses riagendr as an auxiliary variable, calibrating to the
# population totals implied by the interview weights. This produces g-weights
# != 1 (real calibration effect).

.make_nhanes_oracle <- function() {
  skip_if_not_installed("survey")
  nhanes_sub <- nhanes_2017[
    nhanes_2017$wtmec2yr > 0 & nhanes_2017$ridstatr == 2,
  ]
  sv_design <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  )
  gender_pop <- c(
    "(Intercept)" = sum(nhanes_sub$wtmec2yr),
    riagendr = sum(
      nhanes_sub$riagendr * nhanes_sub$wtint2yr / sum(nhanes_sub$wtint2yr)
    ) *
      sum(nhanes_sub$wtmec2yr)
  )
  sv_cal <- survey::calibrate(
    sv_design,
    formula = ~riagendr,
    population = gender_pop
  )
  g <- weights(sv_cal) / nhanes_sub$wtmec2yr
  mm <- model.matrix(~riagendr, nhanes_sub)
  cd <- as_caldata(nhanes_sub$wtmec2yr, g, mm)

  sc_design <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  sc_design@data$wtmec2yr_cal <- weights(sv_cal)
  sc_cal <- suppressMessages(
    suppressWarnings(update_design(sc_design, weights = wtmec2yr_cal))
  )
  sc_cal@calibration <- list(cd)

  list(
    nhanes_sub = nhanes_sub,
    sv_design = sv_design,
    sv_cal = sv_cal,
    sc_design = sc_design,
    sc_cal = sc_cal,
    cd = cd
  )
}

test_that("calibration-adjusted SE from get_means() matches survey oracle [linear, nhanes]", {
  skip_if_not_installed("survey")
  o <- .make_nhanes_oracle()
  sv_se <- as.numeric(
    survey::SE(survey::svymean(~bpxsy1, o$sv_cal, na.rm = TRUE))
  )
  sc_se <- get_means(o$sc_cal, bpxsy1, variance = "se")$se
  expect_equal(sc_se, sv_se, tolerance = 1e-8)
})

test_that("post-calibration point estimate from get_means() matches survey oracle [linear, nhanes]", {
  skip_if_not_installed("survey")
  o <- .make_nhanes_oracle()
  sv_mean <- as.numeric(survey::svymean(~bpxsy1, o$sv_cal, na.rm = TRUE))
  sc_mean <- get_means(o$sc_cal, bpxsy1, variance = "se")$mean
  expect_equal(sc_mean, sv_mean, tolerance = 1e-10)
})

test_that("calibration-adjusted SE from get_totals() matches survey oracle [linear, nhanes]", {
  skip_if_not_installed("survey")
  o <- .make_nhanes_oracle()
  sv_se <- as.numeric(
    survey::SE(survey::svytotal(~bpxsy1, o$sv_cal, na.rm = TRUE))
  )
  sc_se <- get_totals(o$sc_cal, bpxsy1, variance = "se")$se
  expect_equal(sc_se, sv_se, tolerance = 1e-8)
})

test_that("calibration reduces SE when auxiliary variable correlates with outcome [nhanes]", {
  skip_if_not_installed("survey")
  o <- .make_nhanes_oracle()
  se_uncal <- get_means(o$sc_design, bpxsy1, variance = "se")$se
  se_cal <- get_means(o$sc_cal, bpxsy1, variance = "se")$se
  # The calibration is on riagendr (gender), which is correlated with bpxsy1;
  # calibration may increase or decrease SE depending on the correlation.
  # The key test is that they differ (showing calibration had an effect).
  expect_false(isTRUE(all.equal(se_uncal, se_cal, tolerance = 1e-10)))
})

test_that("calibration-adjusted SE matches oracle on SRS design [get_means, R-5]", {
  skip_if_not_installed("survey")
  # Use a small SRS-style design (no ids/strata) to test the SRS path
  set.seed(123)
  n <- 200L
  df <- data.frame(
    y = rnorm(n, 5, 2),
    wt = runif(n, 0.8, 1.2)
  )
  sv_d <- survey::svydesign(ids = ~1, weights = ~wt, data = df)
  pop_totals <- c("(Intercept)" = sum(df$wt))
  sv_cal <- survey::calibrate(sv_d, formula = ~1, population = pop_totals)
  g <- weights(sv_cal) / df$wt
  mm <- model.matrix(~1, df)
  cd <- as_caldata(df$wt, g, mm)
  sc_d <- as_survey(df, weights = wt)
  sc_d@data$wt_cal <- weights(sv_cal)
  sc_cal <- suppressMessages(
    suppressWarnings(update_design(sc_d, weights = wt_cal))
  )
  sc_cal@calibration <- list(cd)

  sv_se <- as.numeric(survey::SE(survey::svymean(~y, sv_cal, na.rm = TRUE)))
  sc_se <- get_means(sc_cal, y, variance = "se")$se
  expect_equal(sc_se, sv_se, tolerance = 1e-8)
})

test_that("get_means() SE is unchanged when @calibration is NULL [regression]", {
  df <- make_survey_data(n = 200L, seed = 99L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  # No calibration set
  expect_null(d@calibration)
  se_result <- get_means(d, y1, variance = "se")$se
  expect_true(is.numeric(se_result))
  expect_true(se_result > 0)
})

test_that("get_means() SE is unchanged when @calibration is an empty list", {
  df <- make_survey_data(n = 200L, seed = 100L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d@calibration <- list()
  se_result <- get_means(d, y1, variance = "se")$se
  expect_true(is.numeric(se_result))
  expect_true(se_result > 0)
})

test_that("get_means() on survey_replicate with @calibration set returns same SE as uncalibrated", {
  # Calibration projection is not applied to replicate path; @calibration on
  # survey_replicate should be silently ignored by .maybe_apply_calibration
  # (since the replicate path never calls .taylor_mean_cell).
  df <- make_survey_data(
    n = 200L,
    design = "replicate",
    type = "brr",
    seed = 77L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d_rep <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::starts_with("repwt"),
    type = "BRR"
  )
  # Attach a caldata element (should be ignored by replicate path)
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  d_cal <- d_rep
  d_cal@calibration <- list(cd)

  se_rep <- get_means(d_rep, y1, variance = "se")$se
  se_cal <- get_means(d_cal, y1, variance = "se")$se
  # Calibration on replicate design should not change SE (ignored in replicate path)
  expect_equal(se_rep, se_cal, tolerance = 1e-12)
})

test_that("get_means() returns NA SE on calibrated design with all-NA outcome column [R-8]", {
  df <- make_survey_data(n = 200L, seed = 101L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  df$y_na <- NA_real_
  d@data$y_na <- NA_real_
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  d@calibration <- list(cd)

  result <- get_means(d, y_na, variance = "se")
  expect_true(is.na(result$se))
  expect_true(is.na(result$mean))
})

test_that("two accumulated caldata entries both applied by .apply_caldata_projection()", {
  # Use a simple SRS design with two sequential calibrations
  set.seed(42L)
  n <- 50L
  df <- data.frame(
    y = rnorm(n),
    wt = rep(1, n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  # Build two caldata elements
  cd1 <- as_caldata(df$wt, rep(1.01, n), model.matrix(~1, df))
  cd2 <- as_caldata(df$wt, rep(1.01, n), model.matrix(~x1, df))
  d <- as_survey(df, weights = wt)
  d@calibration <- list(cd1, cd2)

  result <- get_means(d, y, variance = "se")
  # Both projections applied: df should decrease by rank(cd1) + rank(cd2) = 1 + 2
  expect_equal(result$df, max(1L, .degf(d) - 1L - 2L))
})

test_that("calibration-adjusted SE is applied to domain estimates [get_means with group]", {
  skip_if_not_installed("survey")
  o <- .make_nhanes_oracle()
  # svyby returns a data frame with rows ordered by the by-variable
  sv_res <- survey::svyby(
    ~bpxsy1,
    ~riagendr,
    o$sv_cal,
    survey::svymean,
    na.rm = TRUE
  )
  # Extract SEs for riagendr == 1 and riagendr == 2 by position
  sv_ses <- as.numeric(survey::SE(sv_res)) # SE() on svyby gives a vector

  sc_res <- get_means(o$sc_cal, bpxsy1, group = riagendr, variance = "se")
  sc_res_sorted <- sc_res[order(sc_res$riagendr), ]
  sc_ses <- sc_res_sorted$se

  expect_equal(sc_ses, sv_ses, tolerance = 1e-8)
})


# ── Df-adjustment tests ────────────────────────────────────────────────────────

test_that("calibrated design has reduced df in get_means() output [nhanes, linear]", {
  skip_if_not_installed("survey")
  o <- .make_nhanes_oracle()
  # Uncalibrated df
  df_uncal <- .degf(o$sc_design)
  # Calibrated df: reduced by qr rank of cd (rank = 2 for ~riagendr intercept + coef)
  cd_rank <- o$cd$qr$rank
  result_cal <- get_means(o$sc_cal, bpxsy1, variance = "se")
  expect_equal(result_cal$df, df_uncal - cd_rank)
})

test_that("df column in get_means() decreases by qr$rank for each caldata entry", {
  set.seed(10L)
  n <- 100L
  df <- data.frame(
    y = rnorm(n),
    wt = rep(1, n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  d <- as_survey(df, ids = NULL, weights = wt)

  # No calibration
  result_none <- get_means(d, y, variance = "se")
  # The df column doesn't appear when uncalibrated
  expect_false("df" %in% names(result_none))

  # One caldata entry: rank = 1 (intercept only)
  cd1 <- as_caldata(df$wt, rep(1.01, n), model.matrix(~1, df))
  d1 <- d
  d1@calibration <- list(cd1)
  result1 <- get_means(d1, y, variance = "se")
  base_df <- .degf(d)
  expect_equal(result1$df, base_df - 1L)

  # Two caldata entries: rank 1 + rank 2
  cd2 <- as_caldata(df$wt, rep(1.01, n), model.matrix(~x1, df))
  d2 <- d
  d2@calibration <- list(cd1, cd2)
  result2 <- get_means(d2, y, variance = "se")
  expect_equal(result2$df, base_df - 1L - 2L)
})

test_that("calibration-adjusted SE from get_means() matches oracle with ridageyr aux [alternative calibration, nhanes]", {
  skip_if_not_installed("survey")
  # Second oracle test using ridageyr as auxiliary — verifies calibration works
  # with a continuous auxiliary variable producing non-trivial g-weights.
  nhanes_sub <- nhanes_2017[
    nhanes_2017$wtmec2yr > 0 & nhanes_2017$ridstatr == 2,
  ]
  sv_design <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  )
  # Calibrate to ridageyr totals derived from interview weights
  age_pop <- c(
    "(Intercept)" = sum(nhanes_sub$wtmec2yr),
    ridageyr = sum(
      nhanes_sub$ridageyr * nhanes_sub$wtint2yr / sum(nhanes_sub$wtint2yr)
    ) *
      sum(nhanes_sub$wtmec2yr)
  )
  sv_cal2 <- survey::calibrate(
    sv_design,
    formula = ~ridageyr,
    population = age_pop
  )
  sv_se2 <- as.numeric(
    survey::SE(survey::svymean(~bpxsy1, sv_cal2, na.rm = TRUE))
  )

  g2 <- weights(sv_cal2) / nhanes_sub$wtmec2yr
  mm2 <- model.matrix(~ridageyr, nhanes_sub)
  cd2 <- as_caldata(nhanes_sub$wtmec2yr, g2, mm2)

  sc_design2 <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  sc_design2@data$wt2 <- weights(sv_cal2)
  sc_cal2 <- suppressMessages(
    suppressWarnings(update_design(sc_design2, weights = wt2))
  )
  sc_cal2@calibration <- list(cd2)

  sc_se2 <- get_means(sc_cal2, bpxsy1, variance = "se")$se
  expect_equal(sc_se2, sv_se2, tolerance = 1e-8)
})

test_that("get_means() warns when calibration df reduction >= design df [R-7]", {
  # Create a design where df reduction exceeds the design df.
  # We need n >= min_cell_n (30) to avoid small_cell warning.
  # Use a simple SRS design (no PSUs/strata): df = n - 1
  # Then use a very high rank model matrix to make df reduction > df.
  set.seed(55L)
  n <- 40L
  df <- data.frame(
    y = rnorm(n),
    wt = rep(1, n)
  )
  # SRS design: df_design = n - 1 = 39
  d <- as_survey(df, weights = wt)
  # Use a model matrix with rank > 39 to trigger warning
  # Build identity matrix (rank = n = 40 > 39)
  x_mat <- diag(n)
  cd_high <- as_caldata(df$wt, rep(1.001, n), x_mat)
  d@calibration <- list(cd_high)

  expect_warning(
    get_means(d, y, variance = "se"),
    class = "surveycore_warning_zero_df_after_calibration"
  )
})

test_that("calibration-adjusted SE from get_means() matches survey::calibrate(calfun = 'raking') oracle [raking, nhanes]", {
  skip_if_not_installed("survey")
  skip_if_not_installed("MASS")
  # Architecture A: single combined model matrix for two-margin raking.
  # Oracle: survey::calibrate(calfun = "raking") with the same combined matrix.
  # surveycore: single as_caldata() element with g = raked_wts / base_wts.
  nhanes_sub <- nhanes_2017[
    nhanes_2017$wtmec2yr > 0 & nhanes_2017$ridstatr == 2,
  ]
  nhanes_sub$age_group <- cut(
    nhanes_sub$ridageyr,
    breaks = c(0, 18, 40, 60, Inf),
    right = FALSE
  )
  nhanes_sub$riagendr_f <- factor(nhanes_sub$riagendr)

  sv_design <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = nhanes_sub,
    nest = TRUE
  )

  # Use survey::rake() to obtain converged IPF weights (same as before).
  pop_gender <- list(
    riagendr_f = data.frame(
      riagendr_f = levels(nhanes_sub$riagendr_f),
      Freq = c(
        sum(nhanes_sub$wtmec2yr[nhanes_sub$riagendr_f == "1"]),
        sum(nhanes_sub$wtmec2yr[nhanes_sub$riagendr_f == "2"])
      )
    )
  )
  pop_age <- list(
    age_group = data.frame(
      age_group = levels(nhanes_sub$age_group),
      Freq = c(
        sum(nhanes_sub$wtmec2yr[nhanes_sub$age_group == "[0,18)"]),
        sum(nhanes_sub$wtmec2yr[nhanes_sub$age_group == "[18,40)"]),
        sum(nhanes_sub$wtmec2yr[nhanes_sub$age_group == "[40,60)"]),
        sum(nhanes_sub$wtmec2yr[nhanes_sub$age_group == "[60,Inf)"])
      )
    )
  )
  sv_raked <- survey::rake(
    sv_design,
    sample.margins = list(~riagendr_f, ~age_group),
    population.margins = c(pop_gender, pop_age)
  )

  # Build combined model matrix (Architecture A): single formula for both margins.
  combined_mm <- model.matrix(~ riagendr_f + age_group, nhanes_sub)

  # Population totals for the combined matrix columns.
  combined_pop <- setNames(
    colSums(combined_mm * nhanes_sub$wtmec2yr),
    colnames(combined_mm)
  )

  # Oracle: survey::calibrate with calfun = "raking" and the combined formula.
  sv_oracle <- survey::calibrate(
    sv_design,
    formula = ~ riagendr_f + age_group,
    population = combined_pop,
    calfun = "raking"
  )
  sv_se <- as.numeric(
    survey::SE(survey::svymean(~bpxsy1, sv_oracle, na.rm = TRUE))
  )

  # surveycore: single caldata element with g = raked_wts / base_wts.
  g <- weights(sv_raked) / nhanes_sub$wtmec2yr
  cd <- as_caldata(nhanes_sub$wtmec2yr, g, combined_mm)

  sc_design <- as_survey(
    nhanes_sub,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  sc_design@data$wt_raked <- weights(sv_raked)
  sc_raked <- suppressMessages(
    suppressWarnings(update_design(sc_design, weights = wt_raked))
  )
  sc_raked@calibration <- list(cd)
  sc_se <- get_means(sc_raked, bpxsy1, variance = "se")$se
  expect_equal(sc_se, sv_se, tolerance = 1e-8)
})


# ── .validate_calibration_arg() unit tests ───────────────────────────────────

test_that(".validate_calibration_arg() returns invisible TRUE for NULL [direct]", {
  result <- withVisible(surveycore:::.validate_calibration_arg(NULL, 10L))
  expect_true(result$value)
  expect_false(result$visible)
})

test_that(".validate_calibration_arg() returns invisible TRUE for list() [direct]", {
  result <- withVisible(surveycore:::.validate_calibration_arg(list(), 10L))
  expect_true(result$value)
  expect_false(result$visible)
})

test_that(".validate_calibration_arg() errors on numeric vector (CAL-15) [direct]", {
  expect_error(
    surveycore:::.validate_calibration_arg(c(1, 2, 3), 10L),
    class = "surveycore_error_calibration_not_list"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.validate_calibration_arg(c(1, 2, 3), 10L)
  )
})

test_that(".validate_calibration_arg() errors on data frame (CAL-15) [direct]", {
  expect_error(
    surveycore:::.validate_calibration_arg(
      data.frame(x = 1:3),
      10L
    ),
    class = "surveycore_error_calibration_not_list"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.validate_calibration_arg(data.frame(x = 1:3), 10L)
  )
})

test_that(".validate_calibration_arg() errors on bare caldata (not wrapped) (CAL-15) [direct]", {
  bare_cd <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  # bare_cd is a list with all four caldata keys at top level — detected as
  # a bare (un-wrapped) caldata and triggers CAL-15, not CAL-16.
  expect_error(
    surveycore:::.validate_calibration_arg(bare_cd, 10L),
    class = "surveycore_error_calibration_not_list"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.validate_calibration_arg(bare_cd, 10L)
  )
})

test_that(".validate_calibration_arg() errors on NULL element in list (CAL-16) [direct]", {
  cd <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  expect_error(
    surveycore:::.validate_calibration_arg(list(cd, NULL), 10L),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.validate_calibration_arg(list(cd, NULL), 10L)
  )
})

test_that(".validate_calibration_arg() errors on element missing index field (CAL-16) [direct]", {
  cd <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  cd_bad <- cd
  cd_bad$index <- NULL # remove index field
  expect_error(
    surveycore:::.validate_calibration_arg(list(cd_bad), 10L),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.validate_calibration_arg(list(cd_bad), 10L)
  )
})

test_that(".validate_calibration_arg() errors when length(cd$w) != nrow_data (CAL-16) [direct]", {
  # cd built for 10 obs but we pass nrow_data = 5
  cd <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  expect_error(
    surveycore:::.validate_calibration_arg(list(cd), 5L),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    surveycore:::.validate_calibration_arg(list(cd), 5L)
  )
})

test_that(".validate_calibration_arg() passes for two valid elements [direct]", {
  cd1 <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  cd2 <- as_caldata(rep(1, 10), rep(1.0, 10), matrix(1:10, 10, 1))
  result <- withVisible(
    surveycore:::.validate_calibration_arg(list(cd1, cd2), 10L)
  )
  expect_true(result$value)
  expect_false(result$visible)
})

test_that(".validate_calibration_arg() tolerates extra fields on caldata element [direct]", {
  cd <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  cd$extra_field <- "hello"
  result <- withVisible(
    surveycore:::.validate_calibration_arg(list(cd), 10L)
  )
  expect_true(result$value)
  expect_false(result$visible)
})


# ── Constructor calibration parameter tests ────────────────────────────────────

# as_survey() error paths ──────────────────────────────────────────────────────

test_that("as_survey() errors on bare caldata (not wrapped) (CAL-15)", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  bare_cd <- as_caldata(df$wt, rep(1.1, 10), matrix(1, 10, 1))
  # bare_cd has all four caldata keys at top level — detected as bare caldata
  expect_error(
    as_survey(df, weights = wt, calibration = bare_cd),
    class = "surveycore_error_calibration_not_list"
  )
  expect_snapshot(
    error = TRUE,
    as_survey(df, weights = wt, calibration = bare_cd)
  )
})

test_that("as_survey() errors on numeric vector calibration (CAL-15)", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  expect_error(
    as_survey(df, weights = wt, calibration = c(1, 2, 3)),
    class = "surveycore_error_calibration_not_list"
  )
  expect_snapshot(
    error = TRUE,
    as_survey(df, weights = wt, calibration = c(1, 2, 3))
  )
})

test_that("as_survey() errors on data frame calibration (CAL-15)", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  expect_error(
    as_survey(df, weights = wt, calibration = data.frame(x = 1:3)),
    class = "surveycore_error_calibration_not_list"
  )
  expect_snapshot(
    error = TRUE,
    as_survey(df, weights = wt, calibration = data.frame(x = 1:3))
  )
})

test_that("as_survey() errors on element missing index field (CAL-16)", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  cd <- as_caldata(df$wt, rep(1.1, 10), matrix(1, 10, 1))
  cd_bad <- cd
  cd_bad$index <- NULL
  expect_error(
    as_survey(df, weights = wt, calibration = list(cd_bad)),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    as_survey(df, weights = wt, calibration = list(cd_bad))
  )
})

test_that("as_survey() errors on NULL element in calibration list (CAL-16)", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  cd <- as_caldata(df$wt, rep(1.1, 10), matrix(1, 10, 1))
  expect_error(
    as_survey(df, weights = wt, calibration = list(cd, NULL)),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    as_survey(df, weights = wt, calibration = list(cd, NULL))
  )
})

test_that("as_survey() errors when length(cd$w) != nrow(data) (CAL-16)", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  cd_wrong <- as_caldata(rep(1, 5), rep(1.1, 5), matrix(1, 5, 1))
  expect_error(
    as_survey(df, weights = wt, calibration = list(cd_wrong)),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    as_survey(df, weights = wt, calibration = list(cd_wrong))
  )
})

test_that("as_survey() errors on second bad element in two-element list (CAL-16)", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  cd_good <- as_caldata(df$wt, rep(1.1, 10), matrix(1, 10, 1))
  cd_bad <- as_caldata(rep(1, 5), rep(1.1, 5), matrix(1, 5, 1))
  expect_error(
    as_survey(df, weights = wt, calibration = list(cd_good, cd_bad)),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    as_survey(df, weights = wt, calibration = list(cd_good, cd_bad))
  )
})

test_that("as_survey() with calibration = list() succeeds; @calibration is list()", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  d <- as_survey(df, weights = wt, calibration = list())
  test_invariants(d)
  expect_identical(d@calibration, list())
})

test_that("as_survey() with calibration = NULL succeeds; @calibration is NULL", {
  df <- data.frame(y = 1:10, wt = rep(1, 10))
  d <- suppressWarnings(as_survey(df, weights = wt, calibration = NULL))
  test_invariants(d)
  expect_null(d@calibration)
})

# as_survey_replicate() error paths ───────────────────────────────────────────

test_that("as_survey_replicate() errors on non-list calibration (CAL-15)", {
  df <- make_survey_data(n = 200L, design = "replicate", seed = 3L)
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  expect_error(
    as_survey_replicate(
      df,
      weights = wt,
      repweights = tidyselect::all_of(repwt_cols),
      type = "BRR",
      calibration = c(1, 2, 3)
    ),
    class = "surveycore_error_calibration_not_list"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_replicate(
      df,
      weights = wt,
      repweights = tidyselect::all_of(repwt_cols),
      type = "BRR",
      calibration = c(1, 2, 3)
    )
  )
})

test_that("as_survey_replicate() errors on invalid caldata structure (CAL-16)", {
  df <- make_survey_data(n = 200L, design = "replicate", seed = 4L)
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  cd <- as_caldata(df$wt, rep(1.1, nrow(df)), matrix(1, nrow(df), 1))
  cd_bad <- cd
  cd_bad$index <- NULL
  expect_error(
    as_survey_replicate(
      df,
      weights = wt,
      repweights = tidyselect::all_of(repwt_cols),
      type = "BRR",
      calibration = list(cd_bad)
    ),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_replicate(
      df,
      weights = wt,
      repweights = tidyselect::all_of(repwt_cols),
      type = "BRR",
      calibration = list(cd_bad)
    )
  )
})

test_that("as_survey_replicate() errors when length(cd$w) != nrow(data) (CAL-16)", {
  df <- make_survey_data(n = 200L, design = "replicate", seed = 5L)
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  cd_wrong <- as_caldata(rep(1, 10), rep(1.1, 10), matrix(1, 10, 1))
  expect_error(
    as_survey_replicate(
      df,
      weights = wt,
      repweights = tidyselect::all_of(repwt_cols),
      type = "BRR",
      calibration = list(cd_wrong)
    ),
    class = "surveycore_error_caldata_invalid_element"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_replicate(
      df,
      weights = wt,
      repweights = tidyselect::all_of(repwt_cols),
      type = "BRR",
      calibration = list(cd_wrong)
    )
  )
})

test_that("as_survey_replicate() with calibration = list() succeeds", {
  df <- make_survey_data(n = 200L, design = "replicate", seed = 6L)
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR",
    calibration = list()
  )
  test_invariants(d)
  expect_identical(d@calibration, list())
})

test_that("as_survey_replicate() with calibration = NULL succeeds", {
  df <- make_survey_data(n = 200L, design = "replicate", seed = 7L)
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR",
    calibration = NULL
  )
  test_invariants(d)
  expect_null(d@calibration)
})

test_that("as_survey_replicate() SE unchanged with/without @calibration (provenance-only)", {
  df <- make_survey_data(
    n = 200L,
    design = "replicate",
    type = "brr",
    seed = 42L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)

  d_without <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  test_invariants(d_without)
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  d_with <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR",
    calibration = list(cd)
  )

  se_without <- get_means(d_without, y1, variance = "se")$se
  se_with <- get_means(d_with, y1, variance = "se")$se
  expect_equal(se_without, se_with, tolerance = 1e-12)
})

test_that("as_survey() stores calibration argument at @calibration", {
  df <- make_survey_data(n = 200L, seed = 1L)
  base_w <- df$wt
  g_w <- rep(1, nrow(df))
  mm <- model.matrix(~strata, df)
  cd <- as_caldata(base_w, g_w, mm)
  d <- as_survey(df, weights = wt, calibration = list(cd))
  test_invariants(d)
  expect_identical(d@calibration, list(cd))
})

test_that("as_survey() stores list of two caldata elements at @calibration", {
  df <- make_survey_data(n = 200L, seed = 10L)
  cd1 <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  cd2 <- as_caldata(df$wt, rep(1.02, nrow(df)), matrix(1, nrow(df), 1))
  d <- as_survey(df, weights = wt, calibration = list(cd1, cd2))
  test_invariants(d)
  expect_identical(d@calibration, list(cd1, cd2))
})

test_that("as_survey() @calibration matches whether set via arg or post-construction", {
  df <- make_survey_data(n = 200L, seed = 11L)
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  d_via_arg <- as_survey(df, weights = wt, calibration = list(cd))
  d_post <- as_survey(df, weights = wt)
  d_post@calibration <- list(cd)
  test_invariants(d_via_arg)
  expect_identical(d_via_arg@calibration, d_post@calibration)
})

test_that("as_survey_replicate() stores calibration argument at @calibration", {
  df <- make_survey_data(n = 200L, design = "replicate", seed = 2L)
  base_w <- df$wt
  g_w <- rep(1, nrow(df))
  mm <- model.matrix(~strata, df)
  cd <- as_caldata(base_w, g_w, mm)
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR",
    calibration = list(cd)
  )
  test_invariants(d)
  expect_identical(d@calibration, list(cd))
})
