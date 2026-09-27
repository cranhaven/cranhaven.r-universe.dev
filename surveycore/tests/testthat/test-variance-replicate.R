# test-variance-replicate.R
# Tests for the replicate-weight variance engine: .svy_rep_var(),
# .replicate_estimate(), .replicate_mean(), .replicate_total().
#
# Numerical tolerance: 1e-10 for point estimates, 1e-8 for SE/variance.
# Oracle tests skip if the 'survey' package is not installed.

# ---------------------------------------------------------------------------
# Block 10: Replicate weight variance — numerical comparison
# ---------------------------------------------------------------------------

test_that("get_means() replicate SE matches survey::svymean() — BRR design", {
  skip_if_not_installed("survey")

  d <- make_survey_data(
    n = 200,
    n_psu = 20,
    n_strata = 4,
    design = "replicate",
    type = "brr",
    seed = 7
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)

  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )
  sv <- survey::svrepdesign(
    weights = d$wt,
    repweights = d[, repwt_cols],
    type = "BRR",
    mse = TRUE,
    data = d
  )

  sc_mean <- get_means(sc, y1, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y1, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["y1"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_totals() replicate SE matches survey::svytotal() — BRR design", {
  skip_if_not_installed("survey")

  d <- make_survey_data(
    n = 200,
    n_psu = 20,
    n_strata = 4,
    design = "replicate",
    type = "brr",
    seed = 7
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)

  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )
  sv <- survey::svrepdesign(
    weights = d$wt,
    repweights = d[, repwt_cols],
    type = "BRR",
    mse = TRUE,
    data = d
  )

  sc_total <- get_totals(sc, y1, variance = c("se", "ci"))
  sv_total <- survey::svytotal(~y1, sv, na.rm = TRUE)

  expect_equal(sc_total$total, coef(sv_total)[["y1"]], tolerance = 1e-10)
  expect_equal(sc_total$se, as.numeric(survey::SE(sv_total)), tolerance = 1e-8)
  expect_equal(sc_total$ci_low, confint(sv_total)[1], tolerance = 1e-6)
  expect_equal(sc_total$ci_high, confint(sv_total)[2], tolerance = 1e-6)
})

test_that("get_means() replicate SE matches survey::svymean() — JK1 design", {
  skip_if_not_installed("survey")

  d <- make_survey_data(
    n = 200,
    n_psu = 20,
    n_strata = 4,
    design = "replicate",
    type = "jk1",
    seed = 15
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)

  # Compute scale explicitly to avoid survey package "guessing" warning.
  n_rep <- length(repwt_cols)
  jk1_scale <- (n_rep - 1L) / n_rep

  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "JK1"
  )
  sv <- suppressWarnings(survey::svrepdesign(
    weights = d$wt,
    repweights = d[, repwt_cols],
    type = "JK1",
    scale = jk1_scale,
    mse = TRUE,
    data = d
  ))

  sc_mean <- get_means(sc, y1, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y1, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["y1"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_means() replicate: mse=FALSE matches survey with mse=FALSE", {
  skip_if_not_installed("survey")

  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 22
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)

  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR",
    mse = FALSE
  )
  sv <- survey::svrepdesign(
    weights = d$wt,
    repweights = d[, repwt_cols],
    type = "BRR",
    mse = FALSE,
    data = d
  )

  sc_mean <- get_means(sc, y1, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y1, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["y1"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_means() and get_totals() work for survey_replicate (return structure)", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    design = "replicate",
    type = "brr",
    seed = 3
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )

  m <- get_means(sc, y1, variance = "se")
  test_result_invariants(m, "survey_means")
  expect_identical(names(meta(m)$x), "y1")
  expect_true(is.finite(m$mean[[1L]]))
  expect_gte(m$se[[1L]], 0)

  t <- get_totals(sc, y1, variance = "se")
  test_result_invariants(t, "survey_totals")
  expect_true(is.finite(t$total[[1L]]))
  expect_gte(t$se[[1L]], 0)
})

test_that("get_means() BRR scale formula 1/n_rep is correct for n_rep != 4", {
  # Verifies that scale = 1/n_rep (not 1/4) is the correct BRR formula.
  # With n_psu = 20, n_rep = 10 (half-samples from 20 PSUs).
  # If 1/n_rep is correct, surveycore and survey::svrepdesign agree at 1e-8.
  skip_if_not_installed("survey")

  d <- make_survey_data(
    n = 200,
    n_psu = 20,
    n_strata = 4,
    design = "replicate",
    type = "brr",
    seed = 99
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  n_rep <- length(repwt_cols) # should be 10 (n_psu / 2)

  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )
  # survey hardcodes scale = 1/R for BRR internally; the scale= argument is
  # ignored. Both packages independently compute (1/n_rep) * sum((theta_r - theta)^2).
  sv <- survey::svrepdesign(
    weights = d$wt,
    repweights = d[, repwt_cols],
    type = "BRR",
    mse = TRUE,
    data = d
  )

  sc_mean <- get_means(sc, y1, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y1, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["y1"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})


# ---------------------------------------------------------------------------
# Block 12: na.rm = FALSE paths
# ---------------------------------------------------------------------------

test_that("get_means() na.rm = FALSE on replicate design covers .replicate_mean FALSE path", {
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    design = "replicate",
    type = "brr",
    seed = 200L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  sc <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )
  # na.rm = FALSE on NA-free data exercises the else branch and returns a valid estimate
  result <- get_means(sc, y1, variance = "se", na.rm = FALSE)
  expect_true(is.finite(result$mean[[1L]]))
  expect_true(is.finite(result$se[[1L]]))
})

test_that("get_totals() na.rm = FALSE on replicate design covers .replicate_total FALSE path", {
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    design = "replicate",
    type = "brr",
    seed = 201L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  sc <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )
  # na.rm = FALSE on NA-free data exercises the else branch and returns a valid estimate
  result <- get_totals(sc, y1, variance = "se", na.rm = FALSE)
  expect_true(is.finite(result$total[[1L]]))
  expect_true(is.finite(result$se[[1L]]))
})


# ---------------------------------------------------------------------------
# Block 16: .svy_rep_var() with NA replicates (lines 317–323)
# ---------------------------------------------------------------------------

test_that(".svy_rep_var() skips NA replicates and returns finite variance [direct]", {
  thetas <- c(1.2, 1.3, NA_real_, 1.1, 1.4)
  rscales <- rep(1L, 5L)
  v <- surveycore:::.svy_rep_var(
    thetas,
    scale = 0.2,
    rscales = rscales,
    mse = TRUE,
    coef = 1.25
  )
  expect_true(is.finite(v))
  expect_gte(v, 0)
})

test_that(".svy_rep_var() errors when all replicates are NA [direct]", {
  thetas <- rep(NA_real_, 5L)
  rscales <- rep(1L, 5L)
  expect_error(
    surveycore:::.svy_rep_var(
      thetas,
      scale = 0.2,
      rscales = rscales,
      mse = TRUE,
      coef = 1.25
    ),
    class = "surveycore_error_all_replicates_na"
  )
})

# ---------------------------------------------------------------------------
# Block 22: Direct .replicate_mean() and .replicate_total() calls
# ---------------------------------------------------------------------------

test_that(".replicate_mean() returns finite mean and se for BRR design", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 50
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )

  result <- surveycore:::.replicate_mean(sc, "y1")
  expect_true(is.finite(result$mean))
  expect_true(is.finite(result$se))
  expect_gte(result$se, 0)
  expect_true(is.finite(result$var))
})

test_that(".replicate_mean() na.rm = FALSE errors when all replicates produce NA", {
  d <- make_survey_data(
    n = 60,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 51
  )
  d$y1[[1L]] <- NA_real_
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )

  expect_error(
    surveycore:::.replicate_mean(sc, "y1", na.rm = FALSE),
    class = "surveycore_error_all_replicates_na"
  )
})

test_that(".replicate_total() returns finite total and se for BRR design", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 52
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )

  result <- surveycore:::.replicate_total(sc, "y1")
  expect_true(is.finite(result$total))
  expect_true(is.finite(result$se))
  expect_gte(result$se, 0)
})

test_that(".replicate_total() na.rm = FALSE errors when all replicates produce NA", {
  d <- make_survey_data(
    n = 60,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 53
  )
  d$y1[[2L]] <- NA_real_
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )

  expect_error(
    surveycore:::.replicate_total(sc, "y1", na.rm = FALSE),
    class = "surveycore_error_all_replicates_na"
  )
})

# ---------------------------------------------------------------------------
# Block 23: get_corr() with replicate design — covers .vcov_pair_replicate()
# ---------------------------------------------------------------------------

test_that("get_corr() works for survey_replicate (BRR) design", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 54
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )

  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_identical(as.character(result$var1[[1L]]), "y1")
  expect_identical(as.character(result$var2[[1L]]), "y2")
  expect_true(is.finite(result$r[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_corr() replicate returns NA for domain with fewer than 2 paired obs", {
  d <- make_survey_data(
    n = 60,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 55
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  # Only 1 row in domain → n_d < 2 → NA r
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(nrow(d)) == 1L

  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  expect_true(is.na(result$r[[1L]]))
})
