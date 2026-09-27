# test-variance-srs.R
# Tests for SRS variance estimation (Taylor with no ids/strata).
#
# Since survey_srs has been removed, SRS designs are created via
# as_survey(df, weights = w) which produces a survey_taylor object.
# The Taylor variance path with NULL ids/strata produces mathematically
# identical results to the former SRS-specific engine.
#
# Numerical tolerance: 1e-10 for point estimates, 1e-8 for SE/variance.
# Oracle tests skip if the 'survey' package is not installed.

# ---------------------------------------------------------------------------
# Block 2: SRS design (no ids, no strata) — oracle comparisons
# ---------------------------------------------------------------------------

test_that("get_means() matches survey::svymean() for uniform-weight SRS design", {
  skip_if_not_installed("survey")

  set.seed(42)
  df <- data.frame(y = rnorm(200), w = rep(1, 200))

  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)

  sc_mean <- get_means(sc, y, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean,    coef(sv_mean)[["y"]], tolerance = 1e-10)
  expect_equal(sc_mean$se,      as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low,  confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_means() SRS design: non-uniform weights match survey::svymean() [HT linearization]", {
  skip_if_not_installed("survey")
  set.seed(123)
  df <- data.frame(y = rnorm(100), w = runif(100, 0.5, 3))

  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sc_mean <- get_means(sc, y, variance = "se")
  sv_mean <- survey::svymean(~y, sv)

  expect_equal(sc_mean$mean[[1L]], as.numeric(coef(sv_mean)), tolerance = 1e-10)
  expect_equal(sc_mean$se[[1L]], as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Block 17: SRS variance — oracle comparison
# ---------------------------------------------------------------------------

test_that("get_means() SRS design: unweighted uniform weights match survey::svymean()", {
  skip_if_not_installed("survey")
  set.seed(42)
  df <- data.frame(y = rnorm(200), w = rep(1, 200))
  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sc_mean <- get_means(sc, y, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y, sv, na.rm = TRUE)
  expect_equal(sc_mean$mean,    coef(sv_mean)[["y"]], tolerance = 1e-10)
  expect_equal(sc_mean$se,      as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low,  confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_means() SRS design: non-uniform weights match survey::svymean() [formula verification]", {
  skip_if_not_installed("survey")
  set.seed(77)
  df <- data.frame(y = rnorm(150), w = runif(150, 0.5, 3))
  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sc_mean <- get_means(sc, y, variance = "se")
  sv_mean <- survey::svymean(~y, sv)
  expect_equal(sc_mean$mean[[1L]], as.numeric(coef(sv_mean)), tolerance = 1e-10)
  expect_equal(sc_mean$se[[1L]], as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
})

test_that("get_means() SRS design: population FPC matches survey::svymean() [proper SRS weights]", {
  # Oracle comparison requires w = N/n so HT and classical SRS formulas agree.
  skip_if_not_installed("survey")
  set.seed(11)
  N <- 500L; n <- 50L; w_val <- N / n   # = 10
  df <- data.frame(y = rnorm(n), w = rep(w_val, n), pop = rep(N, n))
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = pop))
  sv <- survey::svydesign(ids = ~1, weights = ~w, fpc = ~pop, data = df)
  sc_mean <- get_means(sc, y, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y, sv, na.rm = TRUE)
  expect_equal(sc_mean$mean,    coef(sv_mean)[["y"]], tolerance = 1e-10)
  expect_equal(sc_mean$se,      as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low,  confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_means() SRS design: fraction FPC matches survey::svymean() [proper SRS weights]", {
  # Oracle comparison requires w = N/n = 1/frac so formulas agree.
  skip_if_not_installed("survey")
  set.seed(22)
  n <- 60L; frac_val <- 0.1; w_val <- 1 / frac_val   # = 10
  df <- data.frame(y = rnorm(n), w = rep(w_val, n), frac = rep(frac_val, n))
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = frac))
  sv <- survey::svydesign(ids = ~1, weights = ~w, fpc = ~frac, data = df)
  sc_mean <- get_means(sc, y, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y, sv, na.rm = TRUE)
  expect_equal(sc_mean$mean,    coef(sv_mean)[["y"]], tolerance = 1e-10)
  expect_equal(sc_mean$se,      as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low,  confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_totals() SRS design: no FPC matches survey::svytotal() [uniform weights]", {
  skip_if_not_installed("survey")
  set.seed(33)
  df <- data.frame(y = rnorm(100), w = rep(5, 100))
  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sc_tot <- get_totals(sc, y, variance = c("se", "ci"))
  sv_tot <- survey::svytotal(~y, sv, na.rm = TRUE)
  expect_equal(sc_tot$total,   coef(sv_tot)[["y"]], tolerance = 1e-10)
  expect_equal(sc_tot$se,      as.numeric(survey::SE(sv_tot)), tolerance = 1e-8)
  expect_equal(sc_tot$ci_low,  confint(sv_tot)[1], tolerance = 1e-6)
  expect_equal(sc_tot$ci_high, confint(sv_tot)[2], tolerance = 1e-6)
})

test_that("get_totals() SRS design: population FPC matches survey::svytotal() [proper SRS weights]", {
  # Oracle comparison requires w = N/n so HT and classical SRS formulas agree.
  skip_if_not_installed("survey")
  set.seed(44)
  N <- 500L; n <- 50L; w_val <- N / n   # = 10
  df <- data.frame(y = rnorm(n), w = rep(w_val, n), pop = rep(N, n))
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = pop))
  sv <- survey::svydesign(ids = ~1, weights = ~w, fpc = ~pop, data = df)
  sc_tot <- get_totals(sc, y, variance = c("se", "ci"))
  sv_tot <- survey::svytotal(~y, sv, na.rm = TRUE)
  expect_equal(sc_tot$total,   coef(sv_tot)[["y"]], tolerance = 1e-10)
  expect_equal(sc_tot$se,      as.numeric(survey::SE(sv_tot)), tolerance = 1e-8)
  expect_equal(sc_tot$ci_low,  confint(sv_tot)[1], tolerance = 1e-6)
  expect_equal(sc_tot$ci_high, confint(sv_tot)[2], tolerance = 1e-6)
})

test_that("get_totals() SRS design: fraction FPC matches survey::svytotal() [proper SRS weights]", {
  # Oracle comparison requires w = N/n = 1/frac so formulas agree.
  skip_if_not_installed("survey")
  set.seed(55)
  n <- 60L; frac_val <- 0.1; w_val <- 1 / frac_val   # = 10
  df <- data.frame(y = rnorm(n), w = rep(w_val, n), frac = rep(frac_val, n))
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = frac))
  sv <- survey::svydesign(ids = ~1, weights = ~w, fpc = ~frac, data = df)
  sc_tot <- get_totals(sc, y, variance = c("se", "ci"))
  sv_tot <- survey::svytotal(~y, sv, na.rm = TRUE)
  expect_equal(sc_tot$total,   coef(sv_tot)[["y"]], tolerance = 1e-10)
  expect_equal(sc_tot$se,      as.numeric(survey::SE(sv_tot)), tolerance = 1e-8)
  expect_equal(sc_tot$ci_low,  confint(sv_tot)[1], tolerance = 1e-6)
  expect_equal(sc_tot$ci_high, confint(sv_tot)[2], tolerance = 1e-6)
})


# ---------------------------------------------------------------------------
# Block 18: SRS variance — structure and edge cases
# ---------------------------------------------------------------------------

test_that("get_means() returns correct structure for Taylor with no ids/strata", {
  df <- data.frame(y = 1:10, w = rep(2, 10))
  sc <- suppressWarnings(as_survey(df, weights = w))
  result <- get_means(sc, y, variance = "se")
  test_result_invariants(result, "survey_means")
  expect_identical(names(meta(result)$x), "y")
  expect_type(result$mean[[1L]], "double")
  expect_type(result$se[[1L]], "double")
  expect_true(is.finite(result$mean[[1L]]))
  expect_true(is.finite(result$se[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_totals() returns correct structure for Taylor with no ids/strata", {
  df <- data.frame(y = 1:10, w = rep(2, 10))
  sc <- suppressWarnings(as_survey(df, weights = w))
  result <- get_totals(sc, y, variance = "se")
  test_result_invariants(result, "survey_totals")
  expect_identical(names(meta(result)$x), "y")
  expect_type(result$total[[1L]], "double")
  expect_type(result$se[[1L]], "double")
  expect_true(is.finite(result$total[[1L]]))
  expect_true(is.finite(result$se[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_means() SRS design returns NA for all-NA column (edge case: n_used = 0)", {
  df <- data.frame(y = rep(NA_real_, 5), w = rep(1, 5))
  sc <- suppressWarnings(as_survey(df, weights = w))
  result <- get_means(sc, y, variance = "se", na.rm = TRUE)
  expect_true(is.na(result$mean[[1L]]))
  expect_true(is.na(result$se[[1L]]))
})

test_that("as_survey() rejects single-row SRS design", {
  df <- data.frame(y = 42, w = 1)
  expect_error(as_survey(df, weights = w), class = "surveycore_error_single_row")
})

test_that("get_means() SRS design na.rm = FALSE propagates NA when y has NA", {
  df <- data.frame(y = c(1, 2, NA_real_, 4, 5), w = rep(1, 5))
  sc <- suppressWarnings(as_survey(df, weights = w))
  result <- get_means(sc, y, variance = "se", na.rm = FALSE)
  expect_true(is.na(result$mean[[1L]]))
  expect_true(is.na(result$se[[1L]]))
})

test_that("get_totals() SRS design returns NA for all-NA column (edge case: n_used = 0)", {
  df <- data.frame(y = rep(NA_real_, 5), w = rep(1, 5))
  sc <- suppressWarnings(as_survey(df, weights = w))
  result <- get_totals(sc, y, variance = "se", na.rm = TRUE)
  expect_true(is.na(result$total[[1L]]))
  expect_true(is.na(result$se[[1L]]))
})

test_that("get_totals() SRS design na.rm = FALSE propagates NA when y has NA", {
  df <- data.frame(y = c(1, 2, NA_real_, 4, 5), w = rep(1, 5))
  sc <- suppressWarnings(as_survey(df, weights = w))
  result <- get_totals(sc, y, variance = "se", na.rm = FALSE)
  expect_true(is.na(result$total[[1L]]))
  expect_true(is.na(result$se[[1L]]))
})

test_that("get_means() SRS design: FPC = population size reduces SE vs no FPC", {
  set.seed(101)
  n <- 50L
  df <- data.frame(y = rnorm(n), w = rep(1, n), pop = rep(500L, n))
  sc_no_fpc   <- suppressWarnings(as_survey(df, weights = w))
  sc_with_fpc <- suppressWarnings(as_survey(df, weights = w, fpc = pop))
  r_no  <- get_means(sc_no_fpc, y, variance = "se")
  r_fpc <- get_means(sc_with_fpc, y, variance = "se")
  # FPC reduces SE: f = n/N = 50/500 = 0.1 -> SE_fpc = SE_no * sqrt(1 - 0.1) < SE_no
  expect_lt(r_fpc$se[[1L]], r_no$se[[1L]])
})

test_that("get_means() SRS design na.rm = FALSE with FPC returns NA when data has NA", {
  set.seed(42)
  df <- data.frame(
    y   = c(rnorm(49), NA_real_),
    w   = runif(50, 0.5, 2),
    fpc = rep(1000L, 50)
  )
  sc     <- suppressWarnings(as_survey(df, weights = w, fpc = fpc))
  result <- get_means(sc, y, na.rm = FALSE)
  expect_true(is.na(result$mean[[1L]]))
})

# ---------------------------------------------------------------------------
# Block 21: get_corr() with SRS design — Taylor with no ids/strata
# ---------------------------------------------------------------------------

test_that("get_corr() works for Taylor with no ids/strata (SRS design)", {
  set.seed(204)
  n <- 100L
  x <- rnorm(n); y <- x + rnorm(n, sd = 0.5)
  df <- data.frame(x = x, y = y, w = rep(1, n))
  sc <- suppressWarnings(as_survey(df, weights = w))
  result <- get_corr(sc, x = c(x, y), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_identical(as.character(result$var1[[1L]]), "x")
  expect_identical(as.character(result$var2[[1L]]), "y")
  expect_true(is.finite(result$r[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_corr() SRS design with FPC covers Taylor FPC path", {
  set.seed(205)
  n <- 80L; N <- 800L
  x <- rnorm(n); y <- x + rnorm(n, sd = 0.5)
  df <- data.frame(x = x, y = y, w = rep(N / n, n), pop = rep(N, n))
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = pop))
  result <- get_corr(sc, x = c(x, y), variance = "se")
  expect_true(is.finite(result$r[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_corr() SRS design returns NA when domain has fewer than 2 paired obs", {
  set.seed(206)
  n <- 20L
  df <- data.frame(
    x = c(rnorm(19L), NA_real_),
    y = c(rnorm(19L), NA_real_),
    w = rep(1, n)
  )
  sc <- suppressWarnings(as_survey(df, weights = w))
  # Only the last row is in domain; it has NA x and y, so paired n_d < 2
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- c(rep(FALSE, 19L), TRUE)
  result <- get_corr(sc, x = c(x, y), variance = "se")
  expect_true(is.na(result$r[[1L]]))
})

# ---------------------------------------------------------------------------
# Additional coverage: FPC fraction path in Taylor variance for SRS design
# ---------------------------------------------------------------------------

test_that("get_corr() SRS design with FPC fraction covers FPC fraction branch", {
  set.seed(901)
  n <- 80L
  frac <- 0.1
  df <- data.frame(
    x = rnorm(n), y = rnorm(n),
    w = rep(1 / frac, n),
    frac = rep(frac, n)
  )
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = frac))
  result <- get_corr(sc, x = c(x, y), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

# ---------------------------------------------------------------------------
# Regression oracle: confirms Taylor SRS output unchanged after refactor
# ---------------------------------------------------------------------------

test_that("get_means() on SRS design matches survey::svymean() after refactor [regression oracle]", {
  skip_if_not_installed("survey")
  set.seed(42)
  df <- data.frame(y1 = rnorm(200), wt = rep(1, 200))
  sc <- suppressWarnings(as_survey(df, weights = wt))
  sv <- survey::svydesign(id = ~1, weights = ~wt, data = df)
  sc_est <- get_means(sc, y1, variance = c("se", "ci"))
  sv_est <- survey::svymean(~y1, sv, na.rm = TRUE)
  expect_equal(
    sc_est$mean,
    coef(sv_est)[[1L]],
    tolerance = 1e-10
  )
  expect_equal(
    sc_est$se,
    as.numeric(survey::SE(sv_est)),
    tolerance = 1e-8
  )
})


# ---------------------------------------------------------------------------
# Block 22: Non-proportional weights — oracle tests (via public API)
# ---------------------------------------------------------------------------
# These tests verify that SRS variance estimation matches the survey package
# when weights are non-proportional (e.g., post-stratification adjustments).
# The classical SRS s^2 formula is only correct for equal weights; the HT
# Taylor linearization used by survey::svydesign(ids = ~1) handles any
# weight structure correctly.

test_that("get_means() SRS design: non-proportional weights match survey [oracle]", {
  skip_if_not_installed("survey")
  set.seed(42)
  n <- 50
  df <- data.frame(y = rnorm(n, 50, 10), w = c(rep(1, 25), rep(5, 25)))
  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sc_est <- get_means(sc, y, variance = "se")
  sv_est <- survey::svymean(~y, sv)
  expect_equal(sc_est$mean[[1L]], as.numeric(coef(sv_est)), tolerance = 1e-10)
  expect_equal(sc_est$se[[1L]], as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
})

test_that("get_totals() SRS design: non-proportional weights match survey [oracle]", {
  skip_if_not_installed("survey")
  set.seed(42)
  n <- 50
  df <- data.frame(y = rnorm(n, 50, 10), w = c(rep(1, 25), rep(5, 25)))
  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sc_est <- get_totals(sc, y, variance = "se")
  sv_est <- survey::svytotal(~y, sv)
  expect_equal(sc_est$total[[1L]], as.numeric(coef(sv_est)), tolerance = 1e-10)
  expect_equal(sc_est$se[[1L]], as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
})

test_that("get_means() SRS design: non-proportional weights + population FPC match survey [oracle]", {
  skip_if_not_installed("survey")
  set.seed(43)
  n <- 50; N <- 500
  df <- data.frame(
    y = rnorm(n, 50, 10),
    w = c(rep(1, 25), rep(5, 25)),
    pop = rep(N, n)
  )
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = pop))
  sv <- survey::svydesign(ids = ~1, weights = ~w, fpc = ~pop, data = df)
  sc_est <- get_means(sc, y, variance = "se")
  sv_est <- survey::svymean(~y, sv)
  expect_equal(sc_est$mean[[1L]], as.numeric(coef(sv_est)), tolerance = 1e-10)
  expect_equal(sc_est$se[[1L]], as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
})

test_that("get_totals() SRS design: non-proportional weights + population FPC match survey [oracle]", {
  skip_if_not_installed("survey")
  set.seed(43)
  n <- 50; N <- 500
  df <- data.frame(
    y = rnorm(n, 50, 10),
    w = c(rep(1, 25), rep(5, 25)),
    pop = rep(N, n)
  )
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = pop))
  sv <- survey::svydesign(ids = ~1, weights = ~w, fpc = ~pop, data = df)
  sc_est <- get_totals(sc, y, variance = "se")
  sv_est <- survey::svytotal(~y, sv)
  expect_equal(sc_est$total[[1L]], as.numeric(coef(sv_est)), tolerance = 1e-10)
  expect_equal(sc_est$se[[1L]], as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
})

test_that("get_means() SRS design: non-proportional weights + fraction FPC match survey [oracle]", {
  skip_if_not_installed("survey")
  set.seed(44)
  n <- 50; frac_val <- 0.1
  df <- data.frame(
    y = rnorm(n, 50, 10),
    w = c(rep(1, 25), rep(5, 25)),
    frac = rep(frac_val, n)
  )
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = frac))
  sv <- survey::svydesign(ids = ~1, weights = ~w, fpc = ~frac, data = df)
  sc_est <- get_means(sc, y, variance = "se")
  sv_est <- survey::svymean(~y, sv)
  expect_equal(sc_est$mean[[1L]], as.numeric(coef(sv_est)), tolerance = 1e-10)
  expect_equal(sc_est$se[[1L]], as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
})

test_that("get_totals() SRS design: non-proportional weights + fraction FPC match survey [oracle]", {
  skip_if_not_installed("survey")
  set.seed(44)
  n <- 50; frac_val <- 0.1
  df <- data.frame(
    y = rnorm(n, 50, 10),
    w = c(rep(1, 25), rep(5, 25)),
    frac = rep(frac_val, n)
  )
  sc <- suppressWarnings(as_survey(df, weights = w, fpc = frac))
  sv <- survey::svydesign(ids = ~1, weights = ~w, fpc = ~frac, data = df)
  sc_est <- get_totals(sc, y, variance = "se")
  sv_est <- survey::svytotal(~y, sv)
  expect_equal(sc_est$total[[1L]], as.numeric(coef(sv_est)), tolerance = 1e-10)
  expect_equal(sc_est$se[[1L]], as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
})

test_that("get_means() SRS design integration: non-proportional weights match survey [oracle]", {
  skip_if_not_installed("survey")
  set.seed(45)
  n <- 80
  df <- data.frame(y = rnorm(n, 100, 20), w = c(rep(2, 40), rep(10, 40)))
  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sc_est <- get_means(sc, y, variance = c("se", "ci"))
  sv_est <- survey::svymean(~y, sv)
  expect_equal(sc_est$mean, as.numeric(coef(sv_est)), tolerance = 1e-10)
  expect_equal(sc_est$se, as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
})

test_that("get_totals() SRS design integration: non-proportional weights match survey [oracle]", {
  skip_if_not_installed("survey")
  set.seed(45)
  n <- 80
  df <- data.frame(y = rnorm(n, 100, 20), w = c(rep(2, 40), rep(10, 40)))
  sc <- suppressWarnings(as_survey(df, weights = w))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sc_est <- get_totals(sc, y, variance = c("se", "ci"))
  sv_est <- survey::svytotal(~y, sv)
  expect_equal(sc_est$total, as.numeric(coef(sv_est)), tolerance = 1e-10)
  expect_equal(sc_est$se, as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
})
