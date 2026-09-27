# test-variance-taylor.R
# Tests for the Taylor series variance engine: .svy_recvar(), .taylor_build_inputs(),
# .taylor_mean(), .taylor_total().
#
# Numerical tolerance: 1e-10 for point estimates, 1e-8 for SE/variance.
# All oracle tests skip if the 'survey' package is not installed.

# ---------------------------------------------------------------------------
# Block 1: Taylor Series — NHANES stratified cluster design
# ---------------------------------------------------------------------------

test_that("get_means() Taylor SE matches survey::svymean() — NHANES", {
  skip_if_not_installed("survey")

  # Filter to complete-case examination respondents (wtmec2yr > 0)
  d <- nhanes_2017[nhanes_2017$ridstatr == 2, ]

  sc <- as_survey(
    d,
    ids = sdmvpsu,
    strata = sdmvstra,
    weights = wtmec2yr,
    nest = TRUE
  )
  sv <- survey::svydesign(
    ids = ~sdmvpsu,
    strata = ~sdmvstra,
    weights = ~wtmec2yr,
    data = d,
    nest = TRUE
  )

  sc_mean <- get_means(sc, bpxsy1, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~bpxsy1, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["bpxsy1"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_totals() Taylor SE matches survey::svytotal() — NHANES", {
  skip_if_not_installed("survey")

  d <- nhanes_2017[nhanes_2017$ridstatr == 2, ]

  sc <- as_survey(
    d,
    ids = sdmvpsu,
    strata = sdmvstra,
    weights = wtmec2yr,
    nest = TRUE
  )
  sv <- survey::svydesign(
    ids = ~sdmvpsu,
    strata = ~sdmvstra,
    weights = ~wtmec2yr,
    data = d,
    nest = TRUE
  )

  sc_total <- get_totals(sc, bpxsy1, variance = c("se", "ci"))
  sv_total <- survey::svytotal(~bpxsy1, sv, na.rm = TRUE)

  expect_equal(sc_total$total, coef(sv_total)[["bpxsy1"]], tolerance = 1e-10)
  expect_equal(sc_total$se, as.numeric(survey::SE(sv_total)), tolerance = 1e-8)
  expect_equal(sc_total$ci_low, confint(sv_total)[1], tolerance = 1e-6)
  expect_equal(sc_total$ci_high, confint(sv_total)[2], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Block 3: Stratified design (no clusters)
# ---------------------------------------------------------------------------

test_that("get_means() matches survey::svymean() for stratified design", {
  skip_if_not_installed("survey")

  set.seed(99)
  df <- data.frame(
    strat = rep(c("A", "B", "C"), each = 30),
    wt = c(runif(30, 1, 2), runif(30, 2, 4), runif(30, 0.5, 1.5)),
    y = rnorm(90)
  )

  sc <- as_survey(df, strata = strat, weights = wt)
  sv <- survey::svydesign(ids = ~1, strata = ~strat, weights = ~wt, data = df)

  sc_mean <- get_means(sc, y, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["y"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

test_that("get_totals() matches survey::svytotal() for stratified design", {
  skip_if_not_installed("survey")

  set.seed(77)
  df <- data.frame(
    strat = rep(c("X", "Y"), each = 20),
    wt = c(runif(20, 1, 3), runif(20, 2, 5)),
    y = rnorm(40)
  )

  sc <- as_survey(df, strata = strat, weights = wt)
  sv <- survey::svydesign(ids = ~1, strata = ~strat, weights = ~wt, data = df)

  sc_total <- get_totals(sc, y, variance = c("se", "ci"))
  sv_total <- survey::svytotal(~y, sv, na.rm = TRUE)

  expect_equal(sc_total$total, coef(sv_total)[["y"]], tolerance = 1e-10)
  expect_equal(sc_total$se, as.numeric(survey::SE(sv_total)), tolerance = 1e-8)
  expect_equal(sc_total$ci_low, confint(sv_total)[1], tolerance = 1e-6)
  expect_equal(sc_total$ci_high, confint(sv_total)[2], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Block 4: Clustered design without stratification
# ---------------------------------------------------------------------------

test_that("get_means() matches survey::svymean() for cluster-only design", {
  skip_if_not_installed("survey")

  set.seed(55)
  df <- data.frame(
    psu = rep(1:10, each = 5),
    wt = rep(runif(10, 1, 4), each = 5),
    y = rnorm(50)
  )

  sc <- as_survey(df, ids = psu, weights = wt)
  sv <- survey::svydesign(ids = ~psu, weights = ~wt, data = df)

  sc_mean <- get_means(sc, y, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["y"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Block 5: Design with FPC
# ---------------------------------------------------------------------------

test_that("get_means() matches survey::svymean() with population-size FPC", {
  skip_if_not_installed("survey")

  set.seed(8)
  df <- data.frame(
    psu = rep(1:4, each = 5),
    st = rep(c("A", "B"), each = 10),
    wt = rep(c(100, 150, 200, 250), each = 5),
    fpc = rep(c(8L, 8L, 12L, 12L), each = 5), # population PSU count
    y = rnorm(20)
  )

  sc <- as_survey(df, ids = psu, strata = st, weights = wt, fpc = fpc)
  sv <- survey::svydesign(
    ids = ~psu,
    strata = ~st,
    weights = ~wt,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )

  sc_mean <- get_means(sc, y, variance = c("se", "ci"))
  sv_mean <- survey::svymean(~y, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["y"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Block 6: na.rm behaviour
# ---------------------------------------------------------------------------

test_that("get_means() na.rm = TRUE matches survey::svymean() with NAs", {
  skip_if_not_installed("survey")

  set.seed(321)
  df <- data.frame(
    psu = rep(1:5, each = 4),
    st = rep(c("A", "B", "A", "B", "A"), each = 4),
    wt = rep(runif(5, 1, 3), each = 4),
    y = c(rnorm(10), rep(NA_real_, 2), rnorm(8))
  )

  sc <- as_survey(df, ids = psu, strata = st, weights = wt)
  sv <- survey::svydesign(
    ids = ~psu,
    strata = ~st,
    weights = ~wt,
    data = df,
    nest = TRUE
  )

  sc_mean <- get_means(sc, y, variance = c("se", "ci"), na.rm = TRUE)
  sv_mean <- survey::svymean(~y, sv, na.rm = TRUE)

  expect_equal(sc_mean$mean, coef(sv_mean)[["y"]], tolerance = 1e-10)
  expect_equal(sc_mean$se, as.numeric(survey::SE(sv_mean)), tolerance = 1e-8)
  expect_equal(sc_mean$ci_low, confint(sv_mean)[1], tolerance = 1e-6)
  expect_equal(sc_mean$ci_high, confint(sv_mean)[2], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Block 13: Fraction FPC (sampling fraction path in .taylor_build_inputs)
# ---------------------------------------------------------------------------

test_that("get_means() Taylor: fraction FPC (values <= 1) converts to population sizes", {
  skip_if_not_installed("survey")
  set.seed(42)
  n <- 100L
  # Add psu column (each row its own PSU) so as_survey() uses the Taylor path,
  # not the SRS fallback.  This test verifies .taylor_build_inputs() FPC conversion.
  df <- data.frame(
    psu = seq_len(n),
    y = rnorm(n),
    w = runif(n, 0.5, 2),
    fpc_frac = rep(0.1, n) # sampling fraction
  )
  sc <- as_survey(df, ids = psu, weights = w, fpc = fpc_frac)
  sv <- survey::svydesign(ids = ~psu, weights = ~w, fpc = ~fpc_frac, data = df)
  sc_est <- get_means(sc, y, variance = c("se", "ci"))
  sv_est <- survey::svymean(~y, sv, na.rm = TRUE)
  expect_equal(sc_est$mean, coef(sv_est)[["y"]], tolerance = 1e-10)
  expect_equal(sc_est$se, as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
  expect_equal(sc_est$ci_low, confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Block 14: Lonely PSU — 1-PSU stratum covers .svy_onestrat switch (lines 62-74)
# ---------------------------------------------------------------------------

# Shared fixture: design where stratum B has exactly 1 PSU
make_lonely_design <- function() {
  set.seed(42)
  data.frame(
    y = rnorm(30),
    w = runif(30, 0.5, 2),
    strata = c(rep("A", 20), rep("B", 10)),
    psu = c(rep(1L, 10), rep(2L, 10), rep(3L, 10))
  )
}

test_that("get_means() works with lonely PSU (lonely.psu = 'remove', default)", {
  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "remove")
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)
  df <- make_lonely_design()
  sc <- as_survey(df, ids = psu, weights = w, strata = strata, nest = TRUE)
  result <- get_means(sc, y)
  expect_true(is.finite(result$mean))
})

test_that("get_means() works with lonely PSU (lonely.psu = 'certainty')", {
  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "certainty")
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)
  df <- make_lonely_design()
  sc <- as_survey(df, ids = psu, weights = w, strata = strata, nest = TRUE)
  result <- get_means(sc, y)
  expect_true(is.finite(result$mean))
})

test_that("get_means() works with lonely PSU (lonely.psu = 'average')", {
  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "average")
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)
  df <- make_lonely_design()
  sc <- as_survey(df, ids = psu, weights = w, strata = strata, nest = TRUE)
  result <- get_means(sc, y)
  # Mean is still finite; SE may be NA due to NA variance contribution
  expect_true(is.finite(result$mean))
})

test_that("get_means() works with lonely PSU (lonely.psu = 'adjust')", {
  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)
  df <- make_lonely_design()
  sc <- as_survey(df, ids = psu, weights = w, strata = strata, nest = TRUE)
  # suppressWarnings: base R "NAs introduced by coercion" is expected for 'adjust'
  result <- suppressWarnings(get_means(sc, y))
  expect_true(is.finite(result$mean))
})

test_that("get_means() errors with lonely PSU (lonely.psu = 'fail')", {
  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "fail")
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)
  df <- make_lonely_design()
  sc <- as_survey(df, ids = psu, weights = w, strata = strata, nest = TRUE)
  expect_error(
    get_means(sc, y),
    class = "surveycore_error_lonely_psu"
  )
})

test_that("get_means() errors with unknown lonely.psu option — surveycore_error_lonely_psu_unknown_option", {
  old_opt <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "bogus_option")
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)
  df <- make_lonely_design()
  sc <- as_survey(df, ids = psu, weights = w, strata = strata, nest = TRUE)
  expect_error(
    get_means(sc, y),
    class = "surveycore_error_lonely_psu_unknown_option"
  )
  expect_snapshot(error = TRUE, get_means(sc, y))
})

# ---------------------------------------------------------------------------
# Block 15: Complete census FPC (f ≈ 0) — .svy_onestrat line 37
# ---------------------------------------------------------------------------

test_that("get_means() returns zero SE when FPC indicates complete census (fpc = nPSU)", {
  # fpc == nPSU for each stratum → f = (fpc-nPSU)/fpc = 0 → variance is 0
  df <- data.frame(
    y = rnorm(20),
    w = rep(1, 20),
    strata = rep(c("A", "B"), each = 10),
    psu = rep(1:5, 4), # 5 unique PSUs per stratum
    fpc = rep(5L, 20) # fpc == nPSU
  )
  sc <- as_survey(
    df,
    ids = psu,
    weights = w,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  result <- get_means(sc, y, variance = "se")
  expect_equal(result$se, 0, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Block 14: Direct .taylor_mean() and .taylor_total() coverage
# ---------------------------------------------------------------------------

test_that(".taylor_mean() returns finite result for stratified cluster design", {
  df <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 60)
  sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  result <- surveycore:::.taylor_mean(sc, "y1")
  expect_true(is.finite(result$mean))
  expect_true(is.finite(result$se))
  expect_gte(result$se, 0)
  expect_true(is.finite(result$var))
})

test_that(".taylor_mean() na.rm = FALSE propagates NA when y has NA", {
  df <- make_survey_data(n = 60, n_psu = 10, n_strata = 2, seed = 61)
  df$y1[[1L]] <- NA_real_
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- surveycore:::.taylor_mean(sc, "y1", na.rm = FALSE)
  expect_true(is.na(result$mean))
})

test_that(".taylor_total() returns finite result for stratified cluster design", {
  df <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 62)
  sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  result <- surveycore:::.taylor_total(sc, "y1")
  expect_true(is.finite(result$total))
  expect_true(is.finite(result$se))
  expect_gte(result$se, 0)
})

test_that(".taylor_total() na.rm = FALSE propagates NA", {
  df <- make_survey_data(n = 60, n_psu = 10, n_strata = 2, seed = 63)
  df$y1[[2L]] <- NA_real_
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- surveycore:::.taylor_total(sc, "y1", na.rm = FALSE)
  expect_true(is.na(result$total))
})

test_that(".taylor_build_inputs() FPC fraction path: popsize_mat built from fractions", {
  # FPC values in (0,1] are fractions → converted to N/n
  set.seed(64)
  n_psu <- 10L
  n_strata <- 2L
  df <- make_survey_data(n = 100, n_psu = n_psu, n_strata = n_strata, seed = 64)
  # Replace integer FPC with sampling fractions
  df$fpc_frac <- df$fpc / (df$fpc * 3) # fraction < 1
  sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc_frac,
    nest = TRUE
  )

  result <- surveycore:::.taylor_mean(sc, "y1")
  expect_true(is.finite(result$mean))
  expect_true(is.finite(result$se))
})

test_that(".taylor_build_inputs() nest=TRUE path makes PSU IDs globally unique", {
  # Two strata each using PSU IDs 1-5: without nest=TRUE these would collide.
  # .taylor_build_inputs() should create unique IDs via interaction().
  set.seed(65)
  df <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 65)
  sc_nest <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    nest = TRUE
  )
  sc_flat <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    nest = FALSE
  )

  # Both should give finite results; with nest=TRUE SE may differ
  r_nest <- surveycore:::.taylor_mean(sc_nest, "y1")
  r_flat <- surveycore:::.taylor_mean(sc_flat, "y1")
  expect_equal(r_nest$mean, r_flat$mean, tolerance = 1e-14)
  expect_true(is.finite(r_nest$se))
  expect_true(is.finite(r_flat$se))
})

# ---------------------------------------------------------------------------
# Block 15: get_corr() with Taylor + nest=TRUE + FPC fraction
# ---------------------------------------------------------------------------

test_that("get_corr() with nest=TRUE covers .vcov_pair_taylor() nest branch", {
  df <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 66)
  sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_corr() with FPC fraction covers .vcov_pair_taylor() FPC fraction path", {
  set.seed(67)
  df <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 67)
  df$fpc_frac <- df$fpc / (df$fpc * 3) # fractions in (0,1)
  sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc_frac,
    nest = TRUE
  )
  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  expect_true(is.finite(result$r[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

# ---------------------------------------------------------------------------
# Additional coverage: .taylor_build_inputs() no-strata / no-ids branches;
# .vcov_pair_taylor() corresponding branches
# ---------------------------------------------------------------------------

test_that(".taylor_mean() with ids-only design covers no-strata branch (line 199)", {
  # ids but no strata → strata_id = rep(1L, n)
  set.seed(901)
  df <- data.frame(
    psu = rep(1:5, each = 10),
    y = rnorm(50),
    wt = runif(50, 0.5, 2)
  )
  sc <- as_survey(df, ids = psu, weights = wt) # no strata → survey_taylor
  result <- surveycore:::.taylor_mean(sc, "y")
  expect_true(is.finite(result$mean))
})

test_that(".taylor_total() with strata-only design covers no-ids branch (line 210)", {
  # strata but no ids → psu_id = seq_len(n)
  set.seed(902)
  df <- data.frame(
    strata = rep(c("A", "B"), each = 25),
    y = rnorm(50),
    wt = runif(50, 0.5, 2)
  )
  sc <- as_survey(df, weights = wt, strata = strata) # no ids → survey_taylor
  result <- surveycore:::.taylor_total(sc, "y")
  expect_true(is.finite(result$total))
})

test_that("get_corr() taylor with ids but no strata covers .vcov_pair_taylor() no-strata branch", {
  set.seed(903)
  df <- data.frame(
    psu = rep(1:10, each = 8),
    y1 = rnorm(80),
    y2 = rnorm(80),
    wt = runif(80, 0.5, 2)
  )
  sc <- as_survey(df, ids = psu, weights = wt) # no strata
  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
})

test_that("get_corr() taylor with strata but no ids covers .vcov_pair_taylor() no-ids branch", {
  set.seed(904)
  df <- data.frame(
    strata = rep(c("A", "B"), each = 40),
    y1 = rnorm(80),
    y2 = rnorm(80),
    wt = runif(80, 0.5, 2)
  )
  sc <- as_survey(df, weights = wt, strata = strata) # no ids
  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
})

test_that("get_corr() taylor nest=FALSE covers raw_ids branch in .vcov_pair_taylor()", {
  set.seed(905)
  df <- make_survey_data(n = 80, n_psu = 10, n_strata = 2, seed = 905)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = FALSE)
  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
})

# ---------------------------------------------------------------------------
# .build_cluster_matrices() unit tests
# ---------------------------------------------------------------------------

test_that(".build_cluster_matrices() returns n x 1 matrices for single-stage design [unit]", {
  df <- make_survey_data(n = 100, n_psu = 10, seed = 1)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  test_invariants(sc)
  mats <- surveycore:::.build_cluster_matrices(sc@data, sc@variables)
  expect_identical(dim(mats$clusters_mat), c(100L, 1L))
  expect_identical(dim(mats$strata_mat), c(100L, 1L))
  expect_identical(dim(mats$fpcs$sampsize), c(100L, 1L))
  expect_identical(dim(mats$fpcs$popsize), c(100L, 1L))
})

test_that(".build_cluster_matrices() returns n x 2 matrices for 2-stage design [unit]", {
  df <- make_survey_data(n = 200, n_psu = 20, n_ssu = 5, seed = 1)
  vars <- list(
    ids = c("psu", "ssu"),
    weights = "wt",
    strata = "strata",
    fpc = c("fpc", "fpc2"),
    nest = TRUE
  )
  mats <- surveycore:::.build_cluster_matrices(df, vars)
  expect_identical(dim(mats$clusters_mat), c(200L, 2L))
  expect_identical(dim(mats$strata_mat), c(200L, 2L))
  expect_identical(dim(mats$fpcs$sampsize), c(200L, 2L))
  expect_identical(dim(mats$fpcs$popsize), c(200L, 2L))
})

test_that(".build_cluster_matrices() returns n x 3 matrices for 3-stage design [unit]", {
  df <- make_survey_data(
    n = 300,
    n_psu = 10,
    n_ssu = 5,
    n_unit = 3,
    seed = 1
  )
  vars <- list(
    ids = c("psu", "ssu", "unit"),
    weights = "wt",
    strata = "strata",
    fpc = NULL,
    nest = FALSE
  )
  mats <- surveycore:::.build_cluster_matrices(df, vars)
  expect_identical(dim(mats$clusters_mat), c(300L, 3L))
  expect_null(mats$fpcs$popsize)
})

test_that(".build_cluster_matrices() assigns each row its own PSU when ids is NULL [unit]", {
  df <- make_survey_data(n = 50, seed = 1)
  vars <- list(
    ids = NULL,
    weights = "wt",
    strata = NULL,
    fpc = NULL,
    nest = FALSE
  )
  mats <- surveycore:::.build_cluster_matrices(df, vars)
  expect_identical(mats$clusters_mat[, 1L], seq_len(50L))
})

test_that(".build_cluster_matrices() applies nest adjustment only at stage 1 [unit]", {
  # Construct data where PSU IDs are NOT globally unique across strata
  # (same numeric IDs 1-5 appear in both strata A and B).
  # Only nest = TRUE should disambiguate them.

  df <- data.frame(
    psu = rep(1:5, each = 10), # IDs 1-5 repeated across strata
    ssu = rep(1:2, 25),
    strata = rep(c("A", "B"), each = 25),
    wt = rep(1, 50)
  )
  vars_nest <- list(
    ids = c("psu", "ssu"),
    weights = "wt",
    strata = "strata",
    fpc = NULL,
    nest = TRUE
  )
  vars_no_nest <- list(
    ids = c("psu", "ssu"),
    weights = "wt",
    strata = "strata",
    fpc = NULL,
    nest = FALSE
  )
  mats_nest <- surveycore:::.build_cluster_matrices(df, vars_nest)
  mats_no_nest <- surveycore:::.build_cluster_matrices(df, vars_no_nest)
  # Stage 1 differs (nest interaction changes PSU IDs)
  expect_false(
    identical(mats_nest$clusters_mat[, 1L], mats_no_nest$clusters_mat[, 1L])
  )
  # nest = TRUE should produce more unique PSU IDs than nest = FALSE
  expect_gt(
    length(unique(mats_nest$clusters_mat[, 1L])),
    length(unique(mats_no_nest$clusters_mat[, 1L]))
  )
})

test_that(".build_cluster_matrices() fills Inf for stages beyond n_fpc [unit]", {
  df <- make_survey_data(n = 200, n_psu = 20, n_ssu = 5, seed = 1)
  vars <- list(
    ids = c("psu", "ssu"),
    weights = "wt",
    strata = "strata",
    fpc = "fpc",
    nest = FALSE # only stage-1 FPC
  )
  mats <- surveycore:::.build_cluster_matrices(df, vars)
  expect_true(all(mats$fpcs$popsize[, 2L] == Inf))
})

test_that(".build_cluster_matrices() sampsize matches PSU/SSU counts [unit]", {
  df <- make_survey_data(n = 200, n_psu = 20, n_ssu = 5, seed = 1)
  vars <- list(
    ids = c("psu", "ssu"),
    weights = "wt",
    strata = "strata",
    fpc = NULL,
    nest = FALSE
  )
  mats <- surveycore:::.build_cluster_matrices(df, vars)
  # Col 2: each row gets the number of unique SSUs in its PSU
  psu_col <- mats$clusters_mat[, 1L]
  ssu_col <- mats$clusters_mat[, 2L]
  expected_ssu_n <- tapply(ssu_col, psu_col, function(x) length(unique(x)))
  actual <- mats$fpcs$sampsize[, 2L]
  expect_identical(
    actual,
    as.integer(expected_ssu_n[as.character(psu_col)])
  )
})

# ---------------------------------------------------------------------------
# Block 16: Multi-stage Taylor refactor — regression oracle + shape tests
# ---------------------------------------------------------------------------

test_that(
  paste(
    "get_means() on single-stage NHANES matches survey::svymean()",
    "[regression oracle]"
  ),
  {
    skip_if_not_installed("survey")
    d <- nhanes_2017[nhanes_2017$ridstatr == 2, ]
    sc <- as_survey(
      d,
      ids = sdmvpsu,
      weights = wtmec2yr,
      strata = sdmvstra,
      nest = TRUE
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~sdmvpsu,
      weights = ~wtmec2yr,
      strata = ~sdmvstra,
      data = d,
      nest = TRUE
    )
    sc_result <- get_means(sc, bpxsy1, variance = "se")
    sv_result <- survey::svymean(~bpxsy1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["bpxsy1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    ".taylor_build_inputs() returns n x 1 matrices for",
    "current single-stage design [shape]"
  ),
  {
    df <- make_survey_data(n = 100, n_psu = 10, seed = 70)
    sc <- as_survey(
      df,
      ids = psu,
      weights = wt,
      strata = strata,
      fpc = fpc
    )
    test_invariants(sc)
    inp <- surveycore:::.taylor_build_inputs(sc, "y1")
    expect_identical(ncol(inp$clusters), 1L)
    expect_identical(ncol(inp$stratas), 1L)
    expect_identical(ncol(inp$fpcs$sampsize), 1L)
  }
)

test_that(
  paste(
    ".taylor_build_inputs() returns n x 2 matrices for",
    "2-stage design without FPC [shape]"
  ),
  {
    df <- make_survey_data(
      n = 300,
      n_psu = 30,
      n_ssu = 5,
      seed = 1
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    inp <- surveycore:::.taylor_build_inputs(sc, "y1")
    expect_identical(ncol(inp$clusters), 2L)
    expect_identical(ncol(inp$stratas), 2L)
    expect_identical(ncol(inp$fpcs$sampsize), 2L)
  }
)

test_that(
  paste(
    ".taylor_build_inputs() computes sampsize from full dataset",
    "before na.rm filtering [semantics]"
  ),
  {
    df <- make_survey_data(n = 100, n_psu = 10, seed = 71)
    # Add NAs to y1
    df$y1[1:10] <- NA_real_
    sc <- as_survey(
      df,
      ids = psu,
      weights = wt,
      strata = strata,
      fpc = fpc
    )
    inp_narm <- surveycore:::.taylor_build_inputs(
      sc,
      "y1",
      na.rm = TRUE
    )
    inp_full <- surveycore:::.taylor_build_inputs(
      sc,
      "y1",
      na.rm = FALSE
    )
    # After na.rm, we have fewer rows, but sampsize values should
    # reflect full-data PSU counts — matching survey package semantics.
    n_narm <- length(inp_narm$y)
    n_full <- length(inp_full$y)
    expect_lt(n_narm, n_full)
    # sampsize values in na.rm output should come from the full
    # dataset's PSU counts
    expect_true(
      all(
        unique(inp_narm$fpcs$sampsize[, 1L]) %in%
          unique(inp_full$fpcs$sampsize[, 1L])
      )
    )
  }
)

test_that(
  paste(
    "get_means() on 2-stage design (no FPC) matches",
    "survey::svymean() [multi-stage oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 300,
      n_psu = 30,
      n_ssu = 5,
      seed = 72
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      strata = ~strata,
      weights = ~wt,
      data = df
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_totals() on 2-stage design (no FPC) matches",
    "survey::svytotal() [multi-stage oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 300,
      n_psu = 30,
      n_ssu = 5,
      seed = 73
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      strata = ~strata,
      weights = ~wt,
      data = df
    )
    sc_result <- get_totals(sc, y1, variance = "se")
    sv_result <- survey::svytotal(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$total,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() on 2-stage design with NAs matches survey",
    "[multi-stage na.rm oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 300,
      n_psu = 30,
      n_ssu = 5,
      seed = 74
    )
    # Introduce NAs
    df$y1[c(1, 50, 100, 200)] <- NA_real_
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      strata = ~strata,
      weights = ~wt,
      data = df
    )
    sc_result <- get_means(sc, y1, variance = "se", na.rm = TRUE)
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

# ---------------------------------------------------------------------------
# Block 17: Multi-stage analysis helpers — full oracle suite (PR 5)
# ---------------------------------------------------------------------------

test_that(
  paste(
    "get_means() matches survey::svymean() for 2-stage,",
    "FPC at stage 1 only [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 500,
      n_psu = 50,
      n_ssu = 10,
      seed = 42
    )
    expect_warning(
      sc <- as_survey(
        df,
        ids = c(psu, ssu),
        weights = wt,
        strata = strata,
        fpc = fpc
      ),
      class = "surveycore_warning_fpc_partial_stages"
    )
    test_invariants(sc)
    # survey pkg needs one FPC column per stage; use Inf for
    # stages without FPC (= infinite population, no correction)
    df$fpc2_inf <- Inf
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      fpc = ~ fpc + fpc2_inf,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() matches survey::svymean() for 2-stage,",
    "FPC at both stages [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 500,
      n_psu = 50,
      n_ssu = 10,
      seed = 42
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata,
      fpc = c(fpc, fpc2)
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      fpc = ~ fpc + fpc2,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_freqs() matches survey::svymean() for 2-stage",
    "binary variable [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 500,
      n_psu = 50,
      n_ssu = 10,
      seed = 42
    )
    # y3 is numeric 0/1; create a factor for freqs
    df$y3_fac <- factor(df$y3, levels = c(0L, 1L))
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_freqs(sc, y3_fac, variance = "se")
    sv_result <- survey::svymean(~y3_fac, sv, na.rm = TRUE)
    # Compare proportion for level "1" (row 2 in output)
    sc_p1 <- sc_result$pct[2L]
    sv_p1 <- coef(sv_result)[["y3_fac1"]]
    expect_equal(sc_p1, sv_p1, tolerance = 1e-10)
    sc_se1 <- sc_result$se[2L]
    sv_se1 <- as.numeric(survey::SE(sv_result))[2L]
    expect_equal(sc_se1, sv_se1, tolerance = 1e-8)
  }
)

test_that(
  paste(
    "get_totals() matches survey::svytotal() for 2-stage",
    "design, seed 42 [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 500,
      n_psu = 50,
      n_ssu = 10,
      seed = 42
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_totals(sc, y1, variance = "se")
    sv_result <- survey::svytotal(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$total,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_corr() matches cov2cor(survey::svyvar()) for",
    "2-stage design [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 500,
      n_psu = 50,
      n_ssu = 10,
      seed = 42
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_corr(sc, x = c(y1, y2), variance = "se")
    sv_var <- survey::svyvar(~ y1 + y2, sv, na.rm = TRUE)
    sv_cor_mat <- cov2cor(as.matrix(sv_var))
    expect_equal(
      sc_result$r[[1L]],
      sv_cor_mat["y1", "y2"],
      tolerance = 1e-10
    )
  }
)

test_that(
  paste(
    "get_quantiles() matches survey::svyquantile() for",
    "2-stage design [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 500,
      n_psu = 50,
      n_ssu = 10,
      seed = 42
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_quantiles(sc, y1, probs = 0.5, variance = "se")
    sv_result <- survey::svyquantile(
      ~y1,
      sv,
      quantiles = 0.5,
      na.rm = TRUE
    )
    expect_equal(
      sc_result$estimate[[1L]],
      as.numeric(coef(sv_result)),
      tolerance = 1e-6
    )
  }
)

test_that(
  paste(
    "get_ratios() matches survey::svyratio() for",
    "2-stage design [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 500,
      n_psu = 50,
      n_ssu = 10,
      seed = 42
    )
    # Ensure positive denominator
    df$y2_pos <- abs(df$y2) + 1
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_ratios(
      sc,
      numerator = y1,
      denominator = y2_pos,
      variance = "se"
    )
    sv_result <- survey::svyratio(~y1, ~y2_pos, sv, na.rm = TRUE)
    expect_equal(
      sc_result$ratio[[1L]],
      as.numeric(coef(sv_result)),
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se[[1L]],
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() matches survey::svymean() for 3-stage,",
    "no FPC [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 300,
      n_psu = 10,
      n_ssu = 5,
      n_unit = 3,
      seed = 42
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu, unit),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu + unit,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() matches survey::svymean() for 3-stage,",
    "FPC at stage 1 only [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 300,
      n_psu = 10,
      n_ssu = 5,
      n_unit = 3,
      seed = 42
    )
    expect_warning(
      sc <- as_survey(
        df,
        ids = c(psu, ssu, unit),
        weights = wt,
        strata = strata,
        fpc = fpc
      ),
      class = "surveycore_warning_fpc_partial_stages"
    )
    test_invariants(sc)
    # survey pkg needs one FPC column per stage; use Inf for
    # stages without FPC (= infinite population, no correction)
    df$fpc2_inf <- Inf
    df$fpc3_inf <- Inf
    sv <- survey::svydesign(
      id = ~ psu + ssu + unit,
      weights = ~wt,
      strata = ~strata,
      fpc = ~ fpc + fpc2_inf + fpc3_inf,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() matches survey::svymean() for 3-stage,",
    "FPC at all stages [oracle]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 300,
      n_psu = 10,
      n_ssu = 5,
      n_unit = 3,
      seed = 42
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu, unit),
      weights = wt,
      strata = strata,
      fpc = c(fpc, fpc2, fpc3)
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu + unit,
      weights = ~wt,
      strata = ~strata,
      fpc = ~ fpc + fpc2 + fpc3,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

# ---------------------------------------------------------------------------
# Block 18: Multi-stage edge cases (PR 5)
# ---------------------------------------------------------------------------

test_that(
  paste(
    "get_means() 2-stage with nest = TRUE matches",
    "survey [edge case]"
  ),
  {
    skip_if_not_installed("survey")
    # PSU IDs reused across strata
    set.seed(100)
    df <- data.frame(
      psu = rep(1:5, each = 20),
      ssu = rep(1:4, 25),
      strata = rep(c("A", "B"), each = 50),
      wt = runif(100, 1, 3),
      y1 = rnorm(100, 50, 10)
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata,
      nest = TRUE
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() 2-stage, single SSU per PSU matches",
    "survey [edge case]"
  ),
  {
    skip_if_not_installed("survey")
    # Each PSU has exactly 1 SSU — lonely PSU at stage 2
    set.seed(101)
    n_psu <- 20L
    df <- data.frame(
      psu = rep(seq_len(n_psu), each = 5),
      ssu = rep(seq_len(n_psu), each = 5), # 1 SSU per PSU
      strata = rep(c("A", "B"), each = 50),
      wt = runif(100, 1, 3),
      y1 = rnorm(100, 50, 10)
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata,
      nest = TRUE
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() 2-stage with na.rm = TRUE, all-NA PSU",
    "matches survey [edge case]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 300,
      n_psu = 30,
      n_ssu = 5,
      seed = 102
    )
    # Set all y1 values in first PSU to NA
    first_psu <- df$psu[[1L]]
    df$y1[df$psu == first_psu] <- NA_real_
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se", na.rm = TRUE)
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() 2-stage with domain estimation matches",
    "survey [edge case]"
  ),
  {
    skip_if_not_installed("survey")
    skip_if_not_installed("surveytidy")
    df <- make_survey_data(
      n = 300,
      n_psu = 30,
      n_ssu = 5,
      seed = 103
    )
    df$domain <- ifelse(
      df$strata %in% unique(df$strata)[1:3],
      1L,
      0L
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sc_dom <- surveytidy::filter(sc, domain == 1L)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc_dom, y1, variance = "se")
    sv_result <- survey::svymean(
      ~y1,
      subset(sv, domain == 1L),
      na.rm = TRUE
    )
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() 2-stage domain + na.rm = TRUE",
    "matches survey [edge case]"
  ),
  {
    skip_if_not_installed("survey")
    skip_if_not_installed("surveytidy")
    df <- make_survey_data(
      n = 300,
      n_psu = 30,
      n_ssu = 5,
      seed = 104
    )
    df$domain <- ifelse(
      df$strata %in% unique(df$strata)[1:3],
      1L,
      0L
    )
    df$y1[c(1, 50, 100)] <- NA_real_
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sc_dom <- surveytidy::filter(sc, domain == 1L)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(
      sc_dom,
      y1,
      variance = "se",
      na.rm = TRUE
    )
    sv_result <- survey::svymean(
      ~y1,
      subset(sv, domain == 1L),
      na.rm = TRUE
    )
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)

test_that(
  paste(
    "get_means() 2-stage grouped estimate matches",
    "survey::svyby() [edge case]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(
      n = 500,
      n_psu = 50,
      n_ssu = 10,
      seed = 42
    )
    sc <- as_survey(
      df,
      ids = c(psu, ssu),
      weights = wt,
      strata = strata
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~ psu + ssu,
      weights = ~wt,
      strata = ~strata,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, group = strata, variance = "se")
    sv_result <- survey::svyby(
      ~y1,
      ~strata,
      sv,
      survey::svymean,
      na.rm = TRUE
    )
    # Compare each stratum
    for (st in unique(df$strata)) {
      sc_row <- sc_result[sc_result$strata == st, ]
      sv_row <- sv_result[sv_result$strata == st, ]
      expect_equal(
        sc_row$mean,
        sv_row$y1,
        tolerance = 1e-10
      )
      expect_equal(
        sc_row$se,
        sv_row$se,
        tolerance = 1e-8
      )
    }
  }
)

test_that(
  paste(
    "get_means() single-stage output unchanged after",
    "analysis helper refactor [regression]"
  ),
  {
    skip_if_not_installed("survey")
    df <- make_survey_data(n = 500, seed = 1)
    sc <- as_survey(
      df,
      ids = psu,
      weights = wt,
      strata = strata,
      fpc = fpc
    )
    test_invariants(sc)
    sv <- survey::svydesign(
      id = ~psu,
      weights = ~wt,
      strata = ~strata,
      fpc = ~fpc,
      data = df,
      nest = TRUE
    )
    sc_result <- get_means(sc, y1, variance = "se")
    sv_result <- survey::svymean(~y1, sv, na.rm = TRUE)
    expect_equal(
      sc_result$mean,
      coef(sv_result)[["y1"]],
      tolerance = 1e-10
    )
    expect_equal(
      sc_result$se,
      as.numeric(survey::SE(sv_result)),
      tolerance = 1e-8
    )
  }
)
