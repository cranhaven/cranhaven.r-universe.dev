# test-analysis-covariance.R
# Tests for get_covariance() — design-based finite-population covariance
# estimation across all five design classes plus collection dispatch.
#
# Numerical tolerance: 1e-10 for point estimates, 1e-8 for SE, 1e-6 for CI.
# Oracle: survey::svyvar() pair-at-a-time; secondary oracle get_variance()
# for the diagonal-parity quality gate.

# ---------------------------------------------------------------------------
# Category 1: Happy path — Taylor single-pair parity with svyvar() (off-diag)
# ---------------------------------------------------------------------------

test_that("get_covariance() Taylor point estimate matches svyvar() off-diag — NHANES [oracle]", {
  skip_if_not_installed("survey")

  d <- nhanes_2017[nhanes_2017$ridstatr == 2, ]
  sc <- as_survey(
    d,
    ids = sdmvpsu,
    strata = sdmvstra,
    weights = wtmec2yr,
    nest = TRUE
  )
  test_invariants(sc)
  sv <- survey::svydesign(
    ids = ~sdmvpsu,
    strata = ~sdmvstra,
    weights = ~wtmec2yr,
    data = d,
    nest = TRUE
  )

  sc_est <- suppressWarnings(
    get_covariance(sc, c(ridageyr, bpxsy1), variance = c("se", "ci"))
  )
  sv_var <- survey::svyvar(~ ridageyr + bpxsy1, sv, na.rm = TRUE)
  off_diag <- as.matrix(sv_var)[1L, 2L]

  expect_equal(sc_est$covariance[[1L]], off_diag, tolerance = 1e-10)
})

test_that("get_covariance() Taylor SE matches SE(svyvar()) off-diag — NHANES [oracle]", {
  skip_if_not_installed("survey")

  d <- nhanes_2017[nhanes_2017$ridstatr == 2, ]
  sc <- as_survey(
    d,
    ids = sdmvpsu,
    strata = sdmvstra,
    weights = wtmec2yr,
    nest = TRUE
  )
  test_invariants(sc)
  sv <- survey::svydesign(
    ids = ~sdmvpsu,
    strata = ~sdmvstra,
    weights = ~wtmec2yr,
    data = d,
    nest = TRUE
  )

  sc_est <- suppressWarnings(
    get_covariance(sc, c(ridageyr, bpxsy1), variance = c("se", "ci"))
  )
  sv_var <- survey::svyvar(~ ridageyr + bpxsy1, sv, na.rm = TRUE)
  vse <- as.numeric(survey::SE(sv_var))
  # SE() on a 2x2 svyvar object returns vec'd 2x2; off-diagonal is index 2 or 3
  off_se <- vse[[2L]]

  expect_equal(sc_est$se[[1L]], off_se, tolerance = 1e-8)
})

test_that("get_covariance() Wald CI = covariance ± qnorm(0.975) * SE", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 1L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_covariance(sc, c(y1, y2), variance = c("se", "ci"))
  z <- stats::qnorm(0.975)
  expect_equal(
    res$ci_low[[1L]],
    res$covariance[[1L]] - z * res$se[[1L]],
    tolerance = 1e-6
  )
  expect_equal(
    res$ci_high[[1L]],
    res$covariance[[1L]] + z * res$se[[1L]],
    tolerance = 1e-6
  )
})

# ---------------------------------------------------------------------------
# Category 2: Diagonal-parity quality gate — Taylor / replicate / nonprob
# ---------------------------------------------------------------------------

test_that("get_covariance() diagonal-parity gate — Taylor", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 2L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  # `c(y1, y1)` resolves to a single column under tidy-select, which would
  # trip the n>=2 guard. Use a duplicate column so we can exercise the
  # self-pair via two distinct names.
  df$y1b <- df$y1
  sc2 <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  cov_diag <- suppressWarnings(get_covariance(
    sc2,
    c(y1, y1b),
    diagonal = TRUE,
    variance = "se"
  ))
  var_y1 <- get_variance(sc2, y1, variance = "se")

  # Diagonal rows for y1 vs y1: the self-pair (y1, y1)
  diag_y1 <- cov_diag[
    cov_diag$var1 == "y1" & cov_diag$var2 == "y1",
    ,
    drop = FALSE
  ]
  expect_equal(diag_y1$covariance[[1L]], var_y1$variance[[1L]], tolerance = 1e-10)
  expect_equal(diag_y1$se[[1L]], var_y1$se[[1L]], tolerance = 1e-8)
})

test_that("get_covariance() diagonal-parity gate — replicate (BRR)", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = 3L
  )
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  sc <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(rep_cols),
    type = "BRR"
  )
  test_invariants(sc)
  df$y1b <- df$y1
  sc2 <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(rep_cols),
    type = "BRR"
  )

  cov_diag <- suppressWarnings(get_covariance(
    sc2,
    c(y1, y1b),
    diagonal = TRUE,
    variance = "se"
  ))
  var_y1 <- get_variance(sc2, y1, variance = "se")

  diag_y1 <- cov_diag[
    cov_diag$var1 == "y1" & cov_diag$var2 == "y1",
    ,
    drop = FALSE
  ]
  expect_equal(diag_y1$covariance[[1L]], var_y1$variance[[1L]], tolerance = 1e-10)
  expect_equal(diag_y1$se[[1L]], var_y1$se[[1L]], tolerance = 1e-8)
})

test_that("get_covariance() diagonal-parity gate — twophase", {
  skip_if_not_installed("survival")
  data("pbc", package = "survival", envir = environment())
  pbc_ph1 <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))
  pbc_ph1$chol2 <- pbc_ph1$chol

  ph1_sc <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")
  test_invariants(d)

  cov_diag <- suppressWarnings(get_covariance(
    d,
    c(chol, chol2),
    diagonal = TRUE,
    variance = "se"
  ))
  var_chol <- get_variance(d, chol, variance = "se")

  diag_chol <- cov_diag[
    cov_diag$var1 == "chol" & cov_diag$var2 == "chol",
    ,
    drop = FALSE
  ]
  expect_equal(
    diag_chol$covariance[[1L]],
    var_chol$variance[[1L]],
    tolerance = 1e-10
  )
  expect_equal(
    diag_chol$se[[1L]],
    var_chol$se[[1L]],
    tolerance = 1e-8
  )
})

test_that("get_covariance() diagonal-parity gate — nonprob", {
  set.seed(11L)
  df <- data.frame(
    y = rnorm(300L, mean = 5, sd = 2),
    yb = NA_real_, # placeholder
    w = runif(300L, 0.5, 2)
  )
  df$yb <- df$y
  d <- as_survey_nonprob(df, weights = w)
  test_invariants(d)

  cov_diag <- suppressWarnings(get_covariance(
    d,
    c(y, yb),
    diagonal = TRUE,
    variance = "se"
  ))
  var_y <- get_variance(d, y, variance = "se")

  diag_y <- cov_diag[
    cov_diag$var1 == "y" & cov_diag$var2 == "y",
    ,
    drop = FALSE
  ]
  expect_equal(diag_y$covariance[[1L]], var_y$variance[[1L]], tolerance = 1e-10)
  expect_equal(diag_y$se[[1L]], var_y$se[[1L]], tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Category 3: Symmetry — (x, y) == (y, x)
# ---------------------------------------------------------------------------

test_that("get_covariance() symmetry under redundant=TRUE — covariance and SE", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 4L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_covariance(
    sc,
    c(y1, y2),
    redundant = TRUE,
    variance = "se"
  )
  expect_identical(nrow(res), 2L)

  # (y1, y2) and (y2, y1) must be numerically identical
  ab <- res[res$var1 == "y1" & res$var2 == "y2", , drop = FALSE]
  ba <- res[res$var1 == "y2" & res$var2 == "y1", , drop = FALSE]
  expect_equal(ab$covariance, ba$covariance, tolerance = 1e-10)
  expect_equal(ab$se, ba$se, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Category 4: Pair-generation rule — row counts under all 4 flag combos
# ---------------------------------------------------------------------------

test_that("get_covariance() default flags emit |vars|*(|vars|-1)/2 rows", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 5L)
  df$y4 <- df$y1 + df$y2
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- suppressWarnings(get_covariance(sc, c(y1, y2, y3, y4)))
  expect_identical(nrow(res), 6L) # 4*3/2
})

test_that("get_covariance() redundant=TRUE emits |vars|*(|vars|-1) rows", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 6L)
  df$y4 <- df$y1 + df$y2
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- suppressWarnings(
    get_covariance(sc, c(y1, y2, y3, y4), redundant = TRUE)
  )
  expect_identical(nrow(res), 12L) # 4*3
})

test_that("get_covariance() diagonal=TRUE emits |vars|*(|vars|+1)/2 rows", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 7L)
  df$y4 <- df$y1 + df$y2
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- suppressWarnings(
    get_covariance(sc, c(y1, y2, y3, y4), diagonal = TRUE)
  )
  expect_identical(nrow(res), 10L) # 4*5/2
})

test_that("get_covariance() redundant + diagonal emit |vars|^2 rows", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 8L)
  df$y4 <- df$y1 + df$y2
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- suppressWarnings(get_covariance(
    sc,
    c(y1, y2, y3, y4),
    redundant = TRUE,
    diagonal = TRUE
  ))
  expect_identical(nrow(res), 16L) # 4*4
})

# ---------------------------------------------------------------------------
# Category 5: Replicate, twophase, nonprob — point/SE numerical parity
# ---------------------------------------------------------------------------

test_that("get_covariance() nonprob matches svyvar() off-diag [oracle]", {
  skip_if_not_installed("survey")

  set.seed(20L)
  df <- data.frame(
    a = rnorm(500L),
    b = rnorm(500L),
    w = runif(500L, 0.5, 2)
  )
  d <- as_survey_nonprob(df, weights = w)
  test_invariants(d)

  d_sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sv <- survey::svyvar(~ a + b, d_sv, na.rm = TRUE)
  off_diag <- as.matrix(sv)[1L, 2L]
  vse <- as.numeric(survey::SE(sv))

  sc <- get_covariance(d, c(a, b), variance = "se")

  expect_equal(sc$covariance[[1L]], off_diag, tolerance = 1e-10)
  expect_equal(sc$se[[1L]], vse[[2L]], tolerance = 1e-8)
})

test_that("get_covariance() twophase matches svyvar() off-diag [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1 <- subset(pbc, !is.na(trt))
  # Phase-2 sample = subjects with non-missing chol AND albumin. The subset
  # column must contain both TRUE and FALSE for as_survey_twophase().
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol) & !is.na(pbc_ph1$albumin)
  pbc_ph1$wt <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))

  ph1_sc <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d_sc <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")
  test_invariants(d_sc)

  d_sv <- survey::twophase(
    id = list(~1, ~1),
    data = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sv <- survey::svyvar(~ chol + albumin, d_sv, na.rm = TRUE)
  off_diag <- as.matrix(sv)[1L, 2L]
  vse <- as.numeric(survey::SE(sv))

  sc <- get_covariance(d_sc, c(chol, albumin), variance = "se")

  expect_equal(sc$covariance[[1L]], off_diag, tolerance = 1e-10)
  expect_equal(sc$se[[1L]], vse[[2L]], tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Category 6: Constant variable — covariance = 0, se = 0, deff = 0
# ---------------------------------------------------------------------------

test_that("get_covariance() returns exact 0 when one var is constant", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 30L)
  df$const <- 7
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(get_covariance(
    sc,
    c(y1, const),
    variance = c("se", "ci", "moe", "deff")
  ))
  expect_identical(res$covariance[[1L]], 0)
  expect_identical(res$se[[1L]], 0)
  expect_identical(res$ci_low[[1L]], 0)
  expect_identical(res$ci_high[[1L]], 0)
  expect_identical(res$moe[[1L]], 0)
  # 0/0 deff guard: report exactly 0 (not NA)
  expect_identical(res$deff[[1L]], 0)
})

test_that("get_covariance() returns 0 when both vars are constant", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 31L)
  df$c1 <- 1
  df$c2 <- 5
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(get_covariance(sc, c(c1, c2), variance = "se"))
  expect_identical(res$covariance[[1L]], 0)
  expect_identical(res$se[[1L]], 0)
})

# ---------------------------------------------------------------------------
# Category 7: name_style = "broom"
# ---------------------------------------------------------------------------

test_that("get_covariance() name_style='broom' renames covariance/se/ci_low/ci_high", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 40L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_covariance(
    sc,
    c(y1, y2),
    variance = c("se", "ci"),
    name_style = "broom"
  )
  expect_true("estimate" %in% names(res))
  expect_true("std.error" %in% names(res))
  expect_true("conf.low" %in% names(res))
  expect_true("conf.high" %in% names(res))
  expect_false("covariance" %in% names(res))
  expect_false("se" %in% names(res))
  expect_false("ci_low" %in% names(res))
  expect_false("ci_high" %in% names(res))
})

# ---------------------------------------------------------------------------
# Category 8: n_weighted column
# ---------------------------------------------------------------------------

test_that("get_covariance() n_weighted = TRUE appends n_weighted column", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 50L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_covariance(sc, c(y1, y2), variance = NULL, n_weighted = TRUE)
  expect_true("n_weighted" %in% names(res))
  mask <- !is.na(df$y1) & !is.na(df$y2) & df$wt > 0
  expect_equal(res$n_weighted[[1L]], sum(df$wt[mask]), tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Category 9: Column label attributes
# ---------------------------------------------------------------------------

test_that("get_covariance() attaches label attribute to every output column", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    design = "taylor",
    seed = 60L,
    with_labels = TRUE
  )
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_covariance(
    sc,
    c(y1, y2),
    variance = c("se", "ci", "moe", "deff"),
    n_weighted = TRUE
  )
  for (nm in names(res)) {
    expect_true(
      !is.null(attr(res[[nm]], "label")),
      label = paste0("label present on column '", nm, "'")
    )
  }
  expect_identical(attr(res$var1, "label"), "Variable 1")
  expect_identical(attr(res$var2, "label"), "Variable 2")
  expect_identical(attr(res$covariance, "label"), "Covariance")
  expect_identical(attr(res$se, "label"), "SE")
  expect_identical(attr(res$ci_low, "label"), "95% CI low")
  expect_identical(attr(res$ci_high, "label"), "95% CI high")
  expect_identical(attr(res$n, "label"), "N")
})

test_that("get_covariance() conf_level interpolates into CI labels", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 61L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- get_covariance(sc, c(y1, y2), variance = "ci", conf_level = 0.90)
  expect_identical(attr(res$ci_low, "label"), "90% CI low")
  expect_identical(attr(res$ci_high, "label"), "90% CI high")
})

# ---------------------------------------------------------------------------
# Category 10: .meta structure
# ---------------------------------------------------------------------------

test_that("get_covariance() .meta has expected top-level keys", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 70L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_covariance(sc, c(y1, y2))
  m <- meta(res)

  expect_true("group" %in% names(m))
  expect_true("x" %in% names(m))
  expect_true("design_class" %in% names(m))
  expect_true("method" %in% names(m))
  expect_true("conf_level" %in% names(m))
  expect_true("name_style" %in% names(m))
  expect_true("min_cell_n" %in% names(m))
  expect_true("redundant" %in% names(m))
  expect_true("diagonal" %in% names(m))
  expect_true("na_rm" %in% names(m))

  expect_false("function_name" %in% names(m))
  expect_false("variable" %in% names(m))

  expect_identical(m$method, "covariance")
  expect_identical(m$design_class, "surveycore::survey_taylor")
  expect_equal(m$conf_level, 0.95)
  expect_identical(m$redundant, FALSE)
  expect_identical(m$diagonal, FALSE)
  expect_identical(m$na_rm, TRUE)
})

test_that("get_covariance() .meta$x has one entry per resolved numeric variable in supply order", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 71L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- suppressWarnings(get_covariance(sc, c(y1, y2, y3)))
  m <- meta(res)
  expect_identical(names(m$x), c("y1", "y2", "y3"))
  for (nm in names(m$x)) {
    expect_true(
      all(c("variable_label", "question_preface", "value_labels") %in% names(m$x[[nm]]))
    )
  }
})

test_that("get_covariance() .meta$group empty when no grouping is active", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 72L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- get_covariance(sc, c(y1, y2))
  m <- meta(res)
  expect_type(m$group, "list")
  expect_length(m$group, 0L)
})

test_that("get_covariance() .meta$group populated when grouping is active", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 73L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- suppressWarnings(get_covariance(sc, c(y1, y2), group = group))
  m <- meta(res)
  expect_identical(names(m$group), "group")
})

# ---------------------------------------------------------------------------
# Category 11: var1/var2 factor levels
# ---------------------------------------------------------------------------

test_that("get_covariance() var1/var2 are factors with levels in supply order", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 80L)
  df$y4 <- df$y1 + df$y2
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- suppressWarnings(get_covariance(sc, c(y2, y4, y1)))
  expect_s3_class(res$var1, "factor")
  expect_s3_class(res$var2, "factor")
  expect_identical(levels(res$var1), c("y2", "y4", "y1"))
  expect_identical(levels(res$var2), c("y2", "y4", "y1"))
})

test_that("get_covariance() label_vars=TRUE substitutes variable labels in factor levels", {
  df <- make_survey_data(
    n = 100L,
    design = "taylor",
    seed = 81L,
    with_labels = TRUE
  )
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc <- set_var_label(sc, y1 = "Outcome 1", y2 = "Outcome 2")

  res_labelled <- get_covariance(sc, c(y1, y2), label_vars = TRUE)
  res_raw <- get_covariance(sc, c(y1, y2), label_vars = FALSE)

  expect_identical(as.character(res_labelled$var1[[1L]]), "Outcome 1")
  expect_identical(as.character(res_labelled$var2[[1L]]), "Outcome 2")
  expect_identical(as.character(res_raw$var1[[1L]]), "y1")
  expect_identical(as.character(res_raw$var2[[1L]]), "y2")
})

# ---------------------------------------------------------------------------
# Category 12: decimals
# ---------------------------------------------------------------------------

test_that("get_covariance() decimals = 3 rounds every numeric output column", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 90L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  res_full <- get_covariance(
    sc,
    c(y1, y2),
    variance = c("se", "ci", "moe", "deff"),
    n_weighted = TRUE
  )
  res_r <- get_covariance(
    sc,
    c(y1, y2),
    variance = c("se", "ci", "moe", "deff"),
    n_weighted = TRUE,
    decimals = 3
  )

  for (nm in c("covariance", "se", "ci_low", "ci_high", "moe", "deff", "n_weighted")) {
    if (is.finite(res_full[[nm]][[1L]])) {
      expect_identical(
        res_r[[nm]][[1L]],
        round(res_full[[nm]][[1L]], 3),
        info = paste0("rounding for '", nm, "'")
      )
    }
  }
})

# ---------------------------------------------------------------------------
# Category 13: deff parity (Goodnight/Mood-Graybill)
# ---------------------------------------------------------------------------

test_that("get_covariance() deff = se^2 / ((Var(x)*Var(y) + cov^2) / (n-1))", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 100L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  res <- get_covariance(sc, c(y1, y2), variance = c("se", "deff"))
  cov_hat <- res$covariance[[1L]]
  se_hat <- res$se[[1L]]
  n_d <- res$n[[1L]]

  # Use get_variance() to get Kish-corrected per-variable variances on the
  # pair's complete-case domain. Since y1 and y2 here have no NAs, the
  # active domain is the same.
  v_x <- get_variance(sc, y1)$variance[[1L]]
  v_y <- get_variance(sc, y2)$variance[[1L]]

  se_srs <- sqrt((v_x * v_y + cov_hat^2) / (n_d - 1L))
  expected_deff <- (se_hat / se_srs)^2
  expect_equal(res$deff[[1L]], expected_deff, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Category 14: Print smoke test
# ---------------------------------------------------------------------------

test_that("get_covariance() print method returns invisibly and does not error", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 110L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- get_covariance(sc, c(y1, y2))
  expect_output(out <- print(res))
  expect_identical(out, res)
})

# ---------------------------------------------------------------------------
# Category 15: broom tidy/glance smoke tests
# ---------------------------------------------------------------------------

test_that("broom::tidy(survey_covariance) returns a tibble", {
  skip_if_not_installed("broom")
  df <- make_survey_data(n = 200L, design = "taylor", seed = 120L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- get_covariance(sc, c(y1, y2))
  td <- broom::tidy(res)
  expect_true(tibble::is_tibble(td))
})

test_that("broom::glance(survey_covariance) returns a 1-row tibble", {
  skip_if_not_installed("broom")
  df <- make_survey_data(n = 200L, design = "taylor", seed = 121L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- get_covariance(sc, c(y1, y2))
  gl <- broom::glance(res)
  expect_true(tibble::is_tibble(gl))
  expect_identical(nrow(gl), 1L)
})

# ---------------------------------------------------------------------------
# Category 16: Result class invariants
# ---------------------------------------------------------------------------

test_that("get_covariance() result inherits survey_covariance/survey_result/tbl_df", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 130L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- get_covariance(sc, c(y1, y2))
  expect_s3_class(res, "survey_covariance")
  expect_s3_class(res, "survey_result")
  expect_true(tibble::is_tibble(res))
  expect_true("covariance" %in% names(res))
  expect_true("n" %in% names(res))
})

# ---------------------------------------------------------------------------
# Category 17: Error paths — dual-pattern (class + snapshot)
# ---------------------------------------------------------------------------

test_that("get_covariance() rejects non-survey design", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  expect_error(
    get_covariance(df, c(x, y)),
    class = "surveycore_error_unsupported_class"
  )
  expect_snapshot(error = TRUE, get_covariance(df, c(x, y)))
})

test_that("get_covariance() rejects empty selection (0 cols)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 140L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    get_covariance(sc, dplyr::any_of("nonexistent_var")),
    class = "surveycore_error_insufficient_variables"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(sc, dplyr::any_of("nonexistent_var"))
  )
})

test_that("get_covariance() rejects single-variable selection (1 col)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 141L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    get_covariance(sc, c(y1)),
    class = "surveycore_error_insufficient_variables"
  )
  expect_snapshot(error = TRUE, get_covariance(sc, c(y1)))
})

test_that("get_covariance() warns then errors when only 1 numeric remains after drop", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 142L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    expect_error(
      get_covariance(sc, c(y1, group)),
      class = "surveycore_error_insufficient_variables"
    ),
    class = "surveycore_warning_covariance_non_numeric"
  )
})

test_that("get_covariance() rejects invalid variance value", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 143L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    get_covariance(sc, c(y1, y2), variance = "foo"),
    class = "surveycore_error_invalid_variance_arg"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(sc, c(y1, y2), variance = "foo")
  )
})

test_that("get_covariance() rejects invalid conf_level", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 144L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    get_covariance(sc, c(y1, y2), conf_level = 0),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_error(
    get_covariance(sc, c(y1, y2), conf_level = 1),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_error(
    get_covariance(sc, c(y1, y2), conf_level = NA),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(sc, c(y1, y2), conf_level = 0)
  )
})

test_that("get_covariance() rejects invalid decimals", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 145L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    get_covariance(sc, c(y1, y2), decimals = -1),
    class = "surveycore_error_invalid_decimals"
  )
  expect_error(
    get_covariance(sc, c(y1, y2), decimals = 1.5),
    class = "surveycore_error_invalid_decimals"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(sc, c(y1, y2), decimals = -1)
  )
})

test_that("get_covariance() rejects invalid name_style", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 146L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    get_covariance(sc, c(y1, y2), name_style = "foo"),
    class = "surveycore_error_invalid_name_style"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(sc, c(y1, y2), name_style = "foo")
  )
})

test_that("get_covariance() rejects non-logical na.rm", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 147L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_error(
    get_covariance(sc, c(y1, y2), na.rm = NA),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_error(
    get_covariance(sc, c(y1, y2), na.rm = 1),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(sc, c(y1, y2), na.rm = NA)
  )
})

# ---------------------------------------------------------------------------
# Category 18: Warning paths
# ---------------------------------------------------------------------------

test_that("get_covariance() fires small_cell when any pair has n < min_cell_n", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 200L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    get_covariance(sc, c(y1, y2), min_cell_n = nrow(df) + 1L),
    class = "surveycore_warning_small_cell"
  )
  expect_snapshot({
    suppressWarnings(
      withCallingHandlers(
        get_covariance(sc, c(y1, y2), min_cell_n = nrow(df) + 1L),
        warning = function(w) {
          if (inherits(w, "surveycore_warning_small_cell")) {
            message(conditionMessage(w))
          }
          invokeRestart("muffleWarning")
        }
      )
    )
  })
})

test_that("get_covariance() fires single_level when grouping var has one observed level", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 201L)
  df$const_grp <- 1L
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    get_covariance(sc, c(y1, y2), group = const_grp),
    class = "surveycore_warning_single_level"
  )
})

test_that("get_covariance() fires covariance_non_numeric when dropping non-numeric vars", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 202L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    res <- get_covariance(sc, c(y1, y2, group)),
    class = "surveycore_warning_covariance_non_numeric"
  )
  expect_identical(nrow(res), 1L)
  expect_snapshot({
    suppressWarnings(
      withCallingHandlers(
        get_covariance(sc, c(y1, y2, group)),
        warning = function(w) {
          if (inherits(w, "surveycore_warning_covariance_non_numeric")) {
            message(conditionMessage(w))
          }
          invokeRestart("muffleWarning")
        }
      )
    )
  })
})

test_that("get_covariance() fires cv_undefined when variance='cv' on constant pair", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 203L)
  df$const <- 5
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    get_covariance(sc, c(y1, const), variance = "cv"),
    class = "surveycore_warning_cv_undefined"
  )
})

test_that("get_covariance() fires covariance_all_na when pair is all-NA in domain", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 204L)
  df$allna <- NA_real_
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    res <- get_covariance(sc, c(y1, allna)),
    class = "surveycore_warning_covariance_all_na"
  )
  expect_true(is.nan(res$covariance[[1L]]))
  expect_identical(res$n[[1L]], 0L)
  expect_snapshot({
    suppressWarnings(
      withCallingHandlers(
        get_covariance(sc, c(y1, allna)),
        warning = function(w) {
          if (inherits(w, "surveycore_warning_covariance_all_na")) {
            message(conditionMessage(w))
          }
          invokeRestart("muffleWarning")
        }
      )
    )
  })
})

test_that("get_covariance() fires covariance_all_na when pair NA patterns are disjoint", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 205L)
  df$a <- df$y1
  df$b <- df$y2
  df$a[1:50] <- NA
  df$b[51:100] <- NA
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    res <- get_covariance(sc, c(a, b)),
    class = "surveycore_warning_covariance_all_na"
  )
  expect_true(is.nan(res$covariance[[1L]]))
  expect_identical(res$n[[1L]], 0L)
})

test_that("get_covariance() fires covariance_insufficient_n when n == 1", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 206L)
  df$one <- NA_real_
  df$one[[1L]] <- 42
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    res <- get_covariance(sc, c(y1, one)),
    class = "surveycore_warning_covariance_insufficient_n"
  )
  expect_true(is.nan(res$covariance[[1L]]))
  expect_identical(res$n[[1L]], 1L)
  expect_snapshot({
    suppressWarnings(
      withCallingHandlers(
        get_covariance(sc, c(y1, one)),
        warning = function(w) {
          if (inherits(w, "surveycore_warning_covariance_insufficient_n")) {
            message(conditionMessage(w))
          }
          invokeRestart("muffleWarning")
        }
      )
    )
  })
})

test_that("get_covariance() diagonal=TRUE with all-NA variable fires covariance_all_na", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 207L)
  df$allna <- NA_real_
  df$y1b <- df$y1
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    res <- get_covariance(sc, c(allna, y1b), diagonal = TRUE),
    class = "surveycore_warning_covariance_all_na"
  )
  diag_allna <- res[res$var1 == "allna" & res$var2 == "allna", , drop = FALSE]
  expect_true(is.nan(diag_allna$covariance[[1L]]))
  expect_identical(diag_allna$n[[1L]], 0L)
})

# ---------------------------------------------------------------------------
# Category 19: na.rm = FALSE behaviour
# ---------------------------------------------------------------------------

test_that("get_covariance() na.rm=FALSE with NAs returns NaN estimate, n = active rows", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 220L)
  df$y_na <- df$y2
  df$y_na[1:5] <- NA
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  res <- suppressWarnings(get_covariance(sc, c(y1, y_na), na.rm = FALSE))
  expect_identical(res$n[[1L]], 100L)
  expect_true(is.nan(res$covariance[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 20: Replicate near-zero variance edge case
# ---------------------------------------------------------------------------

test_that("get_covariance() replicate near-constant pair returns se = 0 (no NaN)", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    design = "replicate",
    type = "brr",
    seed = 230L
  )
  df$const1 <- 42
  df$const2 <- 7
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  sc <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(rep_cols),
    type = "BRR"
  )
  test_invariants(sc)
  res <- suppressWarnings(
    get_covariance(sc, c(const1, const2), variance = c("se", "ci"))
  )
  expect_identical(res$se[[1L]], 0)
  expect_false(is.nan(res$se[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 21: Nonprob zero-weight rows excluded from n
# ---------------------------------------------------------------------------

test_that("get_covariance() nonprob excludes zero-weight rows from n", {
  set.seed(240L)
  n <- 200L
  df_pos <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    w = runif(n, 0.5, 2)
  )
  d <- as_survey_nonprob(df_pos, weights = w)

  zero_idx <- c(5L, 17L, 42L, 101L, 150L)
  df_inj <- df_pos
  df_inj$w[zero_idx] <- 0
  d <- S7::set_props(d, data = df_inj)

  sc <- get_covariance(d, c(a, b), variance = "se")
  expect_identical(sc$n[[1L]], n - length(zero_idx))
})

# ---------------------------------------------------------------------------
# Category 22: Grouped estimate parity with svyby(svyvar)
# ---------------------------------------------------------------------------

test_that("get_covariance() group= matches svyby(svyvar) off-diag [oracle]", {
  skip_if_not_installed("survey")

  df <- make_survey_data(n = 400L, n_psu = 40L, design = "taylor", seed = 300L)
  df$g <- factor(sample(c("A", "B"), nrow(df), replace = TRUE))
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)
  sv <- survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~wt,
    data = df,
    nest = TRUE
  )

  sc_est <- suppressWarnings(
    get_covariance(sc, c(y1, y2), group = g, variance = "se")
  )
  # svyby returns a 2x2 result per group in vec'd form: V1=Var(y1),
  # V2=Cov(y1,y2), V3=Cov(y2,y1), V4=Var(y2)
  sv_est <- survey::svyby(
    ~ y1 + y2,
    ~g,
    sv,
    survey::svyvar,
    na.rm = TRUE
  )
  sc_ord <- sc_est[order(sc_est$g), , drop = FALSE]
  sv_ord <- sv_est[order(sv_est$g), , drop = FALSE]
  expect_equal(
    as.numeric(sc_ord$covariance),
    as.numeric(sv_ord$V2),
    tolerance = 1e-10
  )
})

# ---------------------------------------------------------------------------
# Category 23: Collection dispatch — happy path / .id / .meta carry-over
# ---------------------------------------------------------------------------

.make_covariance_collection <- function(n_surveys = 3L, seed = 42L) {
  surveys <- lapply(seq_len(n_surveys), function(i) {
    df <- make_survey_data(
      n = 120L,
      n_psu = 12L,
      n_strata = 3L,
      design = "taylor",
      seed = seed + i
    )
    as_survey(df, ids = psu, weights = wt, strata = strata)
  })
  names(surveys) <- paste0("w", seq_len(n_surveys))
  do.call(as_survey_collection, surveys)
}

test_that("get_covariance() dispatches over a 3-survey collection", {
  coll <- .make_covariance_collection()
  result <- get_covariance(coll, c(y1, y2))
  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1L], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_covariance() dispatch matches per-survey bind oracle", {
  coll <- .make_covariance_collection()
  got <- get_covariance(coll, c(y1, y2))
  per <- lapply(names(coll), function(nm) get_covariance(coll[[nm]], c(y1, y2)))
  names(per) <- names(coll)
  want <- dplyr::bind_rows(per, .id = ".survey")

  numeric_cols <- intersect(
    names(got)[vapply(got, is.numeric, logical(1L))],
    names(want)
  )
  for (col in numeric_cols) {
    expect_equal(got[[col]], want[[col]], tolerance = 1e-12)
  }
})

test_that("get_covariance() collection custom .id renames identifier column", {
  coll <- .make_covariance_collection()
  result <- get_covariance(coll, c(y1, y2), .id = "wave")
  expect_identical(names(result)[1L], "wave")
  expect_false(".survey" %in% names(result))
})

test_that("get_covariance() collection .meta$collection$surveys records contributing names", {
  coll <- .make_covariance_collection()
  result <- get_covariance(coll, c(y1, y2))
  m <- attr(result, ".meta")
  expect_identical(m$collection$surveys, names(coll))
})

# ---------------------------------------------------------------------------
# Category 24: Collection error paths (C5/C6/C7/C13)
# ---------------------------------------------------------------------------

test_that("get_covariance() collection: .if_missing_var='error' aborts when var missing", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$focal <- rnorm(nrow(df))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)
  expect_error(
    get_covariance(coll, c(focal, y1), .if_missing_var = "error"),
    class = "surveycore_error_collection_missing_var"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(coll, c(focal, y1), .if_missing_var = "error")
  )
})

test_that("get_covariance() collection: .if_missing_var='skip' with all missing aborts", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)
  expect_error(
    get_covariance(coll, c(focal, y1), .if_missing_var = "skip"),
    class = "surveycore_error_collection_all_skipped"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(coll, c(focal, y1), .if_missing_var = "skip")
  )
})

test_that("get_covariance() collection: .id collision aborts", {
  coll <- .make_covariance_collection()
  expect_error(
    get_covariance(coll, c(y1, y2), .id = "covariance"),
    class = "surveycore_error_collection_id_collision"
  )
  expect_snapshot(
    error = TRUE,
    get_covariance(coll, c(y1, y2), .id = "covariance")
  )
})

test_that("get_covariance() collection: .id rejects empty / NA / wrong length", {
  coll <- .make_covariance_collection()
  expect_error(
    get_covariance(coll, c(y1, y2), .id = ""),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_error(
    get_covariance(coll, c(y1, y2), .id = c("a", "b")),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("get_covariance() per-survey path raises variable_not_found (C10)", {
  df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_error(
    get_covariance(d, c(nonexistent_variable, y1)),
    class = "surveycore_error_variable_not_found"
  )
})

# ---------------------------------------------------------------------------
# Category 25: Collection skip / divergence / length-1
# ---------------------------------------------------------------------------

test_that("get_covariance() collection: .if_missing_var='skip' emits message", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$focal <- rnorm(nrow(df))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)
  expect_message(
    get_covariance(coll, c(focal, y1), .if_missing_var = "skip"),
    class = "surveycore_message_collection_skipped_surveys"
  )
})

test_that("get_covariance() collection meta divergence emits warning", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      attr(df$y1, "label") <- "Outcome 1"
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)
  expect_warning(
    get_covariance(coll, c(y1, y2)),
    class = "surveycore_warning_collection_meta_divergence"
  )
})

test_that("get_covariance() collection length-1 dispatches", {
  coll <- .make_covariance_collection(n_surveys = 1L)
  result <- get_covariance(coll, c(y1, y2))
  direct <- get_covariance(coll[["w1"]], c(y1, y2))
  expect_identical(nrow(result), nrow(direct))
  expect_identical(unique(result$.survey), "w1")
})

# ---------------------------------------------------------------------------
# Category 26: Collection diagonal-parity gate
# ---------------------------------------------------------------------------

test_that("get_covariance() collection diagonal-parity matches per-survey get_variance()", {
  coll <- .make_covariance_collection(n_surveys = 2L)
  # Inject a duplicate variable so we can exercise the self-pair via two names
  for (nm in names(coll)) {
    df <- coll[[nm]]@data
    df$y1b <- df$y1
    coll@surveys[[nm]] <- as_survey(
      df,
      ids = psu,
      weights = wt,
      strata = strata
    )
  }

  # Walk per-survey: the diagonal-parity invariant is per-survey, and explicit
  # named control args (diagonal, variance) are not forwarded through
  # collection dispatch — call each survey directly via `coll[[nm]]`.
  for (nm in names(coll)) {
    sc_one <- coll[[nm]]
    res_one <- suppressWarnings(get_covariance(
      sc_one,
      c(y1, y1b),
      diagonal = TRUE,
      variance = "se"
    ))
    var_y1 <- get_variance(sc_one, y1, variance = "se")
    diag_row <- res_one[
      res_one$var1 == "y1" & res_one$var2 == "y1",
      ,
      drop = FALSE
    ]
    expect_equal(
      diag_row$covariance[[1L]],
      var_y1$variance[[1L]],
      tolerance = 1e-10
    )
    expect_equal(
      diag_row$se[[1L]],
      var_y1$se[[1L]],
      tolerance = 1e-8
    )
  }
})
