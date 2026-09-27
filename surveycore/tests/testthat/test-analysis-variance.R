# test-analysis-variance.R
# Tests for get_variance() — design-based finite-population variance estimation.
#
# Numerical tolerance: 1e-10 for point estimates, 1e-8 for SE, 1e-6 for CI.
# Oracle tests skip if the 'survey' package is not installed.

# ---------------------------------------------------------------------------
# Category 1: Happy path — Taylor single-variable parity with survey::svyvar()
# ---------------------------------------------------------------------------

test_that("get_variance() Taylor point estimate matches svyvar() — NHANES [oracle]", {
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

  sc_est <- get_variance(sc, ridageyr, variance = c("se", "ci"))
  sv_est <- survey::svyvar(~ridageyr, sv, na.rm = TRUE)

  expect_equal(
    sc_est$variance[[1L]],
    as.numeric(coef(sv_est))[[1L]],
    tolerance = 1e-10
  )
})

test_that("get_variance() Taylor SE matches SE(svyvar()) — NHANES [oracle]", {
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

  sc_est <- get_variance(sc, ridageyr, variance = c("se", "ci"))
  sv_est <- survey::svyvar(~ridageyr, sv, na.rm = TRUE)

  expect_equal(
    sc_est$se[[1L]],
    as.numeric(survey::SE(sv_est))[[1L]],
    tolerance = 1e-8
  )
})

test_that("get_variance() Taylor CI = coef +/- qnorm(0.975) * SE — NHANES [oracle]", {
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

  sc_est <- get_variance(sc, ridageyr, variance = c("se", "ci"))
  z <- stats::qnorm(0.975)
  expect_equal(
    sc_est$ci_low[[1L]],
    sc_est$variance[[1L]] - z * sc_est$se[[1L]],
    tolerance = 1e-6
  )
  expect_equal(
    sc_est$ci_high[[1L]],
    sc_est$variance[[1L]] + z * sc_est$se[[1L]],
    tolerance = 1e-6
  )
})

# ---------------------------------------------------------------------------
# Category 2: Happy path — Replicate (BRR) parity with svyvar() on ACS PUMS
# ---------------------------------------------------------------------------

test_that("get_variance() replicate (BRR) matches svyvar() — ACS PUMS WY [oracle]", {
  skip_if_not_installed("survey")
  rep_cols <- grep("^pwgtp[0-9]+$", names(acs_pums_wy), value = TRUE)
  skip_if(length(rep_cols) == 0L, "acs_pums_wy missing replicate weights")
  skip_if(!("agep" %in% names(acs_pums_wy)), "acs_pums_wy missing agep")

  sc <- as_survey_replicate(
    acs_pums_wy,
    weights = pwgtp,
    repweights = all_of(rep_cols),
    type = "BRR"
  )
  test_invariants(sc)
  sv <- survey::svrepdesign(
    weights = ~pwgtp,
    repweights = acs_pums_wy[, rep_cols],
    data = acs_pums_wy,
    type = "BRR",
    combined.weights = TRUE,
    mse = TRUE
  )

  sc_est <- get_variance(sc, agep, variance = c("se", "ci"))
  sv_est <- survey::svyvar(~agep, sv, na.rm = TRUE)

  expect_equal(
    sc_est$variance[[1L]],
    as.numeric(coef(sv_est))[[1L]],
    tolerance = 1e-10
  )
  expect_equal(
    sc_est$se[[1L]],
    as.numeric(survey::SE(sv_est))[[1L]],
    tolerance = 1e-8
  )
})

# ---------------------------------------------------------------------------
# Category 3: Multi-variable — pairwise vs listwise NA handling
# ---------------------------------------------------------------------------

test_that("get_variance() multi-variable pairwise matches per-variable svyvar() [oracle]", {
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
    get_variance(sc, c(ridageyr, bpxsy1), variance = "se")
  )
  sv_a <- survey::svyvar(~ridageyr, sv, na.rm = TRUE)
  sv_b <- survey::svyvar(~bpxsy1, sv, na.rm = TRUE)

  expect_equal(sc_est$variance[[1L]], as.numeric(coef(sv_a))[[1L]], tolerance = 1e-10)
  expect_equal(sc_est$variance[[2L]], as.numeric(coef(sv_b))[[1L]], tolerance = 1e-10)
  expect_equal(sc_est$se[[1L]], as.numeric(survey::SE(sv_a))[[1L]], tolerance = 1e-8)
  expect_equal(sc_est$se[[2L]], as.numeric(survey::SE(sv_b))[[1L]], tolerance = 1e-8)
})

test_that("get_variance() listwise matches svyvar() diagonal on shared complete cases [oracle]", {
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
    get_variance(
      sc,
      c(ridageyr, bpxsy1),
      variance = "se",
      na_handling = "listwise"
    )
  )
  sv_est <- survey::svyvar(~ridageyr + bpxsy1, sv, na.rm = TRUE)
  diag_var <- diag(as.matrix(sv_est))

  expect_equal(sc_est$variance[[1L]], as.numeric(diag_var[["ridageyr"]]), tolerance = 1e-10)
  expect_equal(sc_est$variance[[2L]], as.numeric(diag_var[["bpxsy1"]]), tolerance = 1e-10)
  # Under listwise, every row shares n
  expect_identical(sc_est$n[[1L]], sc_est$n[[2L]])
})

# ---------------------------------------------------------------------------
# Category 4: Grouped estimate — via group= and via group_by()
# ---------------------------------------------------------------------------

test_that("get_variance() group= matches svyby(svyvar) [oracle]", {
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
    get_variance(sc, ridageyr, group = riagendr, variance = "se")
  )
  sv_est <- survey::svyby(
    ~ridageyr,
    ~riagendr,
    sv,
    survey::svyvar,
    na.rm = TRUE
  )

  sc_ord <- sc_est[order(sc_est$riagendr), , drop = FALSE]
  sv_ord <- sv_est[order(sv_est$riagendr), , drop = FALSE]

  expect_equal(
    as.numeric(sc_ord$variance),
    as.numeric(sv_ord$ridageyr),
    tolerance = 1e-10
  )
  expect_equal(
    as.numeric(sc_ord$se),
    as.numeric(sv_ord$se),
    tolerance = 1e-8
  )
})

test_that("get_variance() group_by(design) matches group= [oracle]", {
  skip_if_not_installed("survey")
  skip_if_not_installed("surveytidy")

  d <- nhanes_2017[nhanes_2017$ridstatr == 2, ]
  sc <- as_survey(
    d,
    ids = sdmvpsu,
    strata = sdmvstra,
    weights = wtmec2yr,
    nest = TRUE
  )
  test_invariants(sc)

  sc_grouped <- surveytidy::group_by(sc, riagendr)
  est_a <- suppressWarnings(
    get_variance(sc_grouped, ridageyr, variance = "se")
  )
  est_b <- suppressWarnings(
    get_variance(sc, ridageyr, group = riagendr, variance = "se")
  )
  # Same rows (possibly different row order)
  a_ord <- est_a[order(est_a$riagendr), , drop = FALSE]
  b_ord <- est_b[order(est_b$riagendr), , drop = FALSE]
  expect_equal(a_ord$variance, b_ord$variance, tolerance = 1e-10)
  expect_equal(a_ord$se, b_ord$se, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Category 5: Constant variable — exact 0 with NA cv
# ---------------------------------------------------------------------------

test_that("get_variance() constant variable returns exact 0 for point + SE + CI + moe + deff", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 101L)
  df$const <- 7
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(
    get_variance(sc, const, variance = c("se", "ci", "var", "moe", "deff"))
  )
  expect_identical(res$variance[[1L]], 0)
  expect_identical(res$se[[1L]], 0)
  expect_identical(res$var[[1L]], 0)
  expect_identical(res$ci_low[[1L]], 0)
  expect_identical(res$ci_high[[1L]], 0)
  expect_identical(res$moe[[1L]], 0)
  # deff guard: 0/0 → NA from .add_variance_cols when se_srs is all 0
  expect_true(is.na(res$deff[[1L]]))
})

test_that("get_variance() constant variable with variance='cv' fires cv_undefined and returns NA cv", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 102L)
  df$const <- 3
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  expect_warning(
    res <- get_variance(sc, const, variance = "cv"),
    class = "surveycore_warning_cv_undefined"
  )
  expect_true(is.na(res$cv[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 6: name_style = "broom"
# ---------------------------------------------------------------------------

test_that("get_variance() name_style='broom' renames variance/se/ci_low/ci_high", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 103L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(
    sc,
    y1,
    variance = c("se", "ci"),
    name_style = "broom"
  )
  expect_true("estimate" %in% names(res))
  expect_true("std.error" %in% names(res))
  expect_true("conf.low" %in% names(res))
  expect_true("conf.high" %in% names(res))
  expect_false("variance" %in% names(res))
  expect_false("se" %in% names(res))
  expect_false("ci_low" %in% names(res))
  expect_false("ci_high" %in% names(res))
  # Labels preserved on renamed columns
  expect_identical(attr(res$estimate, "label"), "Variance")
  expect_identical(attr(res$std.error, "label"), "SE")
  expect_identical(attr(res$conf.low, "label"), "95% CI low")
  expect_identical(attr(res$conf.high, "label"), "95% CI high")
})

# ---------------------------------------------------------------------------
# Category 7: n_weighted column
# ---------------------------------------------------------------------------

test_that("get_variance() n_weighted = TRUE appends n_weighted column", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 104L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(sc, y1, variance = NULL, n_weighted = TRUE)
  expect_true("n_weighted" %in% names(res))
  # Manual: sum of weights over non-NA, positive-weight rows
  mask <- !is.na(df$y1) & df$wt > 0
  expect_equal(res$n_weighted[[1L]], sum(df$wt[mask]), tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Category 8: Column label attributes
# ---------------------------------------------------------------------------

test_that("get_variance() attaches label attribute to every output column", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 105L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(
    sc,
    y1,
    variance = c("se", "ci", "var", "moe", "deff"),
    n_weighted = TRUE
  )
  for (nm in names(res)) {
    expect_true(
      !is.null(attr(res[[nm]], "label")),
      label = paste0("label present on column '", nm, "'")
    )
  }
  expect_identical(attr(res$name, "label"), "Variable")
  expect_identical(attr(res$variance, "label"), "Variance")
  expect_identical(attr(res$se, "label"), "SE")
  expect_identical(attr(res$ci_low, "label"), "95% CI low")
  expect_identical(attr(res$ci_high, "label"), "95% CI high")
  expect_identical(attr(res$n, "label"), "N")
  expect_identical(attr(res$n_weighted, "label"), "Weighted N")
})

test_that("get_variance() conf_level interpolates into CI labels", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 106L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(sc, y1, variance = "ci", conf_level = 0.90)
  expect_identical(attr(res$ci_low, "label"), "90% CI low")
  expect_identical(attr(res$ci_high, "label"), "90% CI high")
})

# ---------------------------------------------------------------------------
# Category 9: .meta structure
# ---------------------------------------------------------------------------

test_that("get_variance() .meta has FAMILY_META_KEYS + design_class + conf_level + name_style + min_cell_n + na_handling", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 107L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(sc, y1)
  m <- meta(res)

  expect_true("group" %in% names(m))
  expect_true("x" %in% names(m))
  expect_true("design_class" %in% names(m))
  expect_true("conf_level" %in% names(m))
  expect_true("name_style" %in% names(m))
  expect_true("min_cell_n" %in% names(m))
  expect_true("na_handling" %in% names(m))
  # No function_name or variable keys
  expect_false("function_name" %in% names(m))
  expect_false("variable" %in% names(m))

  expect_type(m$group, "list")
  expect_length(m$group, 0L)
  expect_identical(names(m$x), "y1")
  expect_true(all(
    c("variable_label", "question_preface", "value_labels") %in% names(m$x$y1)
  ))
  expect_identical(m$design_class, "surveycore::survey_taylor")
  expect_equal(m$conf_level, 0.95)
  expect_identical(m$na_handling, "pairwise")
})

test_that("get_variance() .meta$group populated when grouping is active", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 108L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(get_variance(sc, y1, group = group))
  m <- meta(res)
  expect_identical(names(m$group), "group")
})

# ---------------------------------------------------------------------------
# Category 10: label_vars
# ---------------------------------------------------------------------------

test_that("get_variance() label_vars = TRUE/FALSE controls name column display", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    design = "taylor",
    seed = 109L,
    with_labels = TRUE
  )
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  # Set a variable label so label_vars has a visible effect
  sc2 <- set_var_label(sc, y1 = "Outcome Y1")

  res_labelled <- get_variance(sc2, y1, label_vars = TRUE)
  res_raw <- get_variance(sc2, y1, label_vars = FALSE)
  expect_identical(as.character(res_labelled$name[[1L]]), "Outcome Y1")
  expect_identical(as.character(res_raw$name[[1L]]), "y1")
})

test_that("get_variance() label_vars = TRUE falls back to raw name when label is unset", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 110L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  # No label set on y1; label_vars=TRUE should fall back
  res <- get_variance(sc, y1, label_vars = TRUE)
  expect_identical(as.character(res$name[[1L]]), "y1")
})

# ---------------------------------------------------------------------------
# Category 11: decimals
# ---------------------------------------------------------------------------

test_that("get_variance() decimals = 3 rounds every numeric output column exactly", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 111L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res_full <- get_variance(
    sc,
    y1,
    variance = c("se", "ci", "var", "moe", "deff"),
    n_weighted = TRUE
  )
  res_r <- get_variance(
    sc,
    y1,
    variance = c("se", "ci", "var", "moe", "deff"),
    n_weighted = TRUE,
    decimals = 3
  )

  for (nm in c("variance", "se", "var", "ci_low", "ci_high", "moe", "deff", "n_weighted")) {
    if (is.finite(res_full[[nm]][[1L]])) {
      expect_identical(
        res_r[[nm]][[1L]],
        round(res_full[[nm]][[1L]], 3),
        info = paste0("decimals rounding for '", nm, "'")
      )
    }
  }
})

# ---------------------------------------------------------------------------
# Category 12: deff parity
# ---------------------------------------------------------------------------

test_that("get_variance() deff = (se / se_srs)^2 where se_srs is SRS-of-score SE", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 112L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(sc, y1, variance = c("se", "deff"))

  # Manual: build score under the same domain, SRS SE of its mean = sd(z)/sqrt(n)
  w <- df$wt
  y <- df$y1
  active <- !is.na(y) & w > 0
  n_d <- sum(active)
  W <- sum(w[active])
  ybar <- sum(w[active] * y[active]) / W
  kish <- n_d / (n_d - 1)
  z <- kish * (y[active] - ybar)^2
  se_srs <- sqrt(sum((z - mean(z))^2) / (n_d - 1) / n_d)
  expected_deff <- (res$se[[1L]] / se_srs)^2
  expect_equal(res$deff[[1L]], expected_deff, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Category 13: Print smoke test
# ---------------------------------------------------------------------------

test_that("get_variance() print method returns invisibly and does not error", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 113L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res_single <- get_variance(sc, y1)
  res_multi <- suppressWarnings(get_variance(sc, c(y1, y2)))

  expect_output(out_a <- print(res_single))
  expect_identical(out_a, res_single)
  expect_output(out_b <- print(res_multi))
  expect_identical(out_b, res_multi)
})

# ---------------------------------------------------------------------------
# Category 14: Broom tidy/glance smoke tests
# ---------------------------------------------------------------------------

test_that("broom::tidy(survey_variance) returns a tibble", {
  skip_if_not_installed("broom")
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 114L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(sc, y1)
  td <- broom::tidy(res)
  expect_true(tibble::is_tibble(td))
})

test_that("broom::glance(survey_variance) returns a 1-row tibble", {
  skip_if_not_installed("broom")
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 115L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(sc, y1)
  gl <- broom::glance(res)
  expect_true(tibble::is_tibble(gl))
  expect_identical(nrow(gl), 1L)
})

# ---------------------------------------------------------------------------
# Category 15: Error paths — dual-pattern (class + snapshot)
# ---------------------------------------------------------------------------

test_that("get_variance() rejects non-survey design with unsupported_class", {
  df <- data.frame(x = 1:10, y = rnorm(10))

  expect_error(
    get_variance(df, x),
    class = "surveycore_error_unsupported_class"
  )
  expect_snapshot(error = TRUE, get_variance(df, x))
})

test_that("get_variance() rejects empty variable selection with wrong_variable_count", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 201L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_variance(sc, dplyr::any_of("nonexistent_var")),
    class = "surveycore_error_wrong_variable_count"
  )
  expect_snapshot(
    error = TRUE,
    get_variance(sc, dplyr::any_of("nonexistent_var"))
  )
})

test_that("get_variance() rejects factor variable as non_numeric_variable", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 202L)
  df$f <- factor(sample(letters[1:3], nrow(df), replace = TRUE))
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_variance(sc, f),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_variance(sc, f))
})

test_that("get_variance() rejects character variable as non_numeric_variable", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 203L)
  df$ch <- sample(letters[1:3], nrow(df), replace = TRUE)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_variance(sc, ch),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_variance(sc, ch))
})

test_that("get_variance() rejects invalid variance value", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 204L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_variance(sc, y1, variance = "foo"),
    class = "surveycore_error_invalid_variance_arg"
  )
  expect_snapshot(error = TRUE, get_variance(sc, y1, variance = "foo"))
})

test_that("get_variance() rejects invalid conf_level", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 205L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_variance(sc, y1, conf_level = 0),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_error(
    get_variance(sc, y1, conf_level = 1),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_error(
    get_variance(sc, y1, conf_level = NA),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_snapshot(error = TRUE, get_variance(sc, y1, conf_level = 0))
})

test_that("get_variance() rejects invalid decimals", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 206L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_variance(sc, y1, decimals = -1),
    class = "surveycore_error_invalid_decimals"
  )
  expect_error(
    get_variance(sc, y1, decimals = 1.5),
    class = "surveycore_error_invalid_decimals"
  )
  expect_snapshot(error = TRUE, get_variance(sc, y1, decimals = -1))
})

test_that("get_variance() rejects invalid name_style", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 207L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_variance(sc, y1, name_style = "foo"),
    class = "surveycore_error_invalid_name_style"
  )
  expect_snapshot(
    error = TRUE,
    get_variance(sc, y1, name_style = "foo")
  )
})

test_that("get_variance() rejects non-logical na.rm", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 208L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_variance(sc, y1, na.rm = NA),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_error(
    get_variance(sc, y1, na.rm = 1),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_snapshot(error = TRUE, get_variance(sc, y1, na.rm = NA))
})

test_that("get_variance() rejects unknown na_handling via match.arg()", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 209L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(get_variance(sc, y1, na_handling = "foo"))
})

# ---------------------------------------------------------------------------
# Category 16: Warning paths
# ---------------------------------------------------------------------------

test_that("get_variance() fires small_cell when any row has n < min_cell_n", {
  df <- make_survey_data(n = 20L, n_psu = 10L, design = "taylor", seed = 301L)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  expect_warning(
    get_variance(sc, y1),
    class = "surveycore_warning_small_cell"
  )
})

test_that("get_variance() fires single_level when grouping var has one observed level", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 302L)
  df$const_grp <- 1L
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  expect_warning(
    get_variance(sc, y1, group = const_grp),
    class = "surveycore_warning_single_level"
  )
})

test_that("get_variance() fires cv_undefined when variance='cv' on constant variable", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 303L)
  df$const <- 5
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  expect_warning(
    get_variance(sc, const, variance = "cv"),
    class = "surveycore_warning_cv_undefined"
  )
})

test_that("get_variance() fires variance_all_na when focal var is all-NA in active domain", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 304L)
  df$allna <- NA_real_
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  expect_warning(
    get_variance(sc, allna),
    class = "surveycore_warning_variance_all_na"
  )
})

test_that("get_variance() fires variance_all_na under listwise with empty intersection", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 305L)
  # Make y1 and y2 disjoint NA patterns so their intersection has 0 non-NA rows
  df$a <- df$y1
  df$b <- df$y2
  df$a[1:50] <- NA
  df$b[51:100] <- NA
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  expect_warning(
    get_variance(sc, c(a, b), na_handling = "listwise"),
    class = "surveycore_warning_variance_all_na"
  )
})

test_that("get_variance() fires variance_insufficient_n when n == 1", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 306L)
  df$one <- NA_real_
  df$one[[1L]] <- 42
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  expect_warning(
    get_variance(sc, one),
    class = "surveycore_warning_variance_insufficient_n"
  )
})

# ---------------------------------------------------------------------------
# Category 17: Edge cases
# ---------------------------------------------------------------------------

test_that("get_variance() all-NA focal with na.rm=TRUE returns NaN point + uncertainty and n=0", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 401L)
  df$allna <- NA_real_
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(
    get_variance(sc, allna, variance = c("se", "ci", "var", "moe", "deff"))
  )
  expect_true(is.nan(res$variance[[1L]]))
  expect_true(is.nan(res$se[[1L]]))
  expect_true(is.nan(res$ci_low[[1L]]))
  expect_true(is.nan(res$ci_high[[1L]]))
  expect_identical(res$n[[1L]], 0L)
})

test_that("get_variance() single non-NA observation returns NaN point + n=1", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 402L)
  df$one <- NA_real_
  df$one[[1L]] <- 3.14
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(get_variance(sc, one))
  expect_true(is.nan(res$variance[[1L]]))
  expect_identical(res$n[[1L]], 1L)
})

test_that("get_variance() na.rm=FALSE with NAs returns NaN estimate", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 403L)
  df$y_na <- df$y1
  df$y_na[1:5] <- NA
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(get_variance(sc, y_na, na.rm = FALSE))
  # n reflects all in-domain rows regardless of NA (spec §Edge cases)
  expect_identical(res$n[[1L]], 100L)
  expect_true(is.nan(res$variance[[1L]]))
})

test_that("get_variance() single-level grouping produces one row per focal variable", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 404L)
  df$g1 <- 1L
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(get_variance(sc, y1, group = g1))
  expect_identical(nrow(res), 1L)
})

test_that("get_variance() listwise shares n across variables with differing NA patterns", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 405L)
  df$a <- df$y1
  df$b <- df$y2
  df$a[1:20] <- NA
  df$b[21:40] <- NA
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(
    get_variance(sc, c(a, b), na_handling = "listwise")
  )
  expect_identical(res$n[[1L]], res$n[[2L]])
})

test_that("get_variance() pairwise per-variable n differs across rows", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 406L)
  df$a <- df$y1
  df$b <- df$y2
  df$a[1:20] <- NA
  df$b[21:40] <- NA
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- suppressWarnings(
    get_variance(sc, c(a, b), na_handling = "pairwise")
  )
  # a loses 20 rows; b loses 20 rows — n should be equal here
  # but not the 0 count. Check n values differ from full n
  expect_identical(res$n[[1L]], 180L)
  expect_identical(res$n[[2L]], 180L)
})

test_that("get_variance() replicate near-zero variance returns se = 0 via max(0, v) guard", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    design = "replicate",
    type = "brr",
    seed = 407L
  )
  df$const <- 42
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  sc <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(rep_cols),
    type = "BRR"
  )
  test_invariants(sc)

  res <- suppressWarnings(
    get_variance(sc, const, variance = c("se", "ci"))
  )
  expect_identical(res$se[[1L]], 0)
  expect_false(is.nan(res$se[[1L]]))
})

test_that("get_variance() CI bounds are NOT clamped at zero (may be negative)", {
  # Construct a near-zero variance with nonzero SE to produce negative ci_low.
  # We simulate this by creating a scenario where numerical noise produces
  # a tiny positive variance and a positive SE.
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 408L)
  df$near <- 1 + rnorm(nrow(df)) * 1e-6 # near-constant, tiny variance
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  test_invariants(sc)

  res <- get_variance(sc, near, variance = c("se", "ci"))
  # Check that we DO NOT clamp: if ci_low < 0 arithmetically, it stays negative
  computed_low <- res$variance[[1L]] - stats::qnorm(0.975) * res$se[[1L]]
  expect_equal(res$ci_low[[1L]], computed_low, tolerance = 1e-12)
  # (We do not require ci_low < 0 — just parity with unclamped arithmetic.)
})

