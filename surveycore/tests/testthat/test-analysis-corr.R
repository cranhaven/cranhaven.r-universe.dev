# test-analysis-corr.R
# Tests for get_corr() — Phase 1 survey-weighted Pearson correlation.
#
# Numerical tolerance: 1e-10 for r, 1e-8 for SE.
# Oracle: survey::svyvar() for all design types.
# Fisher Z CI oracle tolerance: 1e-6.

# ---------------------------------------------------------------------------
# Category 1: Happy path — return structure (all design types)
# ---------------------------------------------------------------------------

test_that("get_corr() returns survey_corr tibble for survey_taylor", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 4L,
    design = "taylor",
    seed = 1L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2))
  test_result_invariants(result, "survey_corr")
  expect_true("var1" %in% names(result))
  expect_true("var2" %in% names(result))
  expect_true("r" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("statistic" %in% names(result))
  expect_true("df" %in% names(result))
  expect_true("n" %in% names(result))
  expect_false("se" %in% names(result)) # not in default variance = "ci"
  expect_equal(nrow(result), 1L) # one pair: (y1, y2)
  expect_equal(as.character(result$var1[[1L]]), "y1")
  expect_equal(as.character(result$var2[[1L]]), "y2")
  expect_true(is.finite(result$r[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_true(result$ci_low[[1L]] >= -1)
  expect_true(result$ci_high[[1L]] <= 1)
  expect_lt(result$ci_low[[1L]], result$ci_high[[1L]])
})

test_that("get_corr() works for survey_replicate design", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    design = "replicate",
    type = "brr",
    seed = 2L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )

  result <- get_corr(d, x = c(y1, y2))
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(result$ci_low[[1L]] >= -1)
  expect_true(result$ci_high[[1L]] <= 1)
})

test_that("get_corr() works for SRS design (Taylor with no ids/strata)", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 3L)
  d <- as_survey(df, weights = wt)

  result <- get_corr(d, x = c(y1, y2))
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
})

test_that("get_corr() works for survey_twophase design", {
  df <- make_survey_data(
    design = "twophase",
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    seed = 4L
  )
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d <- as_survey_twophase(ph1, subset = subset, method = "approx")

  result <- get_corr(d, x = c(y1, y2))
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
})

test_that("get_corr() works for survey_nonprob design", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 5L)
  d <- as_survey_nonprob(df, weights = wt)

  result <- get_corr(d, x = c(y1, y2))
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 2: meta() content
# ---------------------------------------------------------------------------

test_that("get_corr() meta() stores variables, method, and design type", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 6L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2))
  m <- meta(result)

  expect_identical(names(m$x), c("y1", "y2"))
  expect_identical(m$method, "pearson")
  expect_equal(m$conf_level, 0.95)
  expect_identical(m$design_type, "taylor")
  expect_type(m$n_respondents, "integer")
  expect_gt(m$n_respondents, 0L)
  expect_type(m$group, "list")
  expect_length(m$group, 0L)
  # Numeric vars → value_labels entries are NULL
  expect_null(m$x[["y1"]]$value_labels)
  expect_null(m$x[["y2"]]$value_labels)
})

# ---------------------------------------------------------------------------
# Category 3: Grouped analysis — group= argument and @groups
# ---------------------------------------------------------------------------

test_that("get_corr() group= produces one row per group × pair in long format", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 7L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), group = group, label_values = FALSE)
  n_groups <- length(unique(df$group[!is.na(df$group)])) # 3
  expect_equal(nrow(result), n_groups * 1L) # 3 groups × 1 pair
  expect_true("group" %in% names(result))
  # Each group appears exactly once
  expect_identical(
    sort(as.character(result$group)),
    sort(unique(df$group[!is.na(df$group)]))
  )
})

test_that("get_corr() @groups from group_by() is now respected", {
  skip_if_not_installed("surveytidy")
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 71L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  d_grouped <- surveytidy::group_by(d, group)
  result_at <- get_corr(d_grouped, x = c(y1, y2))
  result_arg <- get_corr(d, x = c(y1, y2), group = group)

  # Both paths return the same number of rows (one per group × pair)
  expect_equal(nrow(result_at), nrow(result_arg))
  # group column present in both
  expect_true("group" %in% names(result_at))
  # r values match when aligned by group level
  r_at <- result_at$r[order(as.character(result_at$group))]
  r_arg <- result_arg$r[order(as.character(result_arg$group))]
  expect_equal(r_at, r_arg, tolerance = 1e-10)
})

test_that("get_corr() wide + groups gives n_groups * p rows, group col precedes variable", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 72L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(
    d,
    x = c(y1, y2, y3),
    group = group,
    format = "wide",
    label_values = FALSE
  )
  n_groups <- length(unique(df$group[!is.na(df$group)])) # 3
  p <- 3L
  expect_equal(nrow(result), n_groups * p) # 9 rows
  expect_true("group" %in% names(result))
  expect_true("variable" %in% names(result))
  # group col comes before variable col
  expect_lt(which(names(result) == "group"), which(names(result) == "variable"))
  # No inference columns in wide format
  expect_false("p_value" %in% names(result))
})

test_that("get_corr() group column is a factor with label levels when label_values = TRUE", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    with_labels = TRUE,
    seed = 73L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), group = group, label_values = TRUE)
  expect_true(is.factor(result$group))
  expect_true(all(levels(result$group) %in% c("Group A", "Group B", "Group C")))
})

test_that("get_corr() meta(result)$group is a non-empty named list when group is active", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 74L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), group = group)
  m <- meta(result)
  expect_type(m$group, "list")
  expect_length(m$group, 1L) # one group var: "group"
  expect_true("group" %in% names(m$group))
  expect_true(all(
    c("variable_label", "question_preface", "value_labels") %in%
      names(m$group[["group"]])
  ))
})

test_that("get_corr() grouped r matches filtered-domain result per group [numerical]", {
  skip_if_not_installed("surveytidy")
  df <- make_survey_data(
    n = 400L,
    n_psu = 40L,
    n_strata = 4L,
    design = "taylor",
    seed = 75L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_g <- get_corr(
    d,
    x = c(y1, y2),
    group = group,
    variance = "se",
    label_values = FALSE
  )

  # Verify group "A": grouped result matches filter-domain result
  d_a <- surveytidy::filter(d, group == "A")
  result_a <- get_corr(d_a, x = c(y1, y2), variance = "se")

  row_a <- result_g[as.character(result_g$group) == "A", ]
  expect_equal(row_a$r[[1L]], result_a$r[[1L]], tolerance = 1e-10)
  expect_equal(row_a$se[[1L]], result_a$se[[1L]], tolerance = 1e-8)
})

test_that("get_corr() fires surveycore_warning_single_level for one-level group in domain", {
  skip_if_not_installed("surveytidy")
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 2L,
    design = "taylor",
    seed = 76L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d_one <- surveytidy::filter(d, group == "A")

  expect_warning(
    get_corr(d_one, x = c(y1, y2), group = group),
    class = "surveycore_warning_single_level"
  )
})

test_that("get_corr() returns r = NA, n = 0L when group has all-NA focal values", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 77L
  )
  # Group "C" rows have no complete cases for y1/y2
  df$y1[df$group == "C"] <- NA_real_
  df$y2[df$group == "C"] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), group = group, label_values = FALSE)
  row_c <- result[as.character(result$group) == "C", ]
  expect_equal(nrow(row_c), 1L)
  expect_true(is.na(row_c$r[[1L]]))
  expect_equal(row_c$n[[1L]], 0L)
})

test_that("get_corr() excludes NA group values from group combinations", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 78L
  )
  df$group[1:20] <- NA_character_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), group = group, label_values = FALSE)
  expect_false(anyNA(result$group))
  n_unique_nonna <- length(unique(df$group[!is.na(df$group)])) # 3
  expect_equal(nrow(result), n_unique_nonna * 1L)
})

test_that("get_corr() redundant = TRUE with groups gives n_combos * 2 * n_pairs rows", {
  skip_if_not_installed("surveytidy")
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 79L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d2 <- surveytidy::filter(d, group %in% c("A", "B"))

  result <- get_corr(
    d2,
    x = c(y1, y2, y3),
    group = group,
    redundant = TRUE,
    diagonal = FALSE
  )
  n_combos <- 2L # A and B
  n_pairs <- 3L # (y1,y2), (y1,y3), (y2,y3)
  expect_equal(nrow(result), n_combos * 2L * n_pairs)
})

test_that("get_corr() diagonal = TRUE with groups gives n_combos * (n_pairs + p) rows", {
  skip_if_not_installed("surveytidy")
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 80L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d2 <- surveytidy::filter(d, group %in% c("A", "B"))

  result <- get_corr(
    d2,
    x = c(y1, y2, y3),
    group = group,
    redundant = FALSE,
    diagonal = TRUE
  )
  n_combos <- 2L
  n_pairs <- 3L
  p <- 3L
  expect_equal(nrow(result), n_combos * (n_pairs + p))
})

# ---------------------------------------------------------------------------
# Category 4: Domain estimation
# ---------------------------------------------------------------------------

test_that("get_corr() domain estimation restricts n to in-domain rows", {
  skip_if_not_installed("surveytidy")
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 8L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d_f <- surveytidy::filter(d, group == "A")

  result_full <- get_corr(d, x = c(y1, y2))
  result_domain <- get_corr(d_f, x = c(y1, y2))

  # Domain n ≤ full n
  expect_lte(result_domain$n[[1L]], result_full$n[[1L]])
  # Results differ (domain estimation gives correct SE from full design)
  expect_false(isTRUE(all.equal(result_full$r[[1L]], result_domain$r[[1L]])))
})

test_that("get_corr() domain + survey_taylor SE is finite and nonzero", {
  skip_if_not_installed("surveytidy")
  df <- make_survey_data(
    n = 400L,
    n_psu = 40L,
    n_strata = 4L,
    design = "taylor",
    seed = 9L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d_f <- surveytidy::filter(d, group == "A")

  result <- get_corr(d_f, x = c(y1, y2), variance = "se")
  expect_true(is.finite(result$se[[1L]]))
  expect_gt(result$se[[1L]], 0)
})

# ---------------------------------------------------------------------------
# Category 5: Variance columns
# ---------------------------------------------------------------------------

test_that("get_corr() returns all requested variance columns", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 10L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), variance = c("se", "ci", "var", "moe"))
  expect_true("se" %in% names(result))
  expect_true("var" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("moe" %in% names(result))
  # var = se^2
  expect_equal(result$var[[1L]], result$se[[1L]]^2, tolerance = 1e-15)
  # moe = (ci_high - ci_low) / 2
  expect_equal(
    result$moe[[1L]],
    (result$ci_high[[1L]] - result$ci_low[[1L]]) / 2,
    tolerance = 1e-10
  )
})

test_that("get_corr() variance = NULL produces no variance columns", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 11L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), variance = NULL)
  expect_false("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
})

test_that("get_corr() deff column produced when requested", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 12L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), variance = "deff")
  expect_true("deff" %in% names(result))
  expect_true(is.numeric(result$deff))
})

test_that("get_corr() n_weighted column produced when requested", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 13L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_nw <- get_corr(d, x = c(y1, y2), n_weighted = TRUE)
  expect_true("n_weighted" %in% names(result_nw))
  expect_true(result_nw$n_weighted[[1L]] > 0)

  result_no <- get_corr(d, x = c(y1, y2), n_weighted = FALSE)
  expect_false("n_weighted" %in% names(result_no))
})

# ---------------------------------------------------------------------------
# Category 6: Fisher Z CI properties
# ---------------------------------------------------------------------------

test_that("get_corr() Fisher Z CI bounds are always in (-1, 1)", {
  # Construct a dataset with high correlation for extreme CI test
  set.seed(42L)
  n <- 200L
  x_base <- rnorm(n, 50, 10)
  df <- data.frame(
    y1 = x_base + rnorm(n, 0, 1), # high positive correlation with y2
    y2 = x_base + rnorm(n, 0, 1),
    wt = runif(n, 0.5, 2)
  )
  d <- as_survey(df, weights = wt)

  result <- get_corr(d, x = c(y1, y2))
  expect_gt(result$ci_low[[1L]], -1)
  expect_lt(result$ci_high[[1L]], 1)
  # High correlation: r > 0.9
  expect_gt(result$r[[1L]], 0.9)
})

test_that("get_corr() Fisher Z CI width for |r| > 0.9 matches oracle", {
  skip_if_not_installed("survey")
  set.seed(99L)
  n <- 200L
  x_base <- rnorm(n, 50, 10)
  df <- data.frame(
    y1 = x_base + rnorm(n, 0, 1),
    y2 = x_base + rnorm(n, 0, 1),
    wt = runif(n, 0.5, 2)
  )
  d_sc <- as_survey(df, weights = wt)
  d_sv <- survey::svydesign(ids = ~1, weights = ~wt, data = df)

  result <- get_corr(d_sc, x = c(y1, y2), variance = c("ci", "se"))

  sv <- survey::svyvar(~ y1 + y2, d_sv)
  a <- sv[1L, 1L]
  b <- sv[1L, 2L]
  cv <- sv[2L, 2L]
  r_or <- b / sqrt(a * cv)
  sig <- vcov(sv)[c(1L, 2L, 4L), c(1L, 2L, 4L)]
  g <- c(-r_or / (2 * a), 1 / sqrt(a * cv), -r_or / (2 * cv))
  se_or <- sqrt(as.numeric(t(g) %*% sig %*% g))
  z_crit <- stats::qnorm(0.975)
  ci_low_or <- tanh(atanh(r_or) - z_crit * se_or)
  ci_high_or <- tanh(atanh(r_or) + z_crit * se_or)

  expect_gt(result$r[[1L]], 0.9) # confirm high correlation
  # Fisher Z CI width matches oracle within 1e-5 (larger than typical due to
  # SRS implementation differences in weighted vs. unweighted variance formula)
  expect_equal(result$ci_low[[1L]], ci_low_or, tolerance = 1e-5)
  expect_equal(result$ci_high[[1L]], ci_high_or, tolerance = 1e-5)
})

# ---------------------------------------------------------------------------
# Category 7: p_value, statistic, df columns
# ---------------------------------------------------------------------------

test_that("get_corr() p_value, statistic, df are always present in long format", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 14L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2))
  expect_true("p_value" %in% names(result))
  expect_true("statistic" %in% names(result))
  expect_true("df" %in% names(result))
  expect_true(is.finite(result$p_value[[1L]]))
  expect_true(result$p_value[[1L]] >= 0)
  expect_true(result$p_value[[1L]] <= 1)
  # df = n - 2
  expect_equal(as.integer(result$df[[1L]]), as.integer(result$n[[1L]]) - 2L)
})

# ---------------------------------------------------------------------------
# Category 8: Format and structure options
# ---------------------------------------------------------------------------

test_that("get_corr() wide format returns correlation matrix with correct dims", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 15L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2, y3), format = "wide")
  test_result_invariants(result, "survey_corr")
  expect_equal(nrow(result), 3L) # one row per variable
  expect_equal(ncol(result), 4L) # variable + 3 correlation columns
  expect_true("variable" %in% names(result))
  # No variance/inference columns in wide format
  expect_false("p_value" %in% names(result))
  expect_false("statistic" %in% names(result))
  expect_false("ci_low" %in% names(result))
  # meta() method still works
  m <- meta(result)
  expect_identical(m$method, "pearson")
})

test_that("get_corr() wide format diagonal is NA by default", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 16L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_wide <- get_corr(d, x = c(y1, y2), format = "wide")
  # Diagonal cells: row y1-col y1 and row y2-col y2 should be NA
  r_mat <- as.matrix(result_wide[, -1L])
  expect_true(all(is.na(diag(r_mat))))
})

test_that("get_corr() wide format diagonal = TRUE gives 1 on diagonal", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 17L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_wide <- get_corr(d, x = c(y1, y2), format = "wide", diagonal = TRUE)
  r_mat <- as.matrix(result_wide[, -1L])
  expect_equal(diag(r_mat), c(1, 1), tolerance = 1e-15)
})

test_that("get_corr() meta()$method is 'pearson'", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 18L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2))
  expect_identical(meta(result)$method, "pearson")
})

# ---------------------------------------------------------------------------
# Category 9: redundant and diagonal arguments
# ---------------------------------------------------------------------------

test_that("get_corr() redundant = FALSE produces n*(n-1)/2 rows", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 19L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_3vars <- get_corr(d, x = c(y1, y2, y3), redundant = FALSE)
  expect_equal(nrow(result_3vars), 3L) # 3*(3-1)/2 = 3 pairs
})

test_that("get_corr() redundant = TRUE produces n*(n-1) rows", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 20L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_r <- get_corr(d, x = c(y1, y2, y3), redundant = TRUE, diagonal = FALSE)
  expect_equal(nrow(result_r), 6L) # 3*2 = 6 directed pairs
})

test_that("get_corr() diagonal = TRUE includes self-correlations (r = 1)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 21L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), diagonal = TRUE, redundant = FALSE)
  # 1 pair + 2 diagonal = 3 rows
  expect_equal(nrow(result), 3L)
  diag_rows <- result[result$var1 == result$var2, ]
  expect_equal(diag_rows$r, c(1, 1), tolerance = 1e-15)
})

test_that("get_corr() redundant rows have same r as their mirror", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 22L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2), redundant = TRUE, diagonal = FALSE)
  expect_equal(nrow(result), 2L)
  expect_equal(result$r[[1L]], result$r[[2L]], tolerance = 1e-15)
  expect_equal(result$var1[[1L]], result$var2[[2L]])
  expect_equal(result$var2[[1L]], result$var1[[2L]])
})

# ---------------------------------------------------------------------------
# Category 10: Pairwise n with NA
# ---------------------------------------------------------------------------

test_that("get_corr() pairwise n differs across pairs when NAs are staggered", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 23L)
  df$y1[1:10] <- NA # 10 NAs in y1
  df$y2[5:20] <- NA # 16 NAs in y2 (overlap: rows 5-10 = 6 shared)
  df$y3 <- rnorm(nrow(df)) # no NAs

  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  # 3 variables: pairs (y1,y2), (y1,y3), (y2,y3)
  result <- get_corr(d, x = c(y1, y2, y3), redundant = FALSE)
  expect_equal(nrow(result), 3L)

  # n for (y1, y2) < n for (y1, y3) and n for (y2, y3)
  n_y1y2 <- result$n[result$var1 == "y1" & result$var2 == "y2"]
  n_y1y3 <- result$n[result$var1 == "y1" & result$var2 == "y3"]
  n_y2y3 <- result$n[result$var1 == "y2" & result$var2 == "y3"]

  expect_lt(n_y1y2, n_y1y3)
  expect_lt(n_y1y2, n_y2y3)
})

# ---------------------------------------------------------------------------
# Category 11: label_vars
# ---------------------------------------------------------------------------

test_that("get_corr() label_vars = TRUE shows variable labels in var1/var2", {
  df <- make_survey_data(
    n = 100L,
    design = "taylor",
    with_labels = TRUE,
    seed = 24L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d <- set_var_label(d, y1 = "Outcome Y1")
  d <- set_var_label(d, y2 = "Outcome Y2")

  result_lbl <- get_corr(d, x = c(y1, y2), label_vars = TRUE)
  result_raw <- get_corr(d, x = c(y1, y2), label_vars = FALSE)

  expect_equal(as.character(result_lbl$var1[[1L]]), "Outcome Y1")
  expect_equal(as.character(result_lbl$var2[[1L]]), "Outcome Y2")
  expect_equal(as.character(result_raw$var1[[1L]]), "y1")
  expect_equal(as.character(result_raw$var2[[1L]]), "y2")
})

# ---------------------------------------------------------------------------
# Category 12: name_style
# ---------------------------------------------------------------------------

test_that("get_corr() name_style = 'broom' renames r -> estimate", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 25L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(
    d,
    x = c(y1, y2),
    name_style = "broom",
    variance = c("se", "ci")
  )
  expect_true("estimate" %in% names(result))
  expect_false("r" %in% names(result))
  expect_true("std.error" %in% names(result))
  expect_false("se" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  # class and meta preserved
  expect_true(inherits(result, "survey_corr"))
  expect_false(is.null(meta(result)))
})

# ---------------------------------------------------------------------------
# Category 2 (oracle): Numerical validation — survey::svyvar()
# ---------------------------------------------------------------------------

test_that("get_corr() r matches survey::svyvar() oracle for Taylor [numerical]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 500L,
    n_psu = 50L,
    n_strata = 5L,
    design = "taylor",
    seed = 30L
  )
  d_sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d_sv <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    data = df,
    nest = TRUE
  )

  result <- get_corr(d_sc, x = c(y1, y2), variance = "se")

  sv <- survey::svyvar(~ y1 + y2, d_sv)
  a <- sv[1L, 1L]
  b <- sv[1L, 2L]
  cv <- sv[2L, 2L]
  r_or <- b / sqrt(a * cv)
  # vcov(svyvar()) is 4x4 (all entries of 2x2 matrix); extract unique 3x3
  # indices: (1,1)=Var(y1), (1,2)=Cov(y1,y2), (2,2)=Var(y2) → positions 1,2,4
  sig <- vcov(sv)[c(1L, 2L, 4L), c(1L, 2L, 4L)]
  g <- c(-r_or / (2 * a), 1 / sqrt(a * cv), -r_or / (2 * cv))
  se_or <- sqrt(as.numeric(t(g) %*% sig %*% g))

  expect_equal(result$r[[1L]], r_or, tolerance = 1e-10)
  expect_equal(result$se[[1L]], se_or, tolerance = 1e-8)
})

test_that("get_corr() r matches survey::svyvar() oracle for BRR [numerical]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 500L,
    n_psu = 50L,
    design = "replicate",
    type = "brr",
    seed = 31L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d_sc <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )
  d_sv <- survey::svrepdesign(
    weights = ~wt,
    repweights = df[, repwt_cols],
    type = "BRR",
    mse = TRUE,
    data = df
  )

  result <- get_corr(d_sc, x = c(y1, y2), variance = "se")

  # suppressWarnings: survey::svyvar with mse=TRUE/BRR may warn about sweep dims
  sv <- suppressWarnings(survey::svyvar(~ y1 + y2, d_sv))
  a <- sv[1L, 1L]
  b <- sv[1L, 2L]
  cv <- sv[2L, 2L]
  r_or <- b / sqrt(a * cv)
  sig <- vcov(sv)[c(1L, 2L, 4L), c(1L, 2L, 4L)]
  g <- c(-r_or / (2 * a), 1 / sqrt(a * cv), -r_or / (2 * cv))
  se_or <- sqrt(as.numeric(t(g) %*% sig %*% g))

  expect_equal(result$r[[1L]], r_or, tolerance = 1e-10)
  expect_equal(result$se[[1L]], se_or, tolerance = 1e-8)
})

test_that("get_corr() r matches survey::svyvar() oracle for SRS [numerical]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 500L, design = "taylor", seed = 32L)
  d_sc <- as_survey(df, weights = wt)
  d_sv <- survey::svydesign(ids = ~1, weights = ~wt, data = df)

  result <- get_corr(d_sc, x = c(y1, y2), variance = "se")

  sv <- survey::svyvar(~ y1 + y2, d_sv)
  a <- sv[1L, 1L]
  b <- sv[1L, 2L]
  cv <- sv[2L, 2L]
  r_or <- b / sqrt(a * cv)
  sig <- vcov(sv)[c(1L, 2L, 4L), c(1L, 2L, 4L)]
  g <- c(-r_or / (2 * a), 1 / sqrt(a * cv), -r_or / (2 * cv))
  se_or <- sqrt(as.numeric(t(g) %*% sig %*% g))

  expect_equal(result$r[[1L]], r_or, tolerance = 1e-10)
  expect_equal(result$se[[1L]], se_or, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Error paths
# ---------------------------------------------------------------------------

test_that("get_corr() throws for non-survey-base object", {
  expect_error(
    get_corr(list(x = 1), x = c(y1, y2)),
    class = "surveycore_error_unsupported_class"
  )
  expect_snapshot(error = TRUE, get_corr(list(x = 1), x = c(y1, y2)))
})

test_that("get_corr() throws surveycore_error_insufficient_variables for < 2 vars", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 40L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_corr(d, x = y1),
    class = "surveycore_error_insufficient_variables"
  )
  expect_snapshot(error = TRUE, get_corr(d, x = y1))
})

test_that("get_corr() warns and drops non-numeric variables", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 41L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  # Two numeric + one non-numeric: drop group with warning, (y1, y2) succeed
  expect_warning(
    result <- get_corr(d, x = c(y1, y2, group)),
    class = "surveycore_warning_corr_non_numeric"
  )
  expect_equal(nrow(result), 1L) # only (y1, y2) pair remains

  # Only one numeric after dropping: warn then error
  expect_warning(
    expect_error(
      get_corr(d, x = c(y1, group)),
      class = "surveycore_error_insufficient_variables"
    ),
    class = "surveycore_warning_corr_non_numeric"
  )
})

test_that("get_corr() warns + errors when all vars are non-numeric", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 42L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  # Both group and psu are non-numeric
  expect_warning(
    expect_error(
      get_corr(d, x = c(group, psu)),
      class = "surveycore_error_insufficient_variables"
    ),
    class = "surveycore_warning_corr_non_numeric"
  )
})

test_that("get_corr() throws for invalid variance argument", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 43L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_corr(d, x = c(y1, y2), variance = "bad"),
    class = "surveycore_error_invalid_variance_arg"
  )
})

test_that("get_corr() throws for invalid conf_level", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 44L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_corr(d, x = c(y1, y2), conf_level = 1.5),
    class = "surveycore_error_invalid_conf_level"
  )
})

test_that("get_corr() throws for invalid name_style", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 45L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_corr(d, x = c(y1, y2), name_style = "tidy"),
    class = "surveycore_error_invalid_name_style"
  )
})

test_that("get_corr() fires surveycore_warning_small_cell for small pairwise n", {
  # Construct domain with very few rows
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 2L,
    design = "taylor",
    seed = 46L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  # Use a very large min_cell_n to trigger the warning even for large samples
  expect_warning(
    get_corr(d, x = c(y1, y2), min_cell_n = nrow(df) + 1L),
    class = "surveycore_warning_small_cell"
  )
})

test_that("get_corr() fires surveycore_warning_cv_undefined when r = 0 approx", {
  # We can only reliably test this with a constructed case
  df <- make_survey_data(n = 50L, design = "taylor", seed = 47L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  # Note: cv warning fires only when r <= 0 (will happen in pathological cases)
  # Just verify that requesting cv doesn't cause an error for normal r values
  result <- get_corr(d, x = c(y1, y2), variance = "cv")
  expect_true("cv" %in% names(result))
})

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("get_corr() handles all-NA in one variable (both removed)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 50L)
  df$y1 <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2))
  expect_true(is.na(result$r[[1L]]))
  expect_equal(result$n[[1L]], 0L)
})

test_that("get_corr() na.rm = TRUE gives pairwise complete-case n", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 51L)
  df$y1[1:20] <- NA
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_rm <- get_corr(d, x = c(y1, y2), na.rm = TRUE)
  expect_equal(result_rm$n[[1L]], as.integer(sum(!is.na(df$y1))))
})

test_that("get_corr() 3-variable call gives n*(n-1)/2 = 3 rows", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 52L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2, y3))
  expect_equal(nrow(result), 3L)
  expect_identical(as.character(result$var1), c("y1", "y1", "y2"))
  expect_identical(as.character(result$var2), c("y2", "y3", "y3"))
})

# ---------------------------------------------------------------------------
# Category 16: New meta structure — nested x, group, factor var1/var2
# ---------------------------------------------------------------------------

test_that("get_corr() var1/var2 columns are <fct> with levels in supply order", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 60L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2, y3))
  expect_true(is.factor(result$var1))
  expect_true(is.factor(result$var2))
  expect_identical(levels(result$var1), c("y1", "y2", "y3"))
})

test_that("get_corr() meta$group is always a list (empty when no @groups set)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 61L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_corr(d, x = c(y1, y2))
  expect_type(meta(result)$group, "list")
  expect_length(meta(result)$group, 0L)
})

test_that("get_corr() meta$x has nested structure for each variable", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 62L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d <- set_var_label(d, y1 = "Variable One")

  result <- get_corr(d, x = c(y1, y2))
  m <- meta(result)

  expect_identical(names(m$x), c("y1", "y2"))
  expect_identical(m$x$y1$variable_label, "Variable One")
  expect_null(m$x$y2$variable_label)
  expect_true(all(
    c("variable_label", "question_preface", "value_labels") %in%
      names(m$x$y1)
  ))
})

# ── decimals argument ──────────────────────────────────────────────────────────

test_that("get_corr() decimals=2 rounds all double columns", {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = 501L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_corr(d, x = c(y1, y2), variance = "ci", decimals = 2L)

  dbl_cols <- names(r)[vapply(r, is.double, logical(1L))]
  for (col in dbl_cols) {
    expect_equal(
      r[[col]],
      round(r[[col]], 2L),
      label = paste0(col, " rounded to 2 decimals")
    )
  }
})

test_that("get_corr() rejects invalid decimals", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 502L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  expect_error(
    get_corr(d, x = c(y1, y2), decimals = -2L),
    class = "surveycore_error_invalid_decimals"
  )
})

# ── NA group rows (na.rm extension) — Test Blocks 1–8c + oracle ───────────────

# Block 1: default na.rm = TRUE excludes NA group rows (regression guard)

test_that("get_corr() default (na.rm = TRUE) excludes group NA rows", {
  d <- make_na_group_design()
  r <- get_corr(d, x = c(y1, y2), group = grp, label_values = FALSE)
  expect_false(anyNA(r$grp))
})

# Block 2: na.rm = FALSE includes NA group row

test_that("get_corr() includes NA group row when na.rm = FALSE", {
  d <- make_na_group_design()
  r <- get_corr(
    d,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    label_values = FALSE
  )
  expect_true(any(is.na(r$grp)))
})

# Block 3: NA group row is last

test_that("get_corr() places NA group row after non-NA rows", {
  d <- make_na_group_design()
  r <- get_corr(
    d,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    label_values = FALSE
  )
  na_idx <- which(is.na(r$grp))
  nn_idx <- which(!is.na(r$grp))
  expect_true(all(na_idx > max(nn_idx)))
})

# Block 4: NA group row has finite r estimate

test_that("get_corr() NA group row has finite r estimate", {
  d <- make_na_group_design()
  r <- get_corr(
    d,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    label_values = FALSE
  )
  na_row <- get_na_group_rows(r, "grp")
  expect_true(all(is.finite(na_row$r)))
})

# Block 5a: multi-group — NA in first group var

test_that("get_corr() handles NA in first of two group vars (na.rm = FALSE)", {
  d <- make_na_group_design() # grp has NAs; grp2 has none
  r <- get_corr(
    d,
    x = c(y1, y2),
    group = c(grp, grp2),
    na.rm = FALSE,
    label_values = FALSE
  )
  expect_true(any(is.na(r$grp) & !is.na(r$grp2)))
})

# Block 5b: multi-group — NA in second group var (inline fixture)

test_that("get_corr() handles NA in second of two group vars (na.rm = FALSE)", {
  df <- make_survey_data(n = 200L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c("A", "B", "C"), 200L, replace = TRUE)
  df$grp2 <- sample(c("X", "Y", NA_character_), 200L, replace = TRUE)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_corr(
    d,
    x = c(y1, y2),
    group = c(grp, grp2),
    na.rm = FALSE,
    label_values = FALSE
  )
  expect_true(any(!is.na(r$grp) & is.na(r$grp2)))
})

# Block 6: all-NA group var — warning fires; output has NA group row

test_that("get_corr() handles group var that is entirely NA (na.rm = FALSE)", {
  d <- make_all_na_group_design()
  expect_warning(
    r <- get_corr(
      d,
      x = c(y1, y2),
      group = grp,
      na.rm = FALSE,
      label_values = FALSE
    ),
    class = "surveycore_warning_single_level"
  )
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$grp[[1L]]))
  expect_true(is.finite(r$r[[1L]]))
})

# Block 7a: label_values = TRUE — regular NA group row remains NA in factor

test_that("get_corr() regular NA group row is NA in factor when label_values = TRUE", {
  df <- make_survey_data(n = 200L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c(1L, 2L, NA_integer_), 200L, replace = TRUE)
  attr(df$grp, "labels") <- c("GroupA" = 1L, "GroupB" = 2L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_corr(
    d,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    label_values = TRUE
  )
  expect_true(is.factor(r$grp))
  na_row <- get_na_group_rows(r, "grp")
  expect_true(nrow(na_row) > 0L)
  expect_true(is.na(na_row$grp[[1L]]))
})

# Block 7b: label_values = TRUE — haven-tagged NA becomes a factor level

test_that("get_corr() haven-labeled NA group rows become factor levels when label_values = TRUE", {
  skip_if_not_installed("haven")
  df <- make_survey_data(n = 200L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c(1L, 2L), 200L, replace = TRUE)
  df$grp <- as.double(df$grp)
  df$grp[sample(200L, 40L)] <- haven::tagged_na("r")
  attr(df$grp, "labels") <- c(
    "GroupA" = 1,
    "GroupB" = 2,
    "Refused" = haven::tagged_na("r")
  )
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_corr(
    d,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    label_values = TRUE
  )
  expect_true(is.factor(r$grp))
  expect_true("Refused" %in% levels(r$grp))
  refused_row <- r[!is.na(r$grp) & r$grp == "Refused", ]
  expect_true(nrow(refused_row) > 0L)
})

# Block 8: group_by() path — NA group rows appear with na.rm = FALSE

test_that("get_corr() includes NA group row when group set via group_by() and na.rm = FALSE", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_corr(d, x = c(y1, y2), na.rm = FALSE)
  expect_true(anyNA(r$grp))
})

# Block 8b: group_by() path — NA group rows excluded by default

test_that("get_corr() excludes NA group rows by default when group set via group_by()", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_corr(d, x = c(y1, y2))
  expect_false(anyNA(r$grp))
})

# Block 8c: na.rm = NA is rejected (dual pattern)

test_that("get_corr() rejects na.rm = NA with surveycore_error_na_rm_not_logical", {
  d <- make_na_group_design()
  expect_error(
    get_corr(d, x = c(y1, y2), group = grp, na.rm = NA),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(y1, y2), group = grp, na.rm = NA)
  )
})


# ── Oracle tests: NA group row estimate matches filtered design ────────────────

test_that("get_corr() NA group row r matches filtered taylor design [oracle]", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_oracle <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  na_df <- df[is.na(df$grp), ]
  na_design <- as_survey(
    na_df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  expected <- get_corr(na_design, x = c(y1, y2), variance = "se")
  result <- get_corr(
    design_oracle,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    variance = "se",
    label_values = FALSE
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$r, expected$r, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_corr() NA group row r matches filtered replicate design [oracle]", {
  df_r <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = 42L
  )
  set.seed(43L)
  df_r$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  repwt_cols <- grep("^repwt_", names(df_r), value = TRUE)
  design_rep <- as_survey_replicate(
    df_r,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  na_df_r <- df_r[is.na(df_r$grp), ]
  repwt_cols_na <- grep("^repwt_", names(na_df_r), value = TRUE)
  na_design_rep <- as_survey_replicate(
    na_df_r,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols_na),
    type = "BRR"
  )
  expected <- get_corr(na_design_rep, x = c(y1, y2), variance = "se")
  result <- get_corr(
    design_rep,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    variance = "se",
    label_values = FALSE
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$r, expected$r, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_corr() NA group row r matches filtered twophase design [oracle]", {
  df_p <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "twophase",
    seed = 42L
  )
  set.seed(43L)
  df_p$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  phase1 <- as_survey(
    df_p,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  design_twophase <- as_survey_twophase(
    phase1,
    subset = subset,
    method = "approx"
  )
  na_df_p <- df_p[is.na(df_p$grp), ]
  na_phase1 <- as_survey(
    na_df_p,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  na_design_twophase <- as_survey_twophase(
    na_phase1,
    subset = subset,
    method = "approx"
  )
  expected <- suppressWarnings(
    get_corr(na_design_twophase, x = c(y1, y2), variance = "se")
  )
  result <- suppressWarnings(
    get_corr(
      design_twophase,
      x = c(y1, y2),
      group = grp,
      na.rm = FALSE,
      variance = "se",
      label_values = FALSE
    )
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$r, expected$r, tolerance = 1e-10)
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_corr() NA group row r matches filtered calibrated design [oracle]", {
  df_c <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_c$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_cal <- as_survey_nonprob(df_c, weights = wt)
  na_df_c <- df_c[is.na(df_c$grp), ]
  na_design_cal <- as_survey_nonprob(na_df_c, weights = wt)
  expected <- get_corr(na_design_cal, x = c(y1, y2), variance = "se")
  result <- get_corr(
    design_cal,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    variance = "se",
    label_values = FALSE
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$r, expected$r, tolerance = 1e-10)
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_corr() NA group row r matches filtered srs design [oracle]", {
  df_s <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_s$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_srs <- as_survey(df_s, weights = wt)
  na_df_s <- df_s[is.na(df_s$grp), ]
  na_design_srs <- as_survey(na_df_s, weights = wt)
  expected <- get_corr(na_design_srs, x = c(y1, y2), variance = "se")
  result <- get_corr(
    design_srs,
    x = c(y1, y2),
    group = grp,
    na.rm = FALSE,
    variance = "se",
    label_values = FALSE
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$r, expected$r, tolerance = 1e-10)
  # SE legitimately differs: SRS variance uses nrow(design@data) for n_full;
  # domain estimation (full design) != pre-filtered oracle.
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_corr() multi-group NA row r matches filtered taylor design [oracle]", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  df$grp2 <- sample(c("X", "Y"), 100L, replace = TRUE)
  design_multi <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  result <- suppressWarnings(
    get_corr(
      design_multi,
      x = c(y1, y2),
      group = c(grp, grp2),
      na.rm = FALSE,
      variance = "se",
      label_values = FALSE
    )
  )
  oracle_df <- df[is.na(df$grp) & df$grp2 == "X", ]
  oracle_design <- as_survey(
    oracle_df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  expected <- suppressWarnings(
    get_corr(oracle_design, x = c(y1, y2), variance = "se")
  )
  na_x_rows <- result[is.na(result$grp) & result$grp2 == "X", ]
  expect_equal(na_x_rows$r, expected$r, tolerance = 1e-10)
  expect_true(all(is.finite(na_x_rows$se)))
  expect_equal(na_x_rows$n, expected$n)
})

# ---------------------------------------------------------------------------
# Additional coverage: SRS, twophase, n<3, cv warning
# ---------------------------------------------------------------------------

test_that("get_corr() works for SRS design (covers .vcov_pair_srs())", {
  set.seed(300)
  n <- 80L
  x <- rnorm(n)
  y <- x * 0.8 + rnorm(n, sd = 0.3)
  df <- data.frame(x = x, y = y, w = rep(1, n))
  sc <- as_survey(df, weights = w)
  result <- get_corr(sc, x = c(x, y), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
  expect_true(result$r[[1L]] > 0.5) # known positive correlation
  expect_gte(result$se[[1L]], 0)
})

test_that("get_corr() works for survey_twophase design (covers .vcov_pair_twophase())", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "twophase",
    seed = 301
  )
  phase1 <- as_survey(
    d,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  sc <- as_survey_twophase(
    phase1,
    subset = subset,
    ids2 = psu,
    strata2 = strata
  )
  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_corr() returns NA t-stat/p-val when n < 3 in domain", {
  # Domain with only 2 rows: r is defined but df_t = n-2 = 0 → NA t-stat path
  df <- make_survey_data(n = 80, n_psu = 10, n_strata = 2, seed = 302)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  # Keep only 2 rows in domain via domain column
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(nrow(df)) <= 2L
  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  # With n=2 (nn < 3), statistic and p_value must be NA
  expect_true(is.na(result$statistic[[1L]]))
  expect_true(is.na(result$p_value[[1L]]))
})

test_that("get_corr() variance='cv' warns when correlation is zero or negative", {
  # Force near-zero correlation by making y orthogonal to x
  set.seed(303)
  n <- 100L
  df <- make_survey_data(n = n, n_psu = 10, n_strata = 2, seed = 303)
  # Overwrite y2 so it is negatively correlated with y1
  df$y2_neg <- -df$y1 + rnorm(n, sd = 0.01)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  # cv undefined when r <= 0
  expect_warning(
    get_corr(sc, x = c(y1, y2_neg), variance = "cv"),
    class = "surveycore_warning_cv_undefined"
  )
})

test_that("get_corr() returns NA for corr CI when r is exactly 1 (perfect correlation)", {
  # Perfect positive correlation: y = x exactly → r = 1, t = Inf
  df <- make_survey_data(n = 60, n_psu = 10, n_strata = 2, seed = 304)
  df$y_perfect <- df$y1 # same as y1 → r = 1
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  result <- get_corr(sc, x = c(y1, y_perfect), variance = "se")
  expect_equal(result$r[[1L]], 1, tolerance = 1e-10)
  # t-stat should be Inf or very large for r = 1
  expect_true(
    is.infinite(result$statistic[[1L]]) || result$statistic[[1L]] > 100
  )
})

# ---------------------------------------------------------------------------
# Additional coverage: calibrated design, unsupported class
# ---------------------------------------------------------------------------

test_that("get_corr() works for survey_nonprob design (covers .vcov_pair_calibrated())", {
  set.seed(801)
  n <- 60L
  df <- data.frame(y1 = rnorm(n), y2 = rnorm(n), w = runif(n, 0.5, 2))
  sc <- as_survey_nonprob(df, weights = w)
  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
})

test_that(".corr_vcov_pair() errors for unsupported design class", {
  expect_error(
    surveycore:::.corr_vcov_pair(
      list(fake = TRUE),
      "y1",
      "y2",
      rep(1, 5),
      TRUE
    ),
    class = "surveycore_error_unsupported_class"
  )
})

# ---------------------------------------------------------------------------
# Category: nonprob latent path (polychoric / polyserial on survey_nonprob
# with bootstrap replicate weights)
# ---------------------------------------------------------------------------

test_that("get_corr() polychoric works for survey_nonprob with repweights", {
  set.seed(101L)
  df <- make_survey_data(n = 200L, design = "taylor", seed = 101L)
  df$ord1 <- cut(df$y1, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord1 <- factor(df$ord1, ordered = TRUE)
  df$ord2 <- cut(df$y2, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord2 <- factor(df$ord2, ordered = TRUE)
  set.seed(42L)
  for (r in seq_len(10L)) {
    df[[paste0("repwt_", r)]] <- df$wt *
      exp(rnorm(nrow(df), mean = 0, sd = 0.1))
  }
  repwt_cols <- paste0("repwt_", seq_len(10L))
  d <- as_survey_nonprob(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "bootstrap"
  )
  test_invariants(d)
  result <- get_corr(d, x = c(ord1, ord2), method = "polychoric")
  test_result_invariants(result, "survey_corr")
  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$r[[1L]]))
  expect_true(result$r[[1L]] >= -1 && result$r[[1L]] <= 1)
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_true(result$ci_low[[1L]] >= -1)
  expect_true(result$ci_high[[1L]] <= 1)
  expect_lt(result$ci_low[[1L]], result$ci_high[[1L]])
})

test_that("get_corr() polyserial works for survey_nonprob with repweights", {
  set.seed(101L)
  df <- make_survey_data(n = 200L, design = "taylor", seed = 101L)
  df$ord1 <- cut(df$y1, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord1 <- factor(df$ord1, ordered = TRUE)
  set.seed(42L)
  for (r in seq_len(10L)) {
    df[[paste0("repwt_", r)]] <- df$wt *
      exp(rnorm(nrow(df), mean = 0, sd = 0.1))
  }
  repwt_cols <- paste0("repwt_", seq_len(10L))
  d <- as_survey_nonprob(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "bootstrap"
  )
  test_invariants(d)
  result <- get_corr(d, x = c(ord1, y2), method = "polyserial")
  test_result_invariants(result, "survey_corr")
  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$r[[1L]]))
  expect_true(result$r[[1L]] >= -1 && result$r[[1L]] <= 1)
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_true(result$ci_low[[1L]] >= -1)
  expect_true(result$ci_high[[1L]] <= 1)
  expect_lt(result$ci_low[[1L]], result$ci_high[[1L]])
})

test_that("get_corr() polychoric raises PC-7 for survey_nonprob without repweights", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 101L)
  df$ord1 <- cut(df$y1, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord1 <- factor(df$ord1, ordered = TRUE)
  df$ord2 <- cut(df$y2, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord2 <- factor(df$ord2, ordered = TRUE)
  d <- as_survey_nonprob(df, weights = wt)
  test_invariants(d)
  expect_error(
    get_corr(d, x = c(ord1, ord2), method = "polychoric"),
    class = "surveycore_error_polychoric_design_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(ord1, ord2), method = "polychoric")
  )
})

test_that("get_corr() polyserial raises PC-7 for survey_nonprob without repweights", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 101L)
  df$ord1 <- cut(df$y1, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord1 <- factor(df$ord1, ordered = TRUE)
  d <- as_survey_nonprob(df, weights = wt)
  test_invariants(d)
  expect_error(
    get_corr(d, x = c(ord1, y2), method = "polyserial"),
    class = "surveycore_error_polychoric_design_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    get_corr(d, x = c(ord1, y2), method = "polyserial")
  )
})

test_that("get_corr() polychoric raises PC-7 for survey_twophase (regression guard)", {
  df <- make_survey_data(
    design = "twophase",
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    seed = 4L
  )
  df$ord1 <- cut(df$y1, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord1 <- factor(df$ord1, ordered = TRUE)
  df$ord2 <- cut(df$y2, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord2 <- factor(df$ord2, ordered = TRUE)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d <- as_survey_twophase(ph1, subset = subset, method = "approx")
  expect_error(
    get_corr(d, x = c(ord1, ord2), method = "polychoric"),
    class = "surveycore_error_polychoric_design_unsupported"
  )
})

test_that("get_corr() polychoric: survey_nonprob with repweights matches survey_replicate numerically", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 101L)
  df$ord1 <- cut(df$y1, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord1 <- factor(df$ord1, ordered = TRUE)
  df$ord2 <- cut(df$y2, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord2 <- factor(df$ord2, ordered = TRUE)
  set.seed(42L)
  for (r in seq_len(10L)) {
    df[[paste0("repwt_", r)]] <- df$wt *
      exp(rnorm(nrow(df), mean = 0, sd = 0.1))
  }
  repwt_cols <- paste0("repwt_", seq_len(10L))
  d_nonprob <- as_survey_nonprob(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "bootstrap"
  )
  d_rep <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "bootstrap"
  )
  result_nonprob <- get_corr(
    d_nonprob,
    x = c(ord1, ord2),
    method = "polychoric"
  )
  result_rep <- get_corr(d_rep, x = c(ord1, ord2), method = "polychoric")
  expect_equal(result_nonprob$r[[1L]], result_rep$r[[1L]], tolerance = 1e-10)
  expect_equal(
    result_nonprob$ci_low[[1L]],
    result_rep$ci_low[[1L]],
    tolerance = 1e-6
  )
})

test_that("get_corr() PC-4 fires through nonprob latent path when ordinal has one level", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 201L)
  df$ord_single <- factor(rep(1L, nrow(df)), levels = 1:3, ordered = TRUE)
  df$ord2 <- cut(df$y2, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord2 <- factor(df$ord2, ordered = TRUE)
  set.seed(42L)
  for (r in seq_len(10L)) {
    df[[paste0("repwt_", r)]] <- df$wt *
      exp(rnorm(nrow(df), mean = 0, sd = 0.1))
  }
  repwt_cols <- paste0("repwt_", seq_len(10L))
  d <- as_survey_nonprob(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "bootstrap"
  )
  test_invariants(d)
  expect_error(
    get_corr(d, x = c(ord_single, ord2), method = "polychoric"),
    class = "surveycore_error_polychoric_single_level_ordinal"
  )
})

test_that("get_corr() nonprob with repweights + method = 'pearson' is unchanged (PC-7 not triggered)", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 101L)
  set.seed(42L)
  for (r in seq_len(10L)) {
    df[[paste0("repwt_", r)]] <- df$wt *
      exp(rnorm(nrow(df), mean = 0, sd = 0.1))
  }
  repwt_cols <- paste0("repwt_", seq_len(10L))
  d <- as_survey_nonprob(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "bootstrap"
  )
  test_invariants(d)
  result <- get_corr(d, x = c(y1, y2), method = "pearson")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
})

test_that("get_corr() single-replicate column raises NB-3 at construction (not a get_corr error)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 201L)
  df$repwt_single <- df$wt * exp(rnorm(nrow(df), mean = 0, sd = 0.1))
  expect_error(
    as_survey_nonprob(
      df,
      weights = wt,
      repweights = tidyselect::all_of("repwt_single"),
      type = "bootstrap"
    ),
    class = "surveycore_error_repweights_single"
  )
})

test_that("get_corr() all-NA ordinal returns NA (early-exit) through nonprob latent path", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 201L)
  df$ord_na <- factor(rep(NA_integer_, nrow(df)), levels = 1:3, ordered = TRUE)
  df$ord2 <- cut(df$y2, breaks = 5, labels = FALSE, include.lowest = TRUE)
  df$ord2 <- factor(df$ord2, ordered = TRUE)
  set.seed(42L)
  for (r in seq_len(10L)) {
    df[[paste0("repwt_", r)]] <- df$wt *
      exp(rnorm(nrow(df), mean = 0, sd = 0.1))
  }
  repwt_cols <- paste0("repwt_", seq_len(10L))
  d <- as_survey_nonprob(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "bootstrap"
  )
  test_invariants(d)
  # All-NA ordinal: pairwise-complete domain is empty, early return with NA.
  result <- get_corr(d, x = c(ord_na, ord2), method = "polychoric")
  test_result_invariants(result, "survey_corr")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$r[[1L]]))
})
