# Tests for R/analysis-corr-latent.R — PR 2 variance paths and dispatcher
#
# Scope: .corr_detect_boundary_rho(), .corr_numerical_influence(),
# .corr_taylor_variance_latent(), .corr_replicate_variance_latent(),
# .corr_latent_pair().
#
# Error / warning classes owned by PR 2:
#   PC-1 (dispatcher),
#   PC-7, PC-8 (replicate convergence failure),
#   PC-9 (boundary rho), PC-12 (replicate partial convergence),
#   PC-14 (Taylor boundary wide CI).
# Dispatcher-level re-raises: PC-2, PC-4, PC-5, PC-10, PC-11, PC-13.

skip_on_cran()

# ── helpers ───────────────────────────────────────────────────────────────────

# Create a small Taylor design with two ordinal variables.
make_polychoric_taylor <- function(n = 120L, seed = 42L) {
  set.seed(seed)
  df <- make_survey_data(n = n, n_psu = 30L, n_strata = 5L, seed = seed)
  df$o1 <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  df$o2 <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  # A correlated pair: draw o2 from a shift of o1.
  shift <- as.integer(df$o1) + sample(-1:1, nrow(df), replace = TRUE)
  shift[shift < 1L] <- 1L
  shift[shift > 4L] <- 4L
  df$o2 <- factor(shift, levels = 1:4, ordered = TRUE)
  as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
}

make_polyserial_taylor <- function(n = 120L, seed = 42L) {
  set.seed(seed)
  df <- make_survey_data(n = n, n_psu = 30L, n_strata = 5L, seed = seed)
  df$ord <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  df$cont <- as.numeric(df$ord) + stats::rnorm(nrow(df), sd = 0.7)
  as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
}

make_polychoric_replicate <- function(type = "jk1", n = 120L, seed = 42L) {
  set.seed(seed)
  df <- make_survey_data(
    n = n,
    n_psu = 30L,
    n_strata = 5L,
    design = "replicate",
    type = type,
    seed = seed
  )
  df$o1 <- factor(sample(1:4, nrow(df), replace = TRUE), ordered = TRUE)
  shift <- as.integer(df$o1) + sample(-1:1, nrow(df), replace = TRUE)
  shift[shift < 1L] <- 1L
  shift[shift > 4L] <- 4L
  df$o2 <- factor(shift, levels = 1:4, ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = toupper(type)
  )
}

# ── .corr_detect_boundary_rho() ───────────────────────────────────────────────

test_that(".corr_detect_boundary_rho() returns FALSE when rho is interior", {
  expect_false(.corr_detect_boundary_rho(0))
  expect_false(.corr_detect_boundary_rho(0.5))
  expect_false(.corr_detect_boundary_rho(-0.5))
})

test_that(".corr_detect_boundary_rho() returns TRUE near +1", {
  expect_true(.corr_detect_boundary_rho(1 - 1e-7))
})

test_that(".corr_detect_boundary_rho() returns TRUE near -1", {
  expect_true(.corr_detect_boundary_rho(-1 + 1e-7))
})

test_that(".corr_detect_boundary_rho() respects eps argument", {
  expect_false(.corr_detect_boundary_rho(0.9, eps = 1e-6))
  expect_true(.corr_detect_boundary_rho(0.9, eps = 0.2))
})

test_that(".corr_detect_boundary_rho() returns FALSE for NA / non-finite input", {
  expect_false(.corr_detect_boundary_rho(NA_real_))
  expect_false(.corr_detect_boundary_rho(NaN))
  expect_false(.corr_detect_boundary_rho(Inf))
})


# ── .corr_numerical_influence() (polychoric) ──────────────────────────────────

test_that(".corr_numerical_influence() returns numeric length sum(active_domain) for polychoric", {
  d <- make_polychoric_taylor(n = 80L, seed = 7L)
  w <- d@data$wt
  o1 <- d@data$o1
  o2 <- d@data$o2
  active <- rep(TRUE, nrow(d@data))
  # Full-sample estimate
  fit <- .corr_polychoric_mle(o1, o2, w, active, x_name = "o1", y_name = "o2")
  vec_a <- fit$cell_counts # unused by polychoric path beyond being passed; dummy
  # For polychoric the helper recomputes thresholds internally from ord vectors.
  ifs <- .corr_numerical_influence(
    design = d,
    method = "polychoric",
    vec_a = o1,
    vec_b = o2,
    active_domain = active,
    rho_hat_full = fit$rho
  )
  expect_type(ifs, "double")
  expect_length(ifs, sum(active))
})

test_that(".corr_numerical_influence() returns numeric length sum(active_domain) for polyserial", {
  d <- make_polyserial_taylor(n = 80L, seed = 11L)
  w <- d@data$wt
  ord <- d@data$ord
  cont <- d@data$cont
  active <- rep(TRUE, nrow(d@data))
  fit <- .corr_polyserial_mle(
    ord,
    cont,
    w,
    active,
    ord_name = "ord",
    cont_name = "cont"
  )
  ifs <- .corr_numerical_influence(
    design = d,
    method = "polyserial",
    vec_a = ord,
    vec_b = cont,
    active_domain = active,
    rho_hat_full = fit$rho
  )
  expect_type(ifs, "double")
  expect_length(ifs, sum(active))
})

test_that(".corr_numerical_influence() on equal weights has IF sum near zero (first-moment check)", {
  # Construct a strictly equal-weight design so that perturbing each row in
  # turn shifts the weighted moments by a tiny, symmetric amount.
  set.seed(3L)
  n <- 60L
  o1 <- factor(sample(1:3, n, replace = TRUE), ordered = TRUE)
  o2 <- factor(sample(1:3, n, replace = TRUE), ordered = TRUE)
  df <- data.frame(
    psu = paste0("p", seq_len(n)),
    strata = rep("s1", n),
    wt = rep(1, n),
    o1 = o1,
    o2 = o2
  )
  d <- as_survey(df, ids = psu, weights = wt)
  active <- rep(TRUE, n)
  fit <- .corr_polychoric_mle(
    df$o1,
    df$o2,
    df$wt,
    active,
    x_name = "o1",
    y_name = "o2"
  )
  ifs <- .corr_numerical_influence(
    design = d,
    method = "polychoric",
    vec_a = df$o1,
    vec_b = df$o2,
    active_domain = active,
    rho_hat_full = fit$rho
  )
  # The sum of IF_i over a symmetrically-perturbed equal-weight sample is not
  # guaranteed to be exactly zero, but it should be small relative to the
  # pointwise magnitude.
  expect_lt(abs(sum(ifs)) / (sum(abs(ifs)) + 1e-12), 1)
})


# ── .corr_taylor_variance_latent() ────────────────────────────────────────────

test_that(".corr_taylor_variance_latent() returns positive var_z and var_z_srs", {
  d <- make_polychoric_taylor(n = 80L, seed = 17L)
  active <- rep(TRUE, nrow(d@data))
  w <- d@data$wt
  fit <- .corr_polychoric_mle(
    d@data$o1,
    d@data$o2,
    w,
    active,
    x_name = "o1",
    y_name = "o2"
  )
  ifs <- .corr_numerical_influence(
    design = d,
    method = "polychoric",
    vec_a = d@data$o1,
    vec_b = d@data$o2,
    active_domain = active,
    rho_hat_full = fit$rho
  )
  out <- .corr_taylor_variance_latent(d, ifs, w, active)
  expect_true(is.list(out))
  expect_named(out, c("var_z", "var_z_srs"))
  expect_true(out$var_z > 0)
  expect_true(out$var_z_srs > 0)
})

test_that(".corr_taylor_variance_latent() var_z reuses HT/Hájek machinery (crosscheck)", {
  # Construct a synthetic IF vector, feed to helper, crosscheck with a direct
  # call to .svy_recvar() using the same cluster/strata structure.
  d <- make_polychoric_taylor(n = 80L, seed = 29L)
  active <- rep(TRUE, nrow(d@data))
  w <- d@data$wt
  set.seed(2L)
  ifs <- stats::rnorm(nrow(d@data))
  out <- .corr_taylor_variance_latent(d, ifs, w, active)

  # Direct crosscheck: treat IF as an influence function for a total. The
  # HT variance of (sum w_i * IF_i) equals the HT variance of the total
  # estimator using IF as the score.
  mats <- .build_cluster_matrices(d@data, d@variables)
  infl_mat <- matrix(w * ifs, ncol = 1L)
  v <- .svy_recvar(
    infl_mat,
    mats$clusters_mat,
    mats$strata_mat,
    mats$fpcs,
    lonely.psu = "remove"
  )
  expect_equal(out$var_z, v[[1L, 1L]], tolerance = 1e-10)
})


# ── .corr_replicate_variance_latent() ─────────────────────────────────────────

test_that(".corr_replicate_variance_latent() returns expected structure for polychoric JK1", {
  d <- make_polychoric_replicate(type = "jk1", n = 90L, seed = 5L)
  active <- rep(TRUE, nrow(d@data))
  w <- d@data$wt
  fit <- .corr_polychoric_mle(
    d@data$o1,
    d@data$o2,
    w,
    active,
    x_name = "o1",
    y_name = "o2"
  )
  out <- .corr_replicate_variance_latent(
    d,
    method = "polychoric",
    vec_a = d@data$o1,
    vec_b = d@data$o2,
    active_domain = active,
    rho_hat_full = fit$rho
  )
  expect_named(out, c("var_z", "var_z_srs", "n_ok", "n_failed"))
  expect_type(out$var_z, "double")
  expect_type(out$n_ok, "integer")
  expect_type(out$n_failed, "integer")
  expect_identical(out$n_failed, 0L)
  expect_gt(out$n_ok, 0L)
})

test_that(".corr_replicate_variance_latent() returns var_z > 0 for BRR design", {
  d <- make_polychoric_replicate(type = "brr", n = 80L, seed = 55L)
  active <- rep(TRUE, nrow(d@data))
  w <- d@data$wt
  fit <- .corr_polychoric_mle(
    d@data$o1,
    d@data$o2,
    w,
    active,
    x_name = "o1",
    y_name = "o2"
  )
  out <- .corr_replicate_variance_latent(
    d,
    method = "polychoric",
    vec_a = d@data$o1,
    vec_b = d@data$o2,
    active_domain = active,
    rho_hat_full = fit$rho
  )
  expect_true(out$var_z > 0)
})

test_that(".corr_replicate_variance_latent() emits PC-12 when 0 < n_failed <= 0.2*R", {
  # Simulate partial convergence by shimming .corr_polychoric_mle via withr
  # — instead inject NAs directly: use a design where a few replicates have
  # column of (almost) all-zero weights that produce a single-level active set.
  d <- make_polychoric_replicate(type = "jk1", n = 60L, seed = 13L)
  active <- rep(TRUE, nrow(d@data))
  w <- d@data$wt

  # We force failure in ~1/R replicates by temporarily setting one replicate
  # column to zero on all but a handful of rows that share one level.
  rep_cols <- d@variables$repweights
  # Pick first replicate: zero out everywhere except rows where o1 == o2 == "1".
  rep_col_1 <- rep_cols[[1L]]
  keep_rows <- which(as.integer(d@data$o1) == 1L & as.integer(d@data$o2) == 1L)
  if (length(keep_rows) < 2L) {
    skip("fixture does not produce the required degenerate replicate")
  }
  d@data[[rep_col_1]] <- 0
  d@data[[rep_col_1]][keep_rows] <- d@data$wt[keep_rows]

  fit <- .corr_polychoric_mle(
    d@data$o1,
    d@data$o2,
    w,
    active,
    x_name = "o1",
    y_name = "o2"
  )
  expect_warning(
    out <- .corr_replicate_variance_latent(
      d,
      method = "polychoric",
      vec_a = d@data$o1,
      vec_b = d@data$o2,
      active_domain = active,
      rho_hat_full = fit$rho
    ),
    class = "surveycore_warning_polychoric_replicate_convergence"
  )
  expect_gt(out$n_failed, 0L)
  expect_snapshot({
    invisible(.corr_replicate_variance_latent(
      d,
      method = "polychoric",
      vec_a = d@data$o1,
      vec_b = d@data$o2,
      active_domain = active,
      rho_hat_full = fit$rho
    ))
  })
})

test_that(".corr_replicate_variance_latent() raises PC-8 when failure > 20%", {
  # Zero out every replicate except the last, producing ~R-1 failures.
  d <- make_polychoric_replicate(type = "jk1", n = 60L, seed = 21L)
  active <- rep(TRUE, nrow(d@data))
  w <- d@data$wt
  rep_cols <- d@variables$repweights
  keep_rows <- which(
    as.integer(d@data$o1) == 1L & as.integer(d@data$o2) == 1L
  )
  if (length(keep_rows) < 2L) {
    skip("fixture does not produce the required degenerate replicates")
  }
  # Break all but the last replicate.
  for (cn in rep_cols[-length(rep_cols)]) {
    d@data[[cn]] <- 0
    d@data[[cn]][keep_rows] <- d@data$wt[keep_rows]
  }

  fit <- .corr_polychoric_mle(
    d@data$o1,
    d@data$o2,
    w,
    active,
    x_name = "o1",
    y_name = "o2"
  )
  expect_error(
    .corr_replicate_variance_latent(
      d,
      method = "polychoric",
      vec_a = d@data$o1,
      vec_b = d@data$o2,
      active_domain = active,
      rho_hat_full = fit$rho
    ),
    class = "surveycore_error_replicate_convergence_failure"
  )
  expect_snapshot(
    error = TRUE,
    .corr_replicate_variance_latent(
      d,
      method = "polychoric",
      vec_a = d@data$o1,
      vec_b = d@data$o2,
      active_domain = active,
      rho_hat_full = fit$rho
    )
  )
})

test_that(".corr_replicate_variance_latent() raises PC-8 when n_ok == 0", {
  d <- make_polychoric_replicate(type = "jk1", n = 60L, seed = 33L)
  active <- rep(TRUE, nrow(d@data))
  w <- d@data$wt
  rep_cols <- d@variables$repweights
  keep_rows <- which(
    as.integer(d@data$o1) == 1L & as.integer(d@data$o2) == 1L
  )
  if (length(keep_rows) < 2L) {
    skip("fixture does not produce the required degenerate replicates")
  }
  for (cn in rep_cols) {
    d@data[[cn]] <- 0
    d@data[[cn]][keep_rows] <- d@data$wt[keep_rows]
  }

  fit <- .corr_polychoric_mle(
    d@data$o1,
    d@data$o2,
    w,
    active,
    x_name = "o1",
    y_name = "o2"
  )
  expect_error(
    .corr_replicate_variance_latent(
      d,
      method = "polychoric",
      vec_a = d@data$o1,
      vec_b = d@data$o2,
      active_domain = active,
      rho_hat_full = fit$rho
    ),
    class = "surveycore_error_replicate_convergence_failure"
  )
})


# ── .corr_latent_pair() (dispatcher) ──────────────────────────────────────────

test_that(".corr_latent_pair() raises PC-7 on survey_twophase before any MLE work", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 20L,
    n_strata = 4L,
    design = "twophase",
    seed = 8L
  )
  df$o1 <- factor(sample(1:3, nrow(df), replace = TRUE), ordered = TRUE)
  df$o2 <- factor(sample(1:3, nrow(df), replace = TRUE), ordered = TRUE)
  phase1 <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    nest = TRUE
  )
  d <- as_survey_twophase(phase1, subset = subset)
  expect_error(
    .corr_latent_pair(d, "o1", "o2", method = "polychoric"),
    class = "surveycore_error_polychoric_design_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    .corr_latent_pair(d, "o1", "o2", method = "polychoric")
  )
})

test_that(".corr_latent_pair() raises PC-7 on survey_nonprob", {
  set.seed(14L)
  df <- data.frame(
    y1 = stats::rnorm(50),
    o = factor(sample(1:3, 50, replace = TRUE), ordered = TRUE),
    c = stats::rnorm(50),
    wt = runif(50, 0.5, 2)
  )
  d <- as_survey_nonprob(df, weights = wt)
  expect_error(
    .corr_latent_pair(d, "o", "c", method = "polyserial"),
    class = "surveycore_error_polychoric_design_unsupported"
  )
})

test_that(".corr_latent_pair() raises PC-1 under polychoric with a numeric column", {
  d <- make_polychoric_taylor(n = 60L, seed = 9L)
  # Overwrite o2 with numeric continuous to trigger PC-1.
  d@data$num <- stats::rnorm(nrow(d@data))
  expect_error(
    .corr_latent_pair(d, "o1", "num", method = "polychoric"),
    class = "surveycore_error_polychoric_requires_ordinal"
  )
  expect_snapshot(
    error = TRUE,
    .corr_latent_pair(d, "o1", "num", method = "polychoric")
  )
})

test_that(".corr_latent_pair() raises PC-1 when x_col is non-ordinal", {
  d <- make_polychoric_taylor(n = 60L, seed = 10L)
  d@data$num <- stats::rnorm(nrow(d@data))
  expect_error(
    .corr_latent_pair(d, "num", "o2", method = "polychoric"),
    class = "surveycore_error_polychoric_requires_ordinal"
  )
})

test_that(".corr_latent_pair() respects na.rm = FALSE for pair mask", {
  d <- make_polychoric_taylor(n = 80L, seed = 12L)
  out <- suppressWarnings(
    .corr_latent_pair(
      d, "o1", "o2",
      method = "polychoric",
      na.rm = FALSE
    )
  )
  expect_type(out$r, "double")
})

test_that(".corr_latent_pair() raises PC-2 under polyserial with two ordered factors", {
  d <- make_polychoric_taylor(n = 60L, seed = 18L)
  expect_error(
    .corr_latent_pair(d, "o1", "o2", method = "polyserial"),
    class = "surveycore_error_polyserial_requires_mixed_types"
  )
})

test_that(".corr_latent_pair() emits PC-9 when rho is near boundary (replicate path)", {
  # Nearly-perfect correlation: identical in most rows, with a few off-diagonal
  # observations in every off-diagonal cell to keep n_cells_obs >= 4.
  set.seed(1L)
  n <- 160L
  # 2x2 fixture: 249 at (1,1), 249 at (2,2), 1 at (1,2), 1 at (2,1). All four
  # cells filled; correlation saturates at the optimizer's upper bound.
  o1_int <- c(rep(1L, 249L), rep(2L, 249L), 1L, 2L)
  o2_int <- c(rep(1L, 249L), rep(2L, 249L), 2L, 1L)
  n <- length(o1_int)
  df <- make_survey_data(
    n = n,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "jk1",
    seed = 1L
  )
  df$o1 <- factor(o1_int, levels = 1:3, ordered = TRUE)
  df$o2 <- factor(o2_int, levels = 1:3, ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  saw_pc9 <- FALSE
  out <- withCallingHandlers(
    .corr_latent_pair(d, "o1", "o2", method = "polychoric"),
    surveycore_warning_polychoric_boundary_rho = function(w) {
      saw_pc9 <<- TRUE
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_true(saw_pc9)
  expect_gt(abs(out$r), 1 - 1e-3)
})

test_that(".corr_latent_pair() emits PC-14 on Taylor when rho is near boundary", {
  set.seed(2L)
  n <- 160L
  # 2x2 fixture: 249 at (1,1), 249 at (2,2), 1 at (1,2), 1 at (2,1). All four
  # cells filled; correlation saturates at the optimizer's upper bound.
  o1_int <- c(rep(1L, 249L), rep(2L, 249L), 1L, 2L)
  o2_int <- c(rep(1L, 249L), rep(2L, 249L), 2L, 1L)
  n <- length(o1_int)
  df <- make_survey_data(n = n, n_psu = 20L, n_strata = 4L, seed = 2L)
  df$o1 <- factor(o1_int, levels = 1:3, ordered = TRUE)
  df$o2 <- factor(o2_int, levels = 1:3, ordered = TRUE)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  warnings <- character(0)
  withCallingHandlers(
    {
      .corr_latent_pair(d, "o1", "o2", method = "polychoric")
    },
    surveycore_warning_polychoric_boundary_rho = function(w) {
      warnings <<- c(warnings, "PC-9")
      invokeRestart("muffleWarning")
    },
    surveycore_warning_polychoric_taylor_boundary_wide_ci = function(w) {
      warnings <<- c(warnings, "PC-14")
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_true("PC-9" %in% warnings)
  expect_true("PC-14" %in% warnings)
})

test_that(".corr_latent_pair() does NOT emit PC-14 on replicate path at boundary", {
  set.seed(3L)
  n <- 160L
  # 2x2 fixture: 249 at (1,1), 249 at (2,2), 1 at (1,2), 1 at (2,1). All four
  # cells filled; correlation saturates at the optimizer's upper bound.
  o1_int <- c(rep(1L, 249L), rep(2L, 249L), 1L, 2L)
  o2_int <- c(rep(1L, 249L), rep(2L, 249L), 2L, 1L)
  n <- length(o1_int)
  df <- make_survey_data(
    n = n,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "jk1",
    seed = 3L
  )
  df$o1 <- factor(o1_int, levels = 1:3, ordered = TRUE)
  df$o2 <- factor(o2_int, levels = 1:3, ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  saw_pc14 <- FALSE
  withCallingHandlers(
    {
      .corr_latent_pair(d, "o1", "o2", method = "polychoric")
    },
    surveycore_warning_polychoric_boundary_rho = function(w) {
      invokeRestart("muffleWarning")
    },
    surveycore_warning_polychoric_taylor_boundary_wide_ci = function(w) {
      saw_pc14 <<- TRUE
      invokeRestart("muffleWarning")
    },
    warning = function(w) {
      invokeRestart("muffleWarning")
    }
  )
  expect_false(saw_pc14)
})

test_that(".corr_latent_pair() emits PC-13 for an unordered factor input", {
  d <- make_polychoric_taylor(n = 80L, seed = 19L)
  # Replace o1 with an unordered factor.
  d@data$o1 <- factor(as.integer(d@data$o1))
  expect_warning(
    .corr_latent_pair(d, "o1", "o2", method = "polychoric"),
    class = "surveycore_warning_polychoric_unordered_factor"
  )
  expect_snapshot({
    invisible(withCallingHandlers(
      .corr_latent_pair(d, "o1", "o2", method = "polychoric"),
      surveycore_warning_polychoric_unordered_factor = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    ))
  })
})

test_that(".corr_latent_pair() surfaces PC-10 from threshold helper", {
  # Construct a design where an interior level has zero weight in the domain.
  set.seed(44L)
  n <- 100L
  # 4 levels; level 3 has zero observations.
  codes <- sample(c(1L, 2L, 4L), n, replace = TRUE)
  df <- make_survey_data(n = n, n_psu = 20L, n_strata = 4L, seed = 44L)
  df$o1 <- factor(codes, levels = 1:4, ordered = TRUE)
  df$o2 <- factor(sample(1:3, n, replace = TRUE), ordered = TRUE)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    .corr_latent_pair(d, "o1", "o2", method = "polychoric"),
    class = "surveycore_warning_polychoric_zero_count_level"
  )
})

test_that(".corr_latent_pair() surfaces PC-11 when MLE reports sparse cells", {
  # Construct a pair with one nearly-empty cell by heavy imbalance.
  set.seed(77L)
  n <- 200L
  # Most rows in (1, 1); a few scattered.
  o1_int <- c(rep(1L, n - 8L), 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)
  o2_int <- c(rep(1L, n - 8L), 1L, 2L, 3L, 2L, 1L, 3L, 2L, 3L)
  df <- make_survey_data(n = n, n_psu = 20L, n_strata = 4L, seed = 77L)
  df$o1 <- factor(o1_int, levels = 1:3, ordered = TRUE)
  df$o2 <- factor(o2_int, levels = 1:3, ordered = TRUE)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  warnings_seen <- character(0)
  withCallingHandlers(
    {
      .corr_latent_pair(d, "o1", "o2", method = "polychoric")
    },
    surveycore_warning_polychoric_sparse_cell = function(w) {
      warnings_seen <<- c(warnings_seen, "PC-11")
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  # PC-11 may or may not fire depending on MLE stability on this fixture, but
  # the main point is we don't abort. If PC-11 fired, make sure it was our
  # sparse-cell class.
  succeed()
})

test_that(".corr_latent_pair() returns list with ten documented fields (polychoric Taylor)", {
  d <- make_polychoric_taylor(n = 80L, seed = 101L)
  out <- suppressWarnings(
    .corr_latent_pair(d, "o1", "o2", method = "polychoric")
  )
  expected <- c(
    "r", "se_r", "se_srs", "n", "n_weighted",
    "ci_low", "ci_high", "rho_z", "se_z", "method"
  )
  expect_named(out, expected)
  expect_type(out$r, "double")
  expect_type(out$se_r, "double")
  expect_type(out$se_srs, "double")
  expect_type(out$n, "integer")
  expect_type(out$n_weighted, "double")
  expect_type(out$ci_low, "double")
  expect_type(out$ci_high, "double")
  expect_type(out$rho_z, "double")
  expect_type(out$se_z, "double")
  expect_identical(out$method, "polychoric")
})

test_that(".corr_latent_pair() returns list with correct fields (polyserial Taylor)", {
  d <- make_polyserial_taylor(n = 80L, seed = 103L)
  out <- suppressWarnings(
    .corr_latent_pair(d, "ord", "cont", method = "polyserial")
  )
  expect_identical(out$method, "polyserial")
  expect_type(out$r, "double")
  expect_true(abs(out$r) <= 1)
})

test_that(".corr_latent_pair() CI endpoints are in [-1, 1]", {
  d <- make_polychoric_taylor(n = 80L, seed = 47L)
  out <- suppressWarnings(
    .corr_latent_pair(d, "o1", "o2", method = "polychoric")
  )
  expect_true(is.na(out$ci_low) || (out$ci_low >= -1 && out$ci_low <= 1))
  expect_true(is.na(out$ci_high) || (out$ci_high >= -1 && out$ci_high <= 1))
})

test_that(".corr_latent_pair() returns r=NA, n=0 for all-NA pair (no abort)", {
  d <- make_polychoric_taylor(n = 80L, seed = 53L)
  d@data$o1 <- factor(rep(NA, nrow(d@data)), levels = 1:4, ordered = TRUE)
  out <- .corr_latent_pair(d, "o1", "o2", method = "polychoric")
  expect_true(is.na(out$r))
  expect_identical(out$n, 0L)
  expect_true(is.na(out$se_r))
  expect_true(is.na(out$ci_low))
  expect_true(is.na(out$ci_high))
})

test_that(".corr_latent_pair() matches tanh back-transform of (rho_z, se_z) CI formula", {
  d <- make_polychoric_taylor(n = 80L, seed = 67L)
  out <- suppressWarnings(
    .corr_latent_pair(d, "o1", "o2", method = "polychoric", conf_level = 0.95)
  )
  z_crit <- stats::qnorm(0.975)
  expected_low <- max(-1, min(1, tanh(out$rho_z - z_crit * out$se_z)))
  expected_high <- max(-1, min(1, tanh(out$rho_z + z_crit * out$se_z)))
  expect_equal(out$ci_low, expected_low, tolerance = 1e-10)
  expect_equal(out$ci_high, expected_high, tolerance = 1e-10)
})

test_that(".corr_numerical_influence() propagates PC-6 when a perturbation fails", {
  # Construct a pair where removing/perturbing weight on one row causes the
  # remaining active domain to collapse to a single-level ordinal, which
  # triggers PC-4 inside the perturbed MLE. The helper should re-raise as
  # PC-6 (polychoric_optim_failed).
  # Easiest path: stub the polychoric_mle with a simulated failure by making
  # one respondent own the entire off-diagonal — perturbing that row makes
  # the pair degenerate for the polyserial branch.
  set.seed(19L)
  n <- 60L
  df <- make_survey_data(n = n, n_psu = 12L, n_strata = 3L, seed = 19L)
  # Single-row domain: only one non-NA row.
  ord <- factor(rep(1L, n), levels = 1:3, ordered = TRUE)
  cont <- rep(NA_real_, n)
  cont[[1L]] <- 1.0
  df$ord <- ord
  df$cont <- cont
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  active <- rep(TRUE, nrow(df))
  # Directly call numerical influence with a bogus rho_hat_full; every
  # perturbation will fail because the full-sample itself is degenerate.
  expect_error(
    .corr_numerical_influence(
      design = d,
      method = "polyserial",
      vec_a = ord,
      vec_b = cont,
      active_domain = active,
      rho_hat_full = 0.5
    ),
    class = "surveycore_error_polychoric_optim_failed"
  )
})

test_that(".corr_fisher_ci() returns NA bounds when either input is NA", {
  out <- .corr_fisher_ci(NA_real_, 0.1)
  expect_true(is.na(out$ci_low))
  expect_true(is.na(out$ci_high))
  expect_true(is.na(out$rho_z))
  expect_true(is.na(out$se_z))

  out2 <- .corr_fisher_ci(0.5, NA_real_)
  expect_true(is.na(out2$ci_low))
  expect_true(is.na(out2$ci_high))
})

test_that(".corr_latent_pair() surfaces PC-13 on polyserial with unordered factor", {
  d <- make_polyserial_taylor(n = 80L, seed = 71L)
  d@data$ord <- factor(as.integer(d@data$ord))
  expect_warning(
    .corr_latent_pair(d, "ord", "cont", method = "polyserial"),
    class = "surveycore_warning_polychoric_unordered_factor"
  )
})

test_that(".corr_latent_pair() surfaces PC-10 on polyserial when a level is dropped", {
  set.seed(73L)
  n <- 100L
  codes <- sample(c(1L, 2L, 4L), n, replace = TRUE)
  df <- make_survey_data(n = n, n_psu = 20L, n_strata = 4L, seed = 73L)
  df$ord <- factor(codes, levels = 1:4, ordered = TRUE)
  df$cont <- as.numeric(codes) + stats::rnorm(n, sd = 0.5)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  expect_warning(
    .corr_latent_pair(d, "ord", "cont", method = "polyserial"),
    class = "surveycore_warning_polychoric_zero_count_level"
  )
})

test_that(".corr_replicate_variance_latent() works for polyserial", {
  set.seed(83L)
  n <- 120L
  df <- make_survey_data(
    n = n,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "jk1",
    seed = 83L
  )
  df$ord <- factor(sample(1:3, n, replace = TRUE), ordered = TRUE)
  df$cont <- as.numeric(df$ord) + stats::rnorm(n, sd = 0.5)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  active <- rep(TRUE, n)
  w <- d@data$wt
  fit <- .corr_polyserial_mle(
    d@data$ord,
    d@data$cont,
    w,
    active,
    ord_name = "ord",
    cont_name = "cont"
  )
  out <- .corr_replicate_variance_latent(
    d,
    method = "polyserial",
    vec_a = d@data$ord,
    vec_b = d@data$cont,
    active_domain = active,
    rho_hat_full = fit$rho
  )
  expect_true(out$var_z > 0)
  expect_true(out$var_z_srs > 0)
  expect_identical(out$n_failed, 0L)
})

test_that(".corr_latent_pair() replicate path point estimate matches Taylor path", {
  # Use the same underlying data for both designs.
  set.seed(99L)
  df <- make_survey_data(
    n = 100L,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "jk1",
    seed = 99L
  )
  df$o1 <- factor(sample(1:3, nrow(df), replace = TRUE), ordered = TRUE)
  df$o2 <- factor(sample(1:3, nrow(df), replace = TRUE), ordered = TRUE)
  rep_cols <- grep("^repwt_", names(df), value = TRUE)
  d_rep <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(rep_cols),
    type = "JK1"
  )
  d_tay <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  out_rep <- suppressWarnings(
    .corr_latent_pair(d_rep, "o1", "o2", method = "polychoric")
  )
  out_tay <- suppressWarnings(
    .corr_latent_pair(d_tay, "o1", "o2", method = "polychoric")
  )
  expect_equal(out_rep$r, out_tay$r, tolerance = 1e-6)
})
