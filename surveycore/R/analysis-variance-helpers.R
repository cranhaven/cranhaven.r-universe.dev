# R/analysis-variance-helpers.R
#
# Internal helpers for get_variance().
#
# Functions:
#   .score_variance()         — build the score z_i = a_i * (y_i - ybar)^2 * n/(n-1)
#   .taylor_variance_cell()   — Taylor domain-estimation variance of variance
#   .replicate_variance_cell() — replicate-weight domain variance
#   .variance_cell()          — dispatcher

# ── .score_variance() ─────────────────────────────────────────────────────────
#
# Build the score vector z_i used by get_variance():
#     z_i = a_i * (y_i - ybar_d)^2 * n_d / (n_d - 1)
# where ybar_d and n_d are computed on the in-domain, non-NA-in-y, positive-
# weight subset.
#
# Out-of-domain / NA / non-positive-weight rows contribute z_i = 0. Returns
# the score vector along with the plug-in point estimate and sufficient
# statistics the dispatch branches need.
#
# @param y_col    Numeric vector of the focal variable (full length).
# @param w        Numeric vector of weights (full length).
# @param domain   Numeric 0/1 vector of "active" membership (full length).
# @return Named list:
#   score     — numeric(n_full): z_i with 0 on inactive rows.
#   v_hat     — plug-in variance estimate (scalar).
#   ybar      — plug-in domain mean of y (scalar, NaN if n_d = 0).
#   W         — sum(w * a) domain weight total (scalar).
#   n_d       — integer unweighted count of active rows.
#   active    — logical(n_full): TRUE for in-domain, non-NA-y, w > 0 rows.
#
# Notes:
#   - When n_d = 0L or W <= 0, returns v_hat = NaN, ybar = NaN, score all-zero.
#   - When n_d = 1L, n/(n-1) = Inf and (y - ybar)^2 = 0, so score contains NaN
#     (from 0 * Inf); v_hat = NaN. Caller is responsible for the
#     insufficient-n warning.
.score_variance <- function(y_col, w, domain) {
  n_full <- length(y_col)
  active <- (domain > 0) & !is.na(y_col) & (w > 0)
  n_d <- as.integer(sum(active))
  score <- numeric(n_full)

  if (n_d == 0L) {
    return(list(
      score = score,
      v_hat = NaN,
      ybar = NaN,
      W = 0,
      n_d = 0L,
      active = active
    ))
  }

  W <- sum(w[active])
  if (W <= 0) {
    return(list(
      score = score,
      v_hat = NaN,
      ybar = NaN,
      W = 0,
      n_d = n_d,
      active = active
    ))
  }

  ybar <- sum(w[active] * y_col[active]) / W

  if (n_d < 2L) {
    # n/(n-1) is undefined; point estimate is NaN, score is NaN for active
    # rows (caller emits surveycore_warning_variance_insufficient_n).
    score[active] <- NaN
    return(list(
      score = score,
      v_hat = NaN,
      ybar = ybar,
      W = W,
      n_d = n_d,
      active = active
    ))
  }

  kish <- n_d / (n_d - 1L)
  sq_dev <- (y_col[active] - ybar)^2
  # Snap squared deviations to exact 0 when they are numerically
  # indistinguishable from 0 (arises with constant variables where the
  # weighted mean has trailing floating-point noise).
  scale_sq <- (abs(ybar) + 1)^2
  sq_dev[sq_dev < scale_sq * .Machine$double.eps] <- 0
  score_active <- kish * sq_dev
  score[active] <- score_active

  v_hat <- sum(w[active] * score_active) / W

  list(
    score = score,
    v_hat = v_hat,
    ybar = ybar,
    W = W,
    n_d = n_d,
    active = active
  )
}


# ── .taylor_variance_cell() ───────────────────────────────────────────────────
#
# Taylor-linearization domain variance of variance. The point estimate is
# the weighted mean of the score z_i = a_i * (y_i - ybar)^2 * n/(n-1), and
# the variance of that estimate is the plugged-in weighted-mean influence
# function run through the stratified-clustered recursive variance engine.
#
# Returns the same cell payload as .taylor_mean_cell() but with keys renamed:
# variance (the point estimate), se, se_srs, n, n_weighted.
#
# @param design  A survey_taylor object.
# @param y_col   Character: column name.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: variance, se, se_srs, n, n_weighted.
.taylor_variance_cell <- function(design, y_col, domain) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]
  y_all <- data[[y_col]]

  sc <- .score_variance(y_all, w, domain)
  n_d <- sc$n_d

  if (n_d == 0L || !is.finite(sc$W) || sc$W <= 0) {
    return(list(
      variance = NaN,
      se = NaN,
      se_srs = NaN,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_d < 2L) {
    return(list(
      variance = NaN,
      se = NaN,
      se_srs = NaN,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  v_hat <- sc$v_hat
  W <- sc$W
  score <- sc$score
  active_num <- as.numeric(sc$active)

  # Influence function of the weighted mean of the score:
  #   u_i = w_i * a_i * (score_i - v_hat) / W   (out-of-domain rows = 0)
  u <- w * active_num * (score - v_hat) / W

  mats <- .build_cluster_matrices(data, vars)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  infl_mat <- matrix(u, ncol = 1L, dimnames = list(NULL, y_col))
  v <- .svy_recvar(
    infl_mat,
    mats$clusters_mat,
    mats$strata_mat,
    mats$fpcs,
    lonely.psu = lonely.psu
  )

  se <- sqrt(max(0, v[1L, 1L]))

  # SRS-equivalent SE for design effect: treat the score as an ordinary
  # variable under SRS sampling of size n_d.
  sc_active <- score[sc$active]
  score_mean <- mean(sc_active)
  se_srs <- if (n_d >= 2L) {
    s2 <- sum((sc_active - score_mean)^2) / (n_d - 1L)
    sqrt(s2 / n_d)
  } else {
    0
  }

  list(
    variance = v_hat,
    se = se,
    se_srs = se_srs,
    n = n_d,
    n_weighted = W
  )
}


# ── .replicate_variance_cell() ────────────────────────────────────────────────
#
# Replicate-weight domain variance of variance. Recomputes V^{(r)} per
# replicate with per-replicate ybar^{(r)} and runs .svy_rep_var() on the
# replicate estimates.
#
# @param design  A survey_replicate object.
# @param y_col   Character: variable name.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: variance, se, se_srs, n, n_weighted.
.replicate_variance_cell <- function(design, y_col, domain) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]
  y_all <- data[[y_col]]

  sc <- .score_variance(y_all, w, domain)
  n_d <- sc$n_d

  if (n_d == 0L || !is.finite(sc$W) || sc$W <= 0) {
    return(list(
      variance = NaN,
      se = NaN,
      se_srs = NaN,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_d < 2L) {
    return(list(
      variance = NaN,
      se = NaN,
      se_srs = NaN,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  v_hat <- sc$v_hat
  active <- sc$active
  active_num <- as.numeric(active)

  # Per-replicate recomputation: use replicate weights to recompute ybar^{(r)}
  # and V^{(r)} on the same active rows (n is fixed across replicates).
  rep_mat <- as.matrix(data[, vars$repweights, drop = FALSE])
  n_rep <- ncol(rep_mat)
  kish <- n_d / (n_d - 1L)

  # Restrict rows to active for efficiency; rep_mat rows correspond to data
  # rows, so subset by active directly.
  rep_active <- rep_mat[active, , drop = FALSE]
  y_active <- y_all[active]

  # Per-replicate W and ybar
  rep_W <- colSums(rep_active)
  rep_Y <- colSums(rep_active * y_active)
  rep_ybar <- ifelse(rep_W > 0, rep_Y / rep_W, NA_real_)

  # Per-replicate V: sum_i w_r[i] * (y_i - ybar_r)^2 * n/(n-1) / W_r.
  # Snap sq_dev to exact 0 when numerically indistinguishable from 0 to
  # avoid floating-point drift on constant variables.
  rep_v <- vapply(
    seq_len(n_rep),
    function(r) {
      Wr <- rep_W[[r]]
      if (!is.finite(Wr) || Wr <= 0) {
        return(NA_real_)
      }
      yb_r <- rep_ybar[[r]]
      sq_dev_r <- (y_active - yb_r)^2
      scale_sq_r <- (abs(yb_r) + 1)^2
      sq_dev_r[sq_dev_r < scale_sq_r * .Machine$double.eps] <- 0
      num <- sum(rep_active[, r] * sq_dev_r) * kish
      num / Wr
    },
    numeric(1L)
  )

  na_dropped <- sum(is.na(rep_v))
  na_frac <- na_dropped / n_rep
  if (isTRUE(
    .nonprob_rep_na_warn(design, na_frac, na_dropped, n_rep, vars$scale)
  )) {
    return(list(
      variance = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  v <- .svy_rep_var(
    rep_v,
    scale = vars$scale,
    rscales = if (!is.null(vars$rscales)) vars$rscales else rep(1L, n_rep),
    mse = isTRUE(vars$mse),
    coef = v_hat
  )

  se <- sqrt(max(0, v))

  # SRS-equivalent SE using the full-sample score vector
  sc_active <- sc$score[active]
  score_mean <- mean(sc_active)
  se_srs <- if (n_d >= 2L) {
    s2 <- sum((sc_active - score_mean)^2) / (n_d - 1L)
    sqrt(s2 / n_d)
  } else {
    0
  }

  list(
    variance = v_hat,
    se = se,
    se_srs = se_srs,
    n = n_d,
    n_weighted = sc$W
  )
}


# ── .twophase_variance_cell() ─────────────────────────────────────────────────
#
# Two-phase domain variance of variance. Builds the score z_i on the Phase 2
# rows (using the calibrated weights w_full / pi2_full) and runs the score's
# weighted-mean influence function through .twophasevar().
#
# Rows that are Phase 1 only (subset = FALSE) contribute zero influence: they
# are excluded from `n`, the weighted mean of the score, and the influence
# vector. This matches .twophase_mean_cell()'s treatment of the mean.
#
# @param design  A survey_twophase object.
# @param y_col   Character: variable name.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: variance, se, se_srs, n, n_weighted.
.twophase_variance_cell <- function(design, y_col, domain) {
  data <- design@data
  ph1_vars <- design@variables$phase1
  subset <- data[[design@variables$subset]] # logical, full length

  w_full <- data[[ph1_vars$weights]]
  pi2_full <- .compute_phase2_probs(design, subset)
  cal_wt_full <- w_full / pi2_full

  # Restrict to Phase 2 rows
  dom_ph2 <- domain[subset]
  cal_ph2 <- cal_wt_full[subset]
  y_ph2 <- data[[y_col]][subset]

  sc <- .score_variance(y_ph2, cal_ph2, dom_ph2)
  n_d <- sc$n_d

  if (n_d == 0L || !is.finite(sc$W) || sc$W <= 0) {
    return(list(
      variance = NaN,
      se = NaN,
      se_srs = NaN,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_d < 2L) {
    return(list(
      variance = NaN,
      se = NaN,
      se_srs = NaN,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  v_hat <- sc$v_hat
  W <- sc$W
  score_ph2 <- sc$score
  active_num_ph2 <- as.numeric(sc$active)

  # Influence of the weighted mean of the score on Phase 2 rows:
  #   u_i = cal_wt_i * active_i * (score_i - v_hat) / W
  # Embed in the full-length vector with 0 on Phase 1-only rows.
  infl_ph2 <- cal_ph2 * active_num_ph2 * (score_ph2 - v_hat) / W
  n_total <- nrow(data)
  influence <- numeric(n_total)
  ph2_idx <- which(subset)
  influence[ph2_idx] <- infl_ph2

  lonely.psu <- getOption("survey.lonely.psu", "remove")
  v_raw <- .twophasevar(influence, design, lonely.psu)
  v_scalar <- if (is.matrix(v_raw)) drop(v_raw)[1L] else as.numeric(v_raw)

  se <- sqrt(max(0, v_scalar))

  # SRS-equivalent SE for design effect: treat the score as an ordinary
  # variable under SRS sampling of size n_d.
  sc_active <- score_ph2[sc$active]
  score_mean <- mean(sc_active)
  se_srs <- if (n_d >= 2L) {
    s2 <- sum((sc_active - score_mean)^2) / (n_d - 1L)
    sqrt(s2 / n_d)
  } else {
    0
  }

  list(
    variance = v_hat,
    se = se,
    se_srs = se_srs,
    n = n_d,
    n_weighted = W
  )
}


# ── .nonprob_variance_cell() ──────────────────────────────────────────────────
#
# Non-probability (calibrated) domain variance of variance. Uses the HT
# Taylor linearization of the weighted mean of the score, matching
# survey::svydesign(ids=~1)'s treatment of svyvar():
#   z_i = a_i * (y_i - ybar_d)^2 * n/(n-1)
#   Var(V_hat) = n/(n-1) * sum(w_i^2 * (z_i - V_hat)^2) / W^2
# where W = sum(w_i * a_i) and the sum runs over active rows.
#
# Zero-weight rows are excluded from `n`, the weighted mean, and the variance
# calculation (via .score_variance()'s active-mask which requires w > 0).
#
# @param design  A survey_nonprob object.
# @param y_col   Character: variable name.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: variance, se, se_srs, n, n_weighted.
.nonprob_variance_cell <- function(design, y_col, domain) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]
  y_all <- data[[y_col]]

  sc <- .score_variance(y_all, w, domain)
  n_d <- sc$n_d

  if (n_d == 0L || !is.finite(sc$W) || sc$W <= 0) {
    return(list(
      variance = NaN,
      se = NaN,
      se_srs = NaN,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_d < 2L) {
    return(list(
      variance = NaN,
      se = NaN,
      se_srs = NaN,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  v_hat <- sc$v_hat
  W <- sc$W
  active <- sc$active
  w_sub <- w[active]
  score_sub <- sc$score[active]

  # HT Taylor linearization of the weighted mean:
  #   Var(V_hat) = n/(n-1) * sum(w_i^2 * (z_i - V_hat)^2) / W^2
  var_vhat <- (n_d / (n_d - 1L)) * sum(w_sub^2 * (score_sub - v_hat)^2) / W^2
  se <- sqrt(max(0, var_vhat))

  score_mean <- mean(score_sub)
  se_srs <- if (n_d >= 2L) {
    s2 <- sum((score_sub - score_mean)^2) / (n_d - 1L)
    sqrt(s2 / n_d)
  } else {
    0
  }

  list(
    variance = v_hat,
    se = se,
    se_srs = se_srs,
    n = n_d,
    n_weighted = W
  )
}


# ── .variance_cell() ──────────────────────────────────────────────────────────
#
# Dispatch to the correct per-cell variance estimator by design class.
# Supports survey_taylor, survey_replicate, survey_twophase, and
# survey_nonprob.
#
# @param design  Any survey design object.
# @param y_col   Character: variable name.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: variance, se, se_srs, n, n_weighted.
.variance_cell <- function(design, y_col, domain) {
  if (S7::S7_inherits(design, survey_taylor)) {
    .taylor_variance_cell(design, y_col, domain)
  } else if (S7::S7_inherits(design, survey_replicate)) {
    .replicate_variance_cell(design, y_col, domain)
  } else if (S7::S7_inherits(design, survey_twophase)) {
    .twophase_variance_cell(design, y_col, domain)
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    if (!is.null(design@variables$repweights)) {
      .replicate_variance_cell(design, y_col, domain)
    } else {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.cls survey_nonprob} object has no bootstrap replicate ",
            "weights. Standard errors use an SRS approximation that ",
            "underestimates calibration uncertainty."
          ),
          "i" = paste0(
            "Run {.fn surveywts::create_bootstrap_weights} on this ",
            "design for correct SEs."
          )
        ),
        class = "surveycore_warning_nonprob_srs_fallback"
      )
      .nonprob_variance_cell(design, y_col, domain)
    }
  } else {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Unsupported design class {.cls {class(design)[[1L]]}} ",
          "in {.fn get_variance}."
        )
      ),
      class = "surveycore_error_unsupported_class"
    )
  }
}
