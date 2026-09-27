# R/analysis-totals-helpers.R
#
# Internal helpers for get_totals().
#
# Functions:
#   .taylor_total_cell()     — Taylor domain-estimation total
#   .replicate_total_cell()  — replicate-weight domain total
#   .twophase_total_cell()   — two-phase domain total
#   .calibrated_total_cell() — calibrated domain total
#   .total_cell()            — dispatcher
#
# Note: .total_cell() is also used by get_ratios() (delta method)
# and .quantile_woodruff_cell() (mean cell reuse).

# ── .taylor_total_cell() ──────────────────────────────────────────────────────
#
# Domain estimation of a weighted total using Taylor linearization.
#
# @param design  A survey_taylor object (also used for the SRS path of
#                survey_nonprob without repweights).
# @param y_col   Character: variable name, OR NULL for population size.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: total, se, df (Taylor only), se_srs, n, n_weighted.
.taylor_total_cell <- function(design, y_col, domain) {
  data <- design@data
  vars <- design@variables
  n_full <- nrow(data)
  w <- data[[vars$weights]]

  if (is.null(y_col)) {
    # Population size mode: y_i = 1 for all in-domain rows
    y_safe <- domain
    y_all <- rep(1, n_full)
  } else {
    y_all <- data[[y_col]]
    y_safe <- ifelse(domain > 0, y_all, 0)
  }

  N_d <- sum(w * domain)
  n_d <- as.integer(sum(domain))

  if (n_d == 0L || N_d <= 0) {
    return(list(
      total = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = if (is.null(y_col)) NULL else 0L,
      n_weighted = 0
    ))
  }

  T_hat <- sum(w * domain * y_safe)

  # Influence: u_i = domain_i * y_i  (total linearization)
  u <- domain * y_safe

  # Build cluster / strata / FPC matrices (full dataset, multi-stage aware)
  mats <- .build_cluster_matrices(data, vars)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  lbl <- if (is.null(y_col)) "pop_total" else y_col
  infl_mat <- matrix(w * u, ncol = 1L, dimnames = list(NULL, lbl))
  infl_mat <- .maybe_apply_calibration(infl_mat, design)
  cal_df_reduction <- .get_calibration_df_reduction(design)

  v <- .svy_recvar(
    infl_mat,
    mats$clusters_mat,
    mats$strata_mat,
    mats$fpcs,
    lonely.psu = lonely.psu
  )

  se <- sqrt(max(0, v[1L, 1L]))

  # Calibration-adjusted degrees of freedom
  df_design <- max(1L, .degf_taylor(data, vars))
  df_final <- df_design - cal_df_reduction
  if (df_final <= 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "Calibration reduces design df ({df_design}) to {df_final}."
        ),
        "i" = "CIs and p-values may be invalid.",
        "v" = paste0(
          "Reduce the number of calibration columns or use a larger design."
        )
      ),
      class = "surveycore_warning_zero_df_after_calibration"
    )
    df_final <- max(1L, df_final)
  }

  # SRS-equivalent SE
  y_domain <- y_safe[domain > 0]
  se_srs <- if (n_d >= 2L) {
    N_hat <- N_d
    ybar <- T_hat / N_hat
    s2 <- sum((y_domain - ybar)^2) / (n_d - 1L)
    N_hat * sqrt(s2 / n_d)
  } else {
    0
  }

  list(
    total = T_hat,
    se = se,
    se_srs = se_srs,
    n = if (is.null(y_col)) NULL else n_d,
    n_weighted = N_d,
    df = df_final
  )
}


# ── .replicate_total_cell() ───────────────────────────────────────────────────
#
# Domain estimation of a weighted total using replicate weights.
#
# @param design  A survey_replicate or survey_nonprob object (with repweights).
# @param y_col   Character: variable name, OR NULL for population size.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: total, se, se_srs, n, n_weighted.
.replicate_total_cell <- function(design, y_col, domain) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]

  if (is.null(y_col)) {
    y_safe <- domain
    y_all <- rep(1, nrow(data))
  } else {
    y_all <- data[[y_col]]
    y_safe <- ifelse(domain > 0, y_all, 0)
  }

  N_d <- sum(w * domain)
  n_d <- as.integer(sum(domain))

  if (n_d == 0L || N_d <= 0) {
    return(list(
      total = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = if (is.null(y_col)) NULL else 0L,
      n_weighted = 0
    ))
  }

  T_hat <- sum(w * domain * y_safe)

  # Per-replicate domain totals
  rep_mat <- as.matrix(data[, vars$repweights, drop = FALSE])
  rep_Y <- as.numeric((y_safe * domain) %*% rep_mat)

  R <- ncol(rep_mat)
  na_dropped <- sum(is.na(rep_Y))
  na_frac <- na_dropped / R
  if (
    isTRUE(
      .nonprob_rep_na_warn(design, na_frac, na_dropped, R, vars$scale)
    )
  ) {
    return(list(
      total = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = n_d,
      n_weighted = N_d
    ))
  }

  v <- .svy_rep_var(
    rep_Y,
    scale = vars$scale,
    rscales = if (!is.null(vars$rscales)) vars$rscales else rep(1L, R),
    mse = isTRUE(vars$mse),
    coef = T_hat
  )

  se <- sqrt(max(0, v))

  # SRS-equivalent SE for deff
  y_domain <- y_safe[domain > 0]
  se_srs <- if (n_d >= 2L) {
    N_hat <- N_d
    ybar <- T_hat / N_hat
    s2 <- sum((y_domain - ybar)^2) / (n_d - 1L)
    N_hat * sqrt(s2 / n_d)
  } else {
    0
  }

  list(
    total = T_hat,
    se = se,
    se_srs = se_srs,
    n = if (is.null(y_col)) NULL else n_d,
    n_weighted = N_d
  )
}


# ── .twophase_total_cell() ────────────────────────────────────────────────────
#
# Domain estimation of a weighted total for two-phase designs.
#
# @param design  A survey_twophase object.
# @param y_col   Character: variable name, OR NULL for population size.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: total, se, se_srs, n, n_weighted.
.twophase_total_cell <- function(design, y_col, domain) {
  data <- design@data
  ph1_vars <- design@variables$phase1
  subset <- data[[design@variables$subset]]

  w_full <- data[[ph1_vars$weights]]
  pi2_full <- .compute_phase2_probs(design, subset)
  cal_wt <- w_full / pi2_full

  dom_ph2 <- domain[subset]
  cal_ph2 <- cal_wt[subset]

  if (is.null(y_col)) {
    y_ph2 <- rep(1, sum(subset))
  } else {
    y_ph2 <- data[[y_col]][subset]
  }

  n_d <- as.integer(sum(dom_ph2))
  N_d <- sum(cal_ph2 * dom_ph2)

  if (n_d == 0L || N_d <= 0) {
    return(list(
      total = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = if (is.null(y_col)) NULL else 0L,
      n_weighted = 0
    ))
  }

  y_safe_ph2 <- ifelse(dom_ph2 > 0, y_ph2, 0)
  T_hat <- sum(cal_ph2 * dom_ph2 * y_safe_ph2)

  # Full-length influence vector
  n_total <- nrow(data)
  influence <- numeric(n_total)
  ph2_idx <- which(subset)
  influence[ph2_idx] <- cal_ph2 * dom_ph2 * y_safe_ph2

  lonely.psu <- getOption("survey.lonely.psu", "remove")
  v_raw <- .twophasevar(influence, design, lonely.psu)
  v_scalar <- if (is.matrix(v_raw)) drop(v_raw)[1L] else as.numeric(v_raw)

  se <- sqrt(max(0, v_scalar))

  y_domain <- y_safe_ph2[dom_ph2 > 0]
  se_srs <- if (n_d >= 2L) {
    ybar <- T_hat / N_d
    s2 <- sum((y_domain - ybar)^2) / (n_d - 1L)
    N_d * sqrt(s2 / n_d)
  } else {
    0
  }

  list(
    total = T_hat,
    se = se,
    se_srs = se_srs,
    n = if (is.null(y_col)) NULL else n_d,
    n_weighted = N_d
  )
}


# ── .calibrated_total_cell() ──────────────────────────────────────────────────
#
# Domain estimation of a weighted total for calibrated designs.
# Uses the HT variance of the total (matching survey::svydesign(ids=~1)):
#   z_i = w_i * y_i   (contribution to the total)
#   Var(T_hat) = n/(n-1) * sum((z_i - T_hat/n)^2)
#
#
# @param design  A survey_nonprob object.
# @param y_col   Character: variable name, OR NULL for population size.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: total, se, se_srs, n, n_weighted.
.calibrated_total_cell <- function(design, y_col, domain) {
  data <- design@data
  vars <- design@variables

  idx <- domain > 0
  n_d <- as.integer(sum(idx))

  if (n_d == 0L) {
    return(list(
      total = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = if (is.null(y_col)) NULL else 0L,
      n_weighted = 0
    ))
  }

  w_sub <- data[[vars$weights]][idx]

  if (is.null(y_col)) {
    y_sub <- rep(1, n_d)
    T_hat <- sum(w_sub)
  } else {
    y_sub <- data[[y_col]][idx]
    T_hat <- sum(w_sub * y_sub)
  }

  N_d <- sum(w_sub)

  if (n_d == 1L) {
    return(list(
      total = T_hat,
      se = NA_real_,
      se_srs = NA_real_,
      n = if (is.null(y_col)) NULL else 1L,
      n_weighted = N_d
    ))
  }

  # HT variance of total: n/(n-1) * sum((z_i - T_hat/n)^2), z_i = w_i * y_i
  z_sub <- w_sub * y_sub
  T_mean <- T_hat / n_d
  var_T <- (n_d / (n_d - 1L)) * sum((z_sub - T_mean)^2)
  se <- sqrt(max(0, var_T))

  # SRS-equivalent SE for deff (uses unweighted sample mean)
  se_srs <- if (n_d >= 2L) {
    s2 <- sum((y_sub - mean(y_sub))^2) / (n_d - 1L)
    N_d * sqrt(s2 / n_d)
  } else {
    0
  }

  list(
    total = T_hat,
    se = se,
    se_srs = se_srs,
    n = if (is.null(y_col)) NULL else n_d,
    n_weighted = N_d
  )
}


# ── .total_cell() ─────────────────────────────────────────────────────────────
#
# Dispatch to the correct per-cell total estimator by design class.
#
# @param design  Any survey design object.
# @param y_col   Character: variable name, OR NULL for population size.
# @param domain  Numeric 0/1 vector (full length).
# @return Named list: total, se, df (Taylor path only), se_srs, n, n_weighted.
.total_cell <- function(design, y_col, domain) {
  if (S7::S7_inherits(design, survey_taylor)) {
    .taylor_total_cell(design, y_col, domain)
  } else if (S7::S7_inherits(design, survey_replicate)) {
    .replicate_total_cell(design, y_col, domain)
  } else if (S7::S7_inherits(design, survey_twophase)) {
    .twophase_total_cell(design, y_col, domain)
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    if (!is.null(design@variables$repweights)) {
      .replicate_total_cell(design, y_col, domain)
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
      .calibrated_total_cell(design, y_col, domain)
    }
  } else {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Unsupported design class {.cls {class(design)[[1L]]}} ",
          "in {.fn get_totals}."
        ),
        "i" = paste0(
          "Use {.fn as_survey}, {.fn as_survey_replicate}, ",
          "or {.fn as_survey_twophase}."
        )
      ),
      class = "surveycore_error_unsupported_class"
    )
  }
}
