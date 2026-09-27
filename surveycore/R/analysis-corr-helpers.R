# R/analysis-corr-helpers.R
#
# Internal helpers for get_corr().
#
# Functions:
#   .corr_vcov_pair()              — dispatcher: vcov pair by design class
#   .corr_pair_result()            — compute r, SE, CI, t, p from vcov output
#   .corr_build_matrix_col_vecs()  — build r-matrix column vectors (wide helper)

# ── .corr_vcov_pair() ─────────────────────────────────────────────────────────
#
# Dispatch to the correct per-engine vcov pair estimator by design class.
#
# @param design  Any survey design object.
# @param x_col   Character: first variable name.
# @param y_col   Character: second variable name.
# @param domain  Numeric 0/1 vector (full length): domain membership mask.
# @param na.rm   Logical.
# @return Named list: a, b, c, sigma (3x3), n, n_weighted.
.corr_vcov_pair <- function(design, x_col, y_col, domain, na.rm) {
  if (S7::S7_inherits(design, survey_taylor)) {
    .vcov_pair_taylor(design, x_col, y_col, domain, na.rm)
  } else if (S7::S7_inherits(design, survey_replicate)) {
    .vcov_pair_replicate(design, x_col, y_col, domain, na.rm)
  } else if (S7::S7_inherits(design, survey_twophase)) {
    .vcov_pair_twophase(design, x_col, y_col, domain, na.rm)
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    if (!is.null(design@variables$repweights)) {
      .vcov_pair_replicate(design, x_col, y_col, domain, na.rm)
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
      .vcov_pair_calibrated(design, x_col, y_col, domain, na.rm)
    }
  } else {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Unsupported design class {.cls {class(design)[[1L]]}} ",
          "in {.fn get_corr}."
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


# ── .vcov_pair_calibrated() ───────────────────────────────────────────────────
#
# HT-style variance-covariance pair estimation for survey_nonprob designs.
# Uses SRS-equivalent HT influence functions: infl_j_i = w_i * g_j_i / W_d.
# Meta-vcov is computed as n/(n-1) * crossprod(infl_mat).
#
# @param design  A survey_nonprob object.
# @param x_col   Character: first variable name.
# @param y_col   Character: second variable name.
# @param domain  Numeric 0/1 vector (full length): domain membership mask.
# @param na.rm   Logical.
# @return Named list: a, b, c, sigma (3x3), n, n_weighted.
.vcov_pair_calibrated <- function(design, x_col, y_col, domain, na.rm = TRUE) {
  data <- design@data
  vars <- design@variables
  x_all <- data[[x_col]]
  y_all <- data[[y_col]]

  if (na.rm) {
    idx <- domain > 0 & !is.na(x_all) & !is.na(y_all)
  } else {
    idx <- domain > 0
  }

  n_d <- as.integer(sum(idx))

  if (n_d < 2L) {
    sigma <- matrix(NA_real_, 3L, 3L)
    return(list(
      a = NA_real_,
      b = NA_real_,
      c = NA_real_,
      sigma = sigma,
      n = n_d,
      n_weighted = 0
    ))
  }

  w_sub <- data[[vars$weights]][idx]
  x_sub <- x_all[idx]
  y_sub <- y_all[idx]
  W_d <- sum(w_sub)
  xbar <- sum(w_sub * x_sub) / W_d
  ybar <- sum(w_sub * y_sub) / W_d
  cx <- x_sub - xbar
  cy <- y_sub - ybar
  a <- sum(w_sub * cx^2) / W_d
  b <- sum(w_sub * cx * cy) / W_d
  c_val <- sum(w_sub * cy^2) / W_d

  # HT influence functions: infl_j_i = w_i * g_j_i / W_d
  infl_mat <- cbind(
    w_sub * (cx^2 - a) / W_d,
    w_sub * (cx * cy - b) / W_d,
    w_sub * (cy^2 - c_val) / W_d
  )
  # Meta-vcov: n / (n-1) * t(infl) %*% infl  (HT formula)
  sigma <- (n_d / (n_d - 1L)) * crossprod(infl_mat)

  list(a = a, b = b, c = c_val, sigma = sigma, n = n_d, n_weighted = W_d)
}


# ── .corr_pair_result() ───────────────────────────────────────────────────────
#
# From the vcov pair output, compute r (Pearson correlation), SE via delta
# method, Fisher Z CIs, t-statistic, and two-tailed p-value.
#
# Delta method for r = b / sqrt(a * c):
#   dr/da = -r / (2 * a)
#   dr/db =  1 / sqrt(a * c)
#   dr/dc = -r / (2 * c)
#   SE(r) = sqrt(g' * sigma * g)
#
# Fisher Z CI:
#   ci_low  = tanh(atanh(r) - z_crit * se_r)
#   ci_high = tanh(atanh(r) + z_crit * se_r)
#
# @param vcov_out Named list from .corr_vcov_pair().
# @return Named list: r, se_r, se_srs, n, n_weighted. NA when n < 2 or
#   variance-covariance is undefined.
.corr_pair_result <- function(vcov_out) {
  a <- vcov_out$a
  b <- vcov_out$b
  c_val <- vcov_out$c
  sigma <- vcov_out$sigma
  n_d <- vcov_out$n

  if (is.na(a) || is.na(b) || is.na(c_val) || a <= 0 || c_val <= 0) {
    return(list(
      r = NA_real_,
      se_r = NA_real_,
      se_srs = NA_real_,
      n = n_d,
      n_weighted = vcov_out$n_weighted
    ))
  }

  r <- b / sqrt(a * c_val)
  # Clamp to [-1, 1] to guard against floating-point overshoot
  r <- max(-1, min(1, r))

  # Delta method gradient: g = (dr/da, dr/db, dr/dc)
  g <- c(-r / (2 * a), 1 / sqrt(a * c_val), -r / (2 * c_val))
  v <- as.numeric(t(g) %*% sigma %*% g)
  se_r <- if (is.na(v) || v < 0) NA_real_ else sqrt(v)

  # SRS-equivalent SE: (1 - r^2) / sqrt(max(n-3, 1)) (Fisher Z approximation)
  se_srs <- if (n_d >= 4L) {
    (1 - r^2) / sqrt(n_d - 3L)
  } else {
    NA_real_
  }

  list(
    r = r,
    se_r = se_r,
    se_srs = se_srs,
    n = n_d,
    n_weighted = vcov_out$n_weighted
  )
}


# ── .corr_build_matrix_col_vecs() ────────────────────────────────────────────
#
# Build the r-matrix column vectors for one group combo (or the ungrouped case).
# Returns a named list: variable col + one named col per focal variable
# (r values).
# Does NOT call .make_result_tibble() — caller assembles across all combos
# and calls .make_result_tibble() once.
#
# Diagonal: NA when diagonal = FALSE, else 1.
# Upper triangle mirrors lower triangle (symmetric).
#
# @param x_names       Character vector of focal variable names.
# @param display_names Named character vector mapping x_names → display labels.
# @param pairs_i       Integer vector of first-variable indices
#                      (lower triangle).
# @param pairs_j       Integer vector of second-variable indices
#                      (lower triangle).
# @param pair_results  List of per-pair result lists from .corr_pair_result().
# @param diagonal      Logical: include diagonal (r = 1) in matrix.
# @return Named list: list(variable = ..., "ColName1" = ..., ...).
.corr_build_matrix_col_vecs <- function(
  x_names,
  display_names,
  pairs_i,
  pairs_j,
  pair_results,
  diagonal
) {
  p <- length(x_names)
  n_pairs <- length(pairs_i)

  r_mat <- matrix(NA_real_, p, p)
  for (k in seq_len(n_pairs)) {
    i <- pairs_i[[k]]
    j <- pairs_j[[k]]
    r_mat[i, j] <- pair_results[[k]]$r
    r_mat[j, i] <- pair_results[[k]]$r
  }
  if (isTRUE(diagonal)) {
    diag(r_mat) <- 1
  }

  var_col <- display_names[x_names]
  col_vecs <- list(variable = var_col)
  for (k in seq_len(p)) {
    col_vecs[[display_names[[k]]]] <- r_mat[, k]
  }
  col_vecs
}
