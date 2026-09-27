# R/analysis-ratios-helpers.R
#
# Internal helpers for get_ratios().
#
# Functions:
#   .replicate_ratio_cell() — per-replicate ratio variance
#   .delta_ratio_cell()     — delta-method ratio variance (all other classes)

# ── .replicate_ratio_cell() ───────────────────────────────────────────────────
#
# Compute ratio SE for replicate-weight designs by computing the ratio
# directly for each replicate weight set and using .svy_rep_var().
# Matches survey::svyratio() output exactly for BRR, JK, and bootstrap designs.
#
# @param design     A survey_replicate object.
# @param y_vec      Numeric vector (full length): numerator values.
# @param x_vec      Numeric vector (full length): denominator values.
# @param domain     Numeric 0/1 vector (full length): in-domain indicator.
# @param ratio_est  Numeric scalar: full-sample ratio estimate.
# @param total_x    Numeric scalar: full-sample denominator weighted total.
# @return Named list: se, se_srs, n_weighted.
#' @noRd
.replicate_ratio_cell <- function(
  design,
  y_vec,
  x_vec,
  domain,
  ratio_est,
  total_x
) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]

  n_d <- as.integer(sum(domain))
  N_d <- sum(w * domain)

  if (n_d == 0L || N_d <= 0) {
    return(list(se = NA_real_, se_srs = NA_real_, n_weighted = 0))
  }

  y_safe <- ifelse(domain > 0, y_vec, 0)
  x_safe <- ifelse(domain > 0, x_vec, 0)

  # Per-replicate ratios: ratio_r = total_y_r / total_x_r
  rep_mat <- as.matrix(data[, vars$repweights, drop = FALSE])
  rep_T_y <- as.numeric((y_safe * domain) %*% rep_mat)
  rep_T_x <- as.numeric((x_safe * domain) %*% rep_mat)
  rep_ratio <- ifelse(abs(rep_T_x) > 0, rep_T_y / rep_T_x, NA_real_)

  n_rep <- ncol(rep_mat)
  v <- .svy_rep_var(
    rep_ratio,
    scale = vars$scale,
    rscales = if (!is.null(vars$rscales)) vars$rscales else rep(1L, n_rep),
    mse = isTRUE(vars$mse),
    coef = ratio_est
  )

  se <- sqrt(max(0, v))

  # SRS-equivalent SE for deff: delta method with SRS (unweighted) variance
  z_dom <- (y_safe - ratio_est * x_safe)[domain > 0]
  se_srs <- if (n_d >= 2L && abs(total_x) > 0) {
    s2_z <- sum((z_dom - mean(z_dom))^2) / (n_d - 1L)
    N_d * sqrt(s2_z / n_d) / abs(total_x)
  } else {
    0
  }

  list(se = se, se_srs = se_srs, n_weighted = N_d)
}


# ── .delta_ratio_cell() ───────────────────────────────────────────────────────
#
# Compute ratio SE via the delta method (linearization) for Taylor,
# SRS, calibrated, and twophase designs. Injects the linearized variable
# z_i = y_i - ratio * x_i into design@data and delegates SE computation
# to .total_cell(), then divides by |total_x|.
#
# @param design     A survey design object (not survey_replicate).
# @param y_vec      Numeric vector (full length): numerator values.
# @param x_vec      Numeric vector (full length): denominator values.
# @param domain     Numeric 0/1 vector (full length): in-domain indicator.
# @param ratio_est  Numeric scalar: full-sample ratio estimate.
# @param total_x    Numeric scalar: full-sample denominator weighted total.
# @param tmp_col    Character(1): temp column name for the linearized variable.
# @return Named list: se, se_srs, n_weighted.
#' @noRd
.delta_ratio_cell <- function(
  design,
  y_vec,
  x_vec,
  domain,
  ratio_est,
  total_x,
  tmp_col
) {
  y_safe <- ifelse(domain > 0, y_vec, 0)
  x_safe <- ifelse(domain > 0, x_vec, 0)

  # Linearized variable: z_i = y_i - ratio * x_i  (0 outside domain)
  z_lin <- domain * (y_safe - ratio_est * x_safe)

  design@data[[tmp_col]] <- z_lin
  cell_z <- .total_cell(design, tmp_col, domain)

  se <- cell_z$se / abs(total_x)
  se_srs <- cell_z$se_srs / abs(total_x)

  list(se = se, se_srs = se_srs, n_weighted = cell_z$n_weighted)
}
