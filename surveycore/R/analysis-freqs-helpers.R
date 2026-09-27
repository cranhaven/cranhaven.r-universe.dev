# R/analysis-freqs-helpers.R
#
# Internal helpers for get_freqs().
#
# Functions:
#   .get_levels()            — ordered unique levels for a variable
#   .taylor_freq_cell()      — ratio linearization (survey_taylor)
#   .replicate_freq_cell()   — replicate-weight proportion variance
#   .twophase_freq_cell()    — two-phase proportion via .twophasevar()
#   .calibrated_freq_cell()  — HT proportion variance (survey_nonprob)
#   .freq_cell()             — dispatcher

# ── .get_levels() ─────────────────────────────────────────────────────────────
#
# Return unique levels for a variable column, preserving factor level order
# (or ascending sort for non-factors). When na.rm = FALSE, NA is appended last
# regardless of factor ordering. When na.rm = TRUE, only non-NA values are
# returned, and factor levels absent from the data are dropped.
#
# @param x_col  Atomic vector (factor or plain vector).
# @param na.rm  Logical. If FALSE, NA is appended as a level.
# @return Atomic vector of levels (same type as x_col, except NA is always
#   NA_character_ for factors since factor levels are character).
.get_levels <- function(x_col, na.rm) {
  has_na <- anyNA(x_col)

  if (is.factor(x_col)) {
    # Factor: use declared level order; drop levels not present in data
    all_lvls <- levels(x_col)
    present <- unique(as.character(x_col[!is.na(x_col)]))
    kept_lvls <- all_lvls[all_lvls %in% present]
    if (!na.rm && has_na) {
      c(kept_lvls, NA_character_)
    } else {
      kept_lvls
    }
  } else {
    # Non-factor: sort unique non-NA values ascending
    non_na <- sort(unique(x_col[!is.na(x_col)]))
    if (!na.rm && has_na) {
      c(non_na, NA)
    } else {
      non_na
    }
  }
}


# ── .taylor_freq_cell() ───────────────────────────────────────────────────────
#
# Compute a weighted proportion and its Taylor-linearized standard error for
# one cell using the ratio (domain estimation) approach.
#
# @param design  A survey_taylor object.
# @param num     Numeric vector (0/1): rows in the cell
#                (level AND domain/group).
# @param denom   Numeric vector (0/1): rows in the group/domain (denominator).
#
# @return Named list: pct, se, se_srs, n (unweighted cell count), n_weighted.
.taylor_freq_cell <- function(design, num, denom) {
  data <- design@data
  vars <- design@variables
  n <- nrow(data)
  w <- data[[vars$weights]]

  N_d <- sum(w * denom)
  n_g <- as.integer(sum(denom)) # unweighted group/domain count

  if (n_g == 0L || N_d <= 0) {
    return(list(
      pct = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = 0L,
      n_weighted = 0
    ))
  }

  Y <- sum(w * num)
  p <- Y / N_d
  n_cell <- as.integer(sum(num))

  # Linearized influence: u_i = num_i - p * denom_i
  u <- num - p * denom

  # Build cluster / strata / FPC matrices (full dataset, multi-stage aware)
  mats <- .build_cluster_matrices(data, vars)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  infl_mat <- matrix(w * u / N_d, ncol = 1L, dimnames = list(NULL, "prop"))
  v <- .svy_recvar(
    infl_mat,
    mats$clusters_mat,
    mats$strata_mat,
    mats$fpcs,
    lonely.psu = lonely.psu
  )

  se <- sqrt(max(0, v[1L, 1L]))
  se_srs <- if (n_g >= 2L && p > 0 && p < 1) {
    sqrt(p * (1 - p) / n_g)
  } else {
    0
  }

  list(
    pct = p,
    se = se,
    se_srs = se_srs,
    n = n_cell,
    n_weighted = Y
  )
}


# ── .replicate_freq_cell() ────────────────────────────────────────────────────
#
# Compute a weighted proportion and its replicate-weight variance for one cell.
#
# @param design  A survey_replicate object.
# @param num     Numeric vector (0/1): cell membership (level AND domain/group).
# @param denom   Numeric vector (0/1): group/domain membership.
#
# @return Named list: pct, se, se_srs, n, n_weighted.
.replicate_freq_cell <- function(design, num, denom) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]] # full-sample base weights

  N_d <- sum(w * denom)
  n_g <- as.integer(sum(denom))

  if (n_g == 0L || N_d <= 0) {
    return(list(
      pct = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = 0L,
      n_weighted = 0
    ))
  }

  Y <- sum(w * num)
  p <- Y / N_d
  n_cell <- as.integer(sum(num))

  # Per-replicate proportions using BLAS matrix operations
  rep_mat <- as.matrix(data[, vars$repweights, drop = FALSE])
  rep_N_d <- as.numeric(denom %*% rep_mat) # weighted domain size per replicate
  rep_Y <- as.numeric(num %*% rep_mat) # weighted cell count per replicate
  rep_p <- ifelse(rep_N_d > 0, rep_Y / rep_N_d, NA_real_)

  R <- ncol(rep_mat)
  na_dropped <- sum(is.na(rep_p))
  na_frac <- na_dropped / R
  if (
    isTRUE(
      .nonprob_rep_na_warn(design, na_frac, na_dropped, R, vars$scale)
    )
  ) {
    return(list(
      pct = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = n_cell,
      n_weighted = Y
    ))
  }

  v <- .svy_rep_var(
    rep_p,
    scale = vars$scale,
    rscales = if (!is.null(vars$rscales)) vars$rscales else rep(1L, R),
    mse = isTRUE(vars$mse),
    coef = p
  )

  se <- sqrt(max(0, v))
  se_srs <- if (n_g >= 2L && p > 0 && p < 1) {
    sqrt(p * (1 - p) / n_g)
  } else {
    0
  }

  list(
    pct = p,
    se = se,
    se_srs = se_srs,
    n = n_cell,
    n_weighted = Y
  )
}


# ── .twophase_freq_cell() ─────────────────────────────────────────────────────
#
# Compute a weighted proportion and its two-phase linearized standard error
# for one cell. Uses calibrated weights and the full .twophasevar() engine.
#
# @param design  A survey_twophase object.
# @param num     Numeric vector (0/1): cell membership (full length, Phase 2
#   rows only have non-zero values).
# @param denom   Numeric vector (0/1): group/domain membership (full length).
#
# @return Named list: pct, se, se_srs, n, n_weighted.
.twophase_freq_cell <- function(design, num, denom) {
  data <- design@data
  ph1_vars <- design@variables$phase1
  subset <- data[[design@variables$subset]] # logical, full length

  w_full <- data[[ph1_vars$weights]]
  pi2_full <- .compute_phase2_probs(design, subset)
  cal_wt <- w_full / pi2_full

  # Restrict to Phase 2 rows
  num_ph2 <- num[subset]
  denom_ph2 <- denom[subset]
  cal_ph2 <- cal_wt[subset]

  n_g <- as.integer(sum(denom_ph2))
  N_d <- sum(cal_ph2 * denom_ph2)

  if (n_g == 0L || N_d <= 0) {
    return(list(
      pct = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = 0L,
      n_weighted = 0
    ))
  }

  Y <- sum(cal_ph2 * num_ph2)
  p <- Y / N_d
  n_cell <- as.integer(sum(num_ph2))

  # Full-length influence vector (0 for Phase 1-only rows)
  n_total <- nrow(data)
  influence <- numeric(n_total)
  ph2_idx <- which(subset)
  influence[ph2_idx] <- cal_ph2 * (num_ph2 - p * denom_ph2) / N_d

  lonely.psu <- getOption("survey.lonely.psu", "remove")
  v_raw <- .twophasevar(influence, design, lonely.psu)
  v_scalar <- if (is.matrix(v_raw)) drop(v_raw)[1L, 1L] else as.numeric(v_raw)

  se <- sqrt(max(0, v_scalar))
  se_srs <- if (n_g >= 2L && p > 0 && p < 1) {
    sqrt(p * (1 - p) / n_g)
  } else {
    0
  }

  list(
    pct = p,
    se = se,
    se_srs = se_srs,
    n = n_cell,
    n_weighted = Y
  )
}


# ── .calibrated_freq_cell() ──────────────────────────────────────────────────
#
# Compute a weighted proportion and its HT standard error for one cell in a
# calibrated (non-probability) design. Uses the HT variance directly on the
# 0/1 indicator. This keeps survey_nonprob dispatch consistent across all six
# analysis functions (all use the HT variance path rather than Taylor
# linearization).
#
# @param design  A survey_nonprob object.
# @param num     Numeric vector (0/1): cell membership (level AND domain/group).
# @param denom   Numeric vector (0/1): group/domain membership (denominator).
#
# @return Named list: pct, se, se_srs, n (unweighted cell count), n_weighted.
.calibrated_freq_cell <- function(design, num, denom) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]

  n_g <- as.integer(sum(denom))
  N_d <- sum(w * denom)
  n_cell <- as.integer(sum(num))
  Y <- sum(w * num)
  p <- if (N_d > 0) Y / N_d else NA_real_

  if (n_g == 0L || N_d <= 0) {
    return(list(
      pct = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_g < 2L) {
    return(list(
      pct = p,
      se = NA_real_,
      se_srs = NA_real_,
      n = n_cell,
      n_weighted = Y
    ))
  }

  # HT variance: n/(n-1) * sum(z_i^2), z_i = w_i*(I_i - p)/N_d
  # Only rows in the denominator domain contribute
  idx <- denom > 0
  w_sub <- w[idx]
  I_sub <- (num / denom)[idx] # 0/1 indicator within domain
  z <- w_sub * (I_sub - p) / N_d
  var_p <- (n_g / (n_g - 1L)) * sum(z^2)
  se <- sqrt(max(0, var_p))

  se_srs <- if (p > 0 && p < 1) sqrt(p * (1 - p) / n_g) else 0

  list(
    pct = p,
    se = se,
    se_srs = se_srs,
    n = n_cell,
    n_weighted = Y
  )
}


# ── .freq_cell() ──────────────────────────────────────────────────────────────
#
# Dispatch to the correct per-cell proportion estimator by design class.
#
# @param design  Any survey design object.
# @param num     Numeric 0/1 vector: cell membership (full length).
# @param denom   Numeric 0/1 vector: group/domain membership (full length).
#
# @return Named list: pct, se, se_srs, n, n_weighted.
.freq_cell <- function(design, num, denom) {
  if (S7::S7_inherits(design, survey_taylor)) {
    .taylor_freq_cell(design, num, denom)
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    if (!is.null(design@variables$repweights)) {
      .replicate_freq_cell(design, num, denom)
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
      .calibrated_freq_cell(design, num, denom)
    }
  } else if (S7::S7_inherits(design, survey_replicate)) {
    .replicate_freq_cell(design, num, denom)
  } else if (S7::S7_inherits(design, survey_twophase)) {
    .twophase_freq_cell(design, num, denom)
  } else {
    cli::cli_abort(
      c(
        "x" = "Unsupported design class {.cls {class(design)[[1L]]}} in",
        "i" = "{.fn get_freqs} does not support this design type."
      ),
      class = "surveycore_error_unsupported_class"
    )
  }
}
