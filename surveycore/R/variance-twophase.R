# ---------------------------------------------------------------------------
# R/variance-twophase.R
# ---------------------------------------------------------------------------
# Two-phase sampling variance estimation for survey_twophase designs.
#
# ATTRIBUTION — VENDORED CODE:
# The functions .twophasevar(), .twophase_phase1_var(), .twophase_phase2_var(),
# and .compute_phase2_probs() are adapted from the survey package by
# Thomas Lumley
# (https://cran.r-project.org/package=survey), licensed under GPL-2 | GPL-3.
# Modifications: integrated with S7 classes; three-method dispatch made
# explicit; RCPP path removed; error messages converted to cli format;
# data access via @variables instead of survey's internal list structure.
# ---------------------------------------------------------------------------

# ===========================================================================
# Section 1: General two-phase variance engine
# ===========================================================================

# General two-phase variance engine. Dispatches to phase 1 and optionally
# phase 2 variance components based on method stored in design@variables$method.
# Returns a numeric scalar.
#' @noRd
.twophasevar <- function(
  influence,
  design,
  lonely.psu = getOption("survey.lonely.psu", "remove")
) {
  vars <- design@variables
  method <- vars$method
  subset <- design@data[[vars$subset]] # logical vector, length = nrow(data)

  # method = "full" requires phase 2 design information at estimation time
  if (identical(method, "full")) {
    has_phase2_info <- !is.null(vars$phase2$ids) ||
      !is.null(vars$phase2$strata) ||
      !is.null(vars$phase2$probs)
    if (!has_phase2_info) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Two-phase variance method {.val full} requires ",
            "phase 2 design structure."
          ),
          "i" = paste0(
            "No {.arg ids2}, {.arg strata2}, or {.arg probs2} were ",
            "specified in {.fn as_survey_twophase}."
          ),
          "v" = paste0(
            'Reconstruct with {.arg method = "approx"} or supply ',
            "phase 2 design variables."
          )
        ),
        class = "surveycore_error_full_requires_phase2"
      )
    }
  }

  # Phase 1 variance contribution
  v1 <- .twophase_phase1_var(influence, design, method, lonely.psu)

  if (identical(method, "simple")) {
    return(v1)
  }

  # Phase 2 variance contribution (full and approx only)
  v2 <- .twophase_phase2_var(influence, design, subset, lonely.psu)

  v1 + v2
}


# ===========================================================================
# Section 2: Phase 1 and Phase 2 variance components
# ===========================================================================

# Compute phase 1 variance contribution.
# Adapted from survey:::svyrecvar.phase1 / onestrat.phase1.
# For "simple": standard .svy_recvar() with raw influence scores.
# For "full"/"approx": implements the onestrat.phase1 formula directly:
#   V1 = sum_s { f_s * nPSUfull_s/(nPSUfull_s-1) *
#                sum_j { ((x_j * pi2_j - xcenter_s) / sqrt(pi2_j))^2 } }
# where nPSU_s = phase 2 PSUs in stratum, nPSUfull_s = phase 1 PSUs in stratum,
# pi2_j = nPSU_s/nPSUfull_s (PSU-level stratum sampling fraction,
# survey's `usu`),
# xcenter_s = mean(x_j * nPSU_s/nPSUfull_s) centering term,
# and f_s = (N_s - n_s) / N_s is the Phase 1 FPC factor for stratum s
# (1 when no FPC column is provided).
# Returns a numeric scalar.
.twophase_phase1_var <- function(influence, design, method, lonely.psu) {
  ph1_data <- design@data
  ph1_vars <- design@variables$phase1
  subset <- design@data[[design@variables$subset]]
  n_ph1 <- nrow(ph1_data)

  # Build phase 1 strata and PSU columns (all n1 rows, used for nPSUfull)
  strata_all <- if (!is.null(ph1_vars$strata)) {
    ph1_data[[ph1_vars$strata]]
  } else {
    rep(1L, n_ph1)
  }

  psu_all <- if (!is.null(ph1_vars$ids)) {
    raw_ids <- ph1_data[[ph1_vars$ids[[1L]]]]
    if (isTRUE(ph1_vars$nest) && !is.null(ph1_vars$strata)) {
      as.integer(interaction(strata_all, raw_ids, drop = TRUE))
    } else {
      raw_ids
    }
  } else {
    seq_len(n_ph1)
  }

  if (identical(method, "simple")) {
    # simple: raw influence for Phase 2 rows; 0 for Phase 1-only
    score <- numeric(n_ph1)
    score[subset] <- influence[subset]

    clusters_mat <- matrix(psu_all, ncol = 1L)
    strata_mat <- matrix(strata_all, ncol = 1L)

    psu_per_stratum <- tapply(psu_all, strata_all, function(ps) {
      length(unique(ps))
    })
    sampsize_vec <- as.integer(psu_per_stratum[as.character(strata_all)])
    sampsize_mat <- matrix(sampsize_vec, ncol = 1L)

    fpc_col <- if (!is.null(ph1_vars$fpc)) ph1_data[[ph1_vars$fpc]] else NULL
    popsize_mat <- if (!is.null(fpc_col)) {
      if (any(fpc_col > 1, na.rm = TRUE)) {
        matrix(as.numeric(fpc_col), ncol = 1L)
      } else {
        matrix(as.numeric(sampsize_vec / fpc_col), ncol = 1L)
      }
    } else {
      NULL
    }

    fpcs <- list(sampsize = sampsize_mat, popsize = popsize_mat)
    v <- .svy_recvar(
      matrix(score, ncol = 1L),
      clusters_mat,
      strata_mat,
      fpcs,
      lonely.psu = lonely.psu
    )
    return(v[1L, 1L])
  }

  # full or approx: implement onestrat.phase1 formula using phase 2 rows only
  ph2_idx <- which(subset)
  infl_ph2 <- influence[ph2_idx]
  strata_ph2 <- strata_all[ph2_idx]
  psu_ph2 <- psu_all[ph2_idx]

  # nPSUfull per phase 1 stratum (total distinct PSU IDs across all n1 rows)
  nPSUfull_by_strat <- tapply(psu_all, strata_all, function(p) {
    length(unique(p))
  })

  # Compute per-row Phase 2 PSU sampling fraction (survey's `usu`).
  # With phase 2 strata: n_ph2_PSU_in_strata2 / n_ph1_PSU_in_strata2
  # per Phase 2 row.
  # Without phase 2 strata: n_ph2_PSU_in_ph1_strat / n_ph1_PSU_in_ph1_strat
  # per Phase 2 row.
  strata2_var <- design@variables$phase2$strata
  if (!is.null(strata2_var)) {
    strata2_all <- ph1_data[[strata2_var]]
    strata2_ph2 <- strata2_all[ph2_idx]
    n_ph2_by_s2 <- tapply(psu_ph2, strata2_ph2, function(p) length(unique(p)))
    n_ph1_by_s2 <- tapply(psu_all, strata2_all, function(p) length(unique(p)))
    usu_ph2 <- as.numeric(
      n_ph2_by_s2[as.character(strata2_ph2)] /
        n_ph1_by_s2[as.character(strata2_ph2)]
    )
  } else {
    n_ph2_by_strat <- tapply(psu_ph2, strata_ph2, function(p) length(unique(p)))
    usu_ph2 <- as.numeric(
      n_ph2_by_strat[as.character(strata_ph2)] /
        nPSUfull_by_strat[as.character(strata_ph2)]
    )
  }

  # Phase 1 FPC column (if provided)
  fpc_all <- if (!is.null(ph1_vars$fpc)) ph1_data[[ph1_vars$fpc]] else NULL

  # Compute V1 by phase 1 stratum using onestrat.phase1 formula
  v1_total <- 0
  unique_strata <- unique(strata_ph2)

  for (s in unique_strata) {
    idx_s <- which(strata_ph2 == s)
    x_s <- infl_ph2[idx_s]
    psu_s <- psu_ph2[idx_s]
    nPSUfull_s <- as.integer(nPSUfull_by_strat[[as.character(s)]])

    # Aggregate influence to PSU level
    x_agg <- as.numeric(tapply(x_s, psu_s, sum))
    nPSU_s <- length(x_agg)

    # Aggregate usu to PSU level (mean handles PSUs spanning multiple
    # phase 2 strata)
    pi2_agg <- as.numeric(tapply(usu_ph2[idx_s], psu_s, mean))

    if (nPSU_s <= 1L) {
      next
    } # lonely PSU: contributes 0

    # xcenter = colMeans(x * nPSU/nPSUfull) — BEFORE multiplying by pi2
    xcenter_s <- mean(x_agg * nPSU_s / nPSUfull_s)

    # x = x * pi2 - xcenter
    x_scaled <- x_agg * pi2_agg - xcenter_s

    # Phase 1 FPC correction (if given)
    fpc_s <- if (!is.null(fpc_all)) fpc_all[ph2_idx[idx_s[[1L]]]] else NULL
    f_s <- if (is.null(fpc_s)) {
      1
    } else {
      ifelse(is.infinite(fpc_s), 1, (fpc_s - nPSUfull_s) / fpc_s)
    }

    scale_s <- f_s * nPSUfull_s / (nPSUfull_s - 1L)

    # V1_s = scale * crossprod(x_scaled / sqrt(pi2_agg))
    v1_total <- v1_total + sum((x_scaled / sqrt(pi2_agg))^2) * scale_s
  }

  v1_total
}


# Compute phase 2 variance contribution (used by "full" and "approx" only).
# Adapted from the phase 2 component of survey:::twophasevar.
# Returns a numeric scalar (extracts [1L, 1L] from the .svy_recvar() matrix).
#' @noRd
.twophase_phase2_var <- function(influence, design, subset, lonely.psu) {
  ph1_vars <- design@variables$phase1
  ph2_vars <- design@variables$phase2
  ph2_data <- design@data[subset, , drop = FALSE]
  n_ph2 <- nrow(ph2_data)

  # Phase 2 score: influence[i] * sqrt(1 / w1[i])
  w1 <- design@data[[ph1_vars$weights]]
  w1_ph2 <- w1[subset]
  score_ph2 <- influence[subset] * sqrt(1 / w1_ph2)
  score_mat <- matrix(score_ph2, ncol = 1L)

  # Phase 2 strata column
  strata2_col <- if (!is.null(ph2_vars$strata)) {
    ph2_data[[ph2_vars$strata]]
  } else {
    rep(1L, n_ph2)
  }

  # Phase 2 PSU column (each row is own PSU if no ids2)
  psu2_col <- if (!is.null(ph2_vars$ids)) {
    ph2_data[[ph2_vars$ids[[1L]]]]
  } else {
    seq_len(n_ph2)
  }

  clusters2_mat <- matrix(psu2_col, ncol = 1L)
  strata2_mat <- matrix(strata2_col, ncol = 1L)

  # Phase 2 sampsize: unique PSU IDs per Phase 2 stratum among subset == TRUE
  psu2_per_stratum <- tapply(psu2_col, strata2_col, function(ps) {
    length(unique(ps))
  })
  sampsize2_vec <- as.integer(psu2_per_stratum[as.character(strata2_col)])
  sampsize2_mat <- matrix(sampsize2_vec, ncol = 1L)

  # Phase 2 popsize
  popsize2_mat <- if (!is.null(ph2_vars$fpc)) {
    # Explicit FPC column
    fpc2_vals <- ph2_data[[ph2_vars$fpc]]
    matrix(as.integer(fpc2_vals), ncol = 1L)
  } else if (!is.null(ph2_vars$strata) || !is.null(ph2_vars$ids)) {
    # Auto-compute popsize from all Phase 1 rows
    strata2_all <- if (!is.null(ph2_vars$strata)) {
      design@data[[ph2_vars$strata]]
    } else {
      rep(1L, nrow(design@data))
    }
    psu2_all <- if (!is.null(ph2_vars$ids)) {
      design@data[[ph2_vars$ids[[1L]]]]
    } else {
      seq_len(nrow(design@data))
    }
    pop2_per_stratum <- tapply(psu2_all, strata2_all, function(ps) {
      length(unique(ps))
    })
    popsize2_vec <- as.integer(pop2_per_stratum[as.character(strata2_col)])
    matrix(popsize2_vec, ncol = 1L)
  } else {
    # No explicit Phase 2 structure (probs2, strata2, ids2, or fpc2):
    # treat Phase 1 sample as the Phase 2 population (standard two-phase FPC)
    matrix(rep(as.integer(nrow(design@data)), n_ph2), ncol = 1L)
  }

  fpcs2 <- list(sampsize = sampsize2_mat, popsize = popsize2_mat)

  v <- .svy_recvar(
    score_mat,
    clusters2_mat,
    strata2_mat,
    fpcs2,
    lonely.psu = lonely.psu
  )
  v[1L, 1L]
}


# ===========================================================================
# Section 3: Phase 2 probability computation
# ===========================================================================

# Derive conditional Phase 2 inclusion probabilities (pi2|1,i).
# Adapted from internal probability computation in survey/R/twophase.R.
# Returns numeric vector of length nrow(design@data), with 1 for Phase 1-only
# rows (influence = 0 there, so the placeholder value does not affect results).
#' @noRd
.compute_phase2_probs <- function(design, subset) {
  vars <- design@variables
  n <- nrow(design@data)
  pi2 <- rep(1, n)

  if (!is.null(vars$phase2$probs)) {
    # Priority 1: explicit probs column
    pi2 <- as.numeric(design@data[[vars$phase2$probs]])
    pi2[!subset] <- 1 # Phase 1-only rows: placeholder 1
  } else if (!is.null(vars$phase2$strata)) {
    # Priority 2: within-stratum sampling fraction (n2_stratum / n1_stratum)
    strata2_all <- design@data[[vars$phase2$strata]]
    frac_by_stratum <- tapply(subset, strata2_all, mean)
    pi2 <- as.numeric(frac_by_stratum[as.character(strata2_all)])
    pi2[!subset] <- 1 # Phase 1-only rows: placeholder 1
  } else {
    # Priority 3: overall fraction (n2 / n1)
    frac <- mean(subset)
    pi2[subset] <- frac
    # Phase 1-only rows: pi2 stays 1
  }

  pi2
}


# ===========================================================================
# Section 4: Input builder and estimand adapters
# ===========================================================================

# Prepare all inputs for standard mean and total estimands.
# NA handling: na.rm = TRUE excludes Phase 2 rows with NA y; na.rm = FALSE
# propagates NA through influence values and hence through .twophasevar().
#
# Returns named list:
#   y, w              — Phase 2 values after na.rm
#   influence_mean    — full-length vector, 0 for Phase 1-only rows
#   influence_total   — full-length vector, 0 for Phase 1-only rows
#   ybar_w, total_w   — weighted mean and total (NA when n_phase2 = 0)
#   n_phase2          — integer count of valid Phase 2 observations
#' @noRd
.twophase_build_inputs <- function(design, y_col, na.rm) {
  data <- design@data
  ph1_vars <- design@variables$phase1
  subset <- data[[design@variables$subset]] # logical, length = nrow(data)

  y_full <- data[[y_col]]
  w_full <- data[[ph1_vars$weights]]

  y_ph2 <- y_full[subset]
  w_ph2 <- w_full[subset]

  # Calibrated weights: Phase 1 weight divided by conditional Phase 2
  # probability
  pi2_full <- .compute_phase2_probs(design, subset)
  pi2_ph2 <- pi2_full[subset]
  cal_wt <- w_ph2 / pi2_ph2

  n_total <- nrow(data)

  if (na.rm) {
    keep <- !is.na(y_ph2)
    y_used <- y_ph2[keep]
    w_used <- cal_wt[keep]
  } else {
    keep <- rep(TRUE, length(y_ph2))
    y_used <- y_ph2
    w_used <- cal_wt
  }

  n_phase2 <- sum(keep)

  # Compute weighted estimates
  if (n_phase2 == 0L) {
    ybar_w <- NA_real_
    total_w <- NA_real_
  } else {
    total_w <- sum(w_used * y_used)
    sum_w <- sum(w_used)
    ybar_w <- total_w / sum_w
  }

  # Build full-length influence vectors (0 for Phase 1-only rows)
  influence_mean <- numeric(n_total)
  influence_total <- numeric(n_total)

  ph2_idx <- which(subset)

  if (na.rm) {
    active_ph2_idx <- ph2_idx[keep]
    if (length(active_ph2_idx) > 0L && !is.na(ybar_w)) {
      sum_w <- sum(w_used)
      influence_mean[active_ph2_idx] <- w_used * (y_used - ybar_w) / sum_w
      influence_total[active_ph2_idx] <- w_used * y_used
    }
  } else {
    # All Phase 2 rows — NA y propagates to NA influence (correct behavior)
    if (length(ph2_idx) > 0L) {
      sum_w <- sum(cal_wt)
      influence_mean[ph2_idx] <- cal_wt * (y_ph2 - ybar_w) / sum_w
      influence_total[ph2_idx] <- cal_wt * y_ph2
    }
  }

  list(
    y = y_used,
    w = w_used,
    influence_mean = influence_mean,
    influence_total = influence_total,
    ybar_w = ybar_w,
    total_w = total_w,
    n_phase2 = as.integer(n_phase2)
  )
}


# Compute weighted mean and its variance for a survey_twophase design.
# Returns list(mean, var, se).
#' @noRd
.twophase_mean <- function(design, y_col, na.rm = TRUE) {
  inp <- .twophase_build_inputs(design, y_col, na.rm)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  if (is.na(inp$ybar_w)) {
    return(list(mean = NA_real_, var = NA_real_, se = NA_real_))
  }

  v <- .twophasevar(inp$influence_mean, design, lonely.psu)
  list(mean = inp$ybar_w, var = v, se = sqrt(v))
}


# Compute weighted total and its variance for a survey_twophase design.
# Returns list(total, var, se).
#' @noRd
.twophase_total <- function(design, y_col, na.rm = TRUE) {
  inp <- .twophase_build_inputs(design, y_col, na.rm)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  if (is.na(inp$total_w)) {
    return(list(total = NA_real_, var = NA_real_, se = NA_real_))
  }

  v <- .twophasevar(inp$influence_total, design, lonely.psu)
  list(total = inp$total_w, var = v, se = sqrt(v))
}


# ===========================================================================
# Section 5: Degrees of freedom
# ===========================================================================

# Degrees of freedom for a survey_twophase design (Phase 1 df).
# Formula: sum(n_s - 1) over strata s = sum(PSUs_per_stratum) - n_strata.
# Returns a non-negative integer.
#' @noRd
.twophase_df <- function(design) {
  ph1_vars <- design@variables$phase1
  ph1_data <- design@data

  strata_col <- if (!is.null(ph1_vars$strata)) {
    ph1_data[[ph1_vars$strata]]
  } else {
    rep(1L, nrow(ph1_data))
  }

  psu_col <- if (!is.null(ph1_vars$ids)) {
    ph1_data[[ph1_vars$ids[[1L]]]]
  } else {
    seq_len(nrow(ph1_data))
  }

  if (isTRUE(ph1_vars$nest) && !is.null(ph1_vars$strata)) {
    psu_col <- as.integer(interaction(strata_col, psu_col, drop = TRUE))
  }

  psu_per_stratum <- tapply(psu_col, strata_col, function(p) length(unique(p)))
  as.integer(sum(psu_per_stratum) - length(psu_per_stratum))
}


# ===========================================================================
# Section 6: Variance-covariance pair estimation for survey_twophase
# (used by get_corr())
# ===========================================================================

# Compute variance-covariance pair estimates and the 3x3 meta-vcov of
# (Var(X), Cov(X,Y), Var(Y)) for two-phase designs.
#
# Constructs full-length influence vectors for each of the three estimands
# (Var(X), Cov(X,Y), Var(Y)) using calibrated Phase 2 weights, then
# computes the 3x3 meta-vcov using the polarization identity:
#   Cov(T_a, T_b) = (Var(T_a + T_b) - Var(T_a) - Var(T_b)) / 2
# This allows reuse of the scalar .twophasevar() for all variance methods
# (simple, approx, full) without modification.
#
# @param design  A survey_twophase object.
# @param x_col  Character. Name of the first numeric variable.
# @param y_col  Character. Name of the second numeric variable.
# @param domain Numeric 0/1 vector (full length). Domain membership mask.
# @param na.rm  Logical. If TRUE, exclude rows where x or y is NA.
# @return Named list: $a, $b, $c, $sigma (3x3), $n, $n_weighted
#' @noRd
.vcov_pair_twophase <- function(design, x_col, y_col, domain, na.rm = TRUE) {
  data <- design@data
  ph1_vars <- design@variables$phase1
  subset <- data[[design@variables$subset]] # logical, full length
  n_total <- nrow(data)

  # Calibrated weights (Phase 1 weight / conditional Phase 2 inclusion prob)
  w_full <- data[[ph1_vars$weights]]
  pi2_full <- .compute_phase2_probs(design, subset)
  cal_wt <- w_full / pi2_full

  x_all_full <- data[[x_col]]
  y_all_full <- data[[y_col]]

  # Restrict domain to Phase 2 rows
  dom_ph2 <- domain[subset]
  cal_ph2 <- cal_wt[subset]
  x_ph2 <- x_all_full[subset]
  y_ph2 <- y_all_full[subset]

  # Pair mask over Phase 2 rows
  if (na.rm) {
    pair_ph2 <- dom_ph2 * as.numeric(!is.na(x_ph2) & !is.na(y_ph2))
  } else {
    pair_ph2 <- dom_ph2
  }

  n_d <- as.integer(sum(pair_ph2))
  W_d <- sum(cal_ph2 * pair_ph2)

  if (n_d < 2L || W_d <= 0) {
    sigma <- matrix(NA_real_, 3L, 3L)
    return(list(
      a = NA_real_,
      b = NA_real_,
      c = NA_real_,
      sigma = sigma,
      n = n_d,
      n_weighted = W_d
    ))
  }

  x_safe <- ifelse(pair_ph2 > 0, x_ph2, 0)
  y_safe <- ifelse(pair_ph2 > 0, y_ph2, 0)
  xbar <- sum(cal_ph2 * pair_ph2 * x_safe) / W_d
  ybar <- sum(cal_ph2 * pair_ph2 * y_safe) / W_d

  cx <- pair_ph2 * (x_safe - xbar)
  cy <- pair_ph2 * (y_safe - ybar)

  a <- sum(cal_ph2 * cx^2) / W_d
  b <- sum(cal_ph2 * cx * cy) / W_d
  c_val <- sum(cal_ph2 * cy^2) / W_d

  # Full-length influence vectors (0 for Phase 1-only rows)
  ph2_idx <- which(subset)
  infl_a <- numeric(n_total)
  infl_b <- numeric(n_total)
  infl_c <- numeric(n_total)
  infl_a[ph2_idx] <- cal_ph2 * pair_ph2 * (cx^2 - a) / W_d
  infl_b[ph2_idx] <- cal_ph2 * pair_ph2 * (cx * cy - b) / W_d
  infl_c[ph2_idx] <- cal_ph2 * pair_ph2 * (cy^2 - c_val) / W_d

  lonely.psu <- getOption("survey.lonely.psu", "remove")

  # Diagonal variances
  var_a <- .twophasevar(infl_a, design, lonely.psu)
  var_b <- .twophasevar(infl_b, design, lonely.psu)
  var_c <- .twophasevar(infl_c, design, lonely.psu)

  # Off-diagonal covariances via polarization identity:
  # Cov(T_a, T_b) = (Var(T_a + T_b) - Var(T_a) - Var(T_b)) / 2
  var_ab <- .twophasevar(infl_a + infl_b, design, lonely.psu)
  var_ac <- .twophasevar(infl_a + infl_c, design, lonely.psu)
  var_bc <- .twophasevar(infl_b + infl_c, design, lonely.psu)

  cov_ab <- (var_ab - var_a - var_b) / 2
  cov_ac <- (var_ac - var_a - var_c) / 2
  cov_bc <- (var_bc - var_b - var_c) / 2

  sigma <- matrix(
    c(var_a, cov_ab, cov_ac, cov_ab, var_b, cov_bc, cov_ac, cov_bc, var_c),
    nrow = 3L,
    ncol = 3L
  )

  list(a = a, b = b, c = c_val, sigma = sigma, n = n_d, n_weighted = W_d)
}
