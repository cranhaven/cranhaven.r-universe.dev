# R/analysis-covariance-helpers.R
#
# Internal helpers for get_covariance().
#
# Functions:
#   .score_covariance()           — build the score
#                                     z_i = a_i * (x_i - xbar)(y_i - ybar)
#                                                                 * n/(n-1)
#                                  on the pairwise-complete active domain.
#   .covariance_pair_taylor()     — Taylor pair covariance.
#   .covariance_pair_replicate()  — replicate-weight pair covariance.
#   .covariance_pair_twophase()   — two-phase pair covariance.
#   .covariance_pair_nonprob()    — HT (nonprob) pair covariance.
#   .covariance_pair_result()     — dispatcher.
#   .attach_covariance_labels()   — column-level label attacher.

# ── .score_covariance() ───────────────────────────────────────────────────────
#
# Build the pairwise-complete score vector z_i for get_covariance():
#     z_i = a_i * (x_i - xbar_d)(y_i - ybar_d) * n_d / (n_d - 1)
# where xbar_d, ybar_d, and n_d are computed on the in-domain,
# pairwise-non-NA, positive-weight subset.
#
# Out-of-domain / either-NA / non-positive-weight rows contribute z_i = 0.
# Returns the score vector along with the plug-in covariance estimate, the
# Kish-corrected plug-in variances of x and y on the same active domain,
# and sufficient statistics the dispatch branches need.
#
# @param x_col   Numeric vector of the first variable (full length).
# @param y_col   Numeric vector of the second variable (full length).
# @param w       Numeric vector of weights (full length).
# @param domain  Numeric 0/1 vector of "active" membership (full length).
# @return Named list:
#   score     — numeric(n_full): z_i with 0 on inactive rows.
#   c_hat     — plug-in covariance estimate (scalar).
#   var_x     — Kish-corrected plug-in variance of x on active domain.
#   var_y     — Kish-corrected plug-in variance of y on active domain.
#   xbar      — plug-in domain mean of x (scalar, NaN if n_d = 0).
#   ybar      — plug-in domain mean of y (scalar, NaN if n_d = 0).
#   W         — sum(w * a) domain weight total (scalar).
#   n_d       — integer unweighted count of active rows.
#   active    — logical(n_full): TRUE for in-domain, both-non-NA, w>0 rows.
#
# Notes:
#   - When n_d = 0L or W <= 0, returns c_hat / var_x / var_y all NaN, score 0.
#   - When n_d = 1L, kish = Inf and dev products are 0, so c_hat = NaN; score
#     contains NaN on active rows (caller emits the insufficient-n warning).
#   - Snaps near-zero deviations and dev-products to exact 0 on the same
#     scale-relative tolerance used by .score_variance(), so constant
#     variables yield exactly zero covariance / variance (no FP drift).
.score_covariance <- function(x_col, y_col, w, domain) {
  n_full <- length(x_col)
  active <- (domain > 0) &
    !is.na(x_col) &
    !is.na(y_col) &
    (w > 0)
  n_d <- as.integer(sum(active))
  score <- numeric(n_full)

  if (n_d == 0L) {
    return(list(
      score = score,
      c_hat = NaN,
      var_x = NaN,
      var_y = NaN,
      xbar = NaN,
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
      c_hat = NaN,
      var_x = NaN,
      var_y = NaN,
      xbar = NaN,
      ybar = NaN,
      W = 0,
      n_d = n_d,
      active = active
    ))
  }

  x_a <- x_col[active]
  y_a <- y_col[active]
  w_a <- w[active]
  xbar <- sum(w_a * x_a) / W
  ybar <- sum(w_a * y_a) / W

  if (n_d < 2L) {
    # n/(n-1) is undefined; point estimate is NaN, score is NaN for active
    # rows (caller emits surveycore_warning_covariance_insufficient_n).
    score[active] <- NaN
    return(list(
      score = score,
      c_hat = NaN,
      var_x = NaN,
      var_y = NaN,
      xbar = xbar,
      ybar = ybar,
      W = W,
      n_d = n_d,
      active = active
    ))
  }

  kish <- n_d / (n_d - 1L)

  # Snap near-zero deviations to exact 0 (constant-variable safety, matches
  # .score_variance()).
  dx <- x_a - xbar
  dy <- y_a - ybar
  scale_x <- (abs(xbar) + 1)^2
  scale_y <- (abs(ybar) + 1)^2
  dx[dx^2 < scale_x * .Machine$double.eps] <- 0
  dy[dy^2 < scale_y * .Machine$double.eps] <- 0

  prod_dev <- dx * dy
  # Snap the cross-deviation to 0 when scale-relative magnitude is FP noise.
  scale_xy <- (abs(xbar) + 1) * (abs(ybar) + 1)
  prod_dev[abs(prod_dev) < scale_xy * .Machine$double.eps] <- 0

  score_active <- kish * prod_dev
  score[active] <- score_active

  c_hat <- sum(w_a * score_active) / W
  # Plug-in Kish-corrected variances on the same active domain (so diagonal
  # parity with get_variance() holds at 1e-10 when active domains match).
  var_x <- kish * sum(w_a * dx^2) / W
  var_y <- kish * sum(w_a * dy^2) / W

  list(
    score = score,
    c_hat = c_hat,
    var_x = var_x,
    var_y = var_y,
    xbar = xbar,
    ybar = ybar,
    W = W,
    n_d = n_d,
    active = active
  )
}


# ── .se_srs_cov() ─────────────────────────────────────────────────────────────
#
# Goodnight / Mood-Graybill SRS-reference SE of a covariance estimate:
#   SE_SRS(cov) = sqrt( (Var(x) * Var(y) + cov^2) / (n - 1) )
# Used as the SRS comparator when computing design effect.
#
# Returns 0 when n < 2 (caller will short-circuit upstream anyway).
.se_srs_cov <- function(c_hat, var_x, var_y, n_d) {
  if (is.null(n_d) || is.na(n_d) || n_d < 2L) {
    return(0)
  }
  num <- var_x * var_y + c_hat^2
  if (!is.finite(num) || num < 0) {
    num <- max(0, num)
  }
  sqrt(num / (n_d - 1L))
}


# ── .covariance_pair_taylor() ─────────────────────────────────────────────────
#
# Taylor-linearization domain covariance. The point estimate is the weighted
# mean of the score z_i = a_i * (x_i - xbar)(y_i - ybar) * n/(n-1), and the
# variance of that estimate is the weighted-mean influence run through the
# stratified-clustered recursive variance engine.
#
# @param design  A survey_taylor object.
# @param x_col   Character: column name of the first variable.
# @param y_col   Character: column name of the second variable.
# @param domain  Numeric 0/1 vector (full length).
# @param na.rm   Logical scalar; echoed only — pairwise-complete construction
#                of `domain` is the caller's responsibility.
# @return Named list: covariance, se, se_srs, var_x, var_y, n, n_weighted.
.covariance_pair_taylor <- function(design, x_col, y_col, domain, na.rm) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]
  x_all <- data[[x_col]]
  y_all <- data[[y_col]]

  sc <- .score_covariance(x_all, y_all, w, domain)
  n_d <- sc$n_d

  if (n_d == 0L || !is.finite(sc$W) || sc$W <= 0) {
    return(list(
      covariance = NaN,
      se = NaN,
      se_srs = NaN,
      var_x = NaN,
      var_y = NaN,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_d < 2L) {
    return(list(
      covariance = NaN,
      se = NaN,
      se_srs = NaN,
      var_x = NaN,
      var_y = NaN,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  c_hat <- sc$c_hat
  W <- sc$W
  score <- sc$score
  active_num <- as.numeric(sc$active)

  # Influence function of the weighted mean of the score:
  #   u_i = w_i * a_i * (score_i - c_hat) / W   (out-of-domain rows = 0)
  u <- w * active_num * (score - c_hat) / W

  mats <- .build_cluster_matrices(data, vars)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  infl_mat <- matrix(u, ncol = 1L, dimnames = list(NULL, x_col))
  v <- .svy_recvar(
    infl_mat,
    mats$clusters_mat,
    mats$strata_mat,
    mats$fpcs,
    lonely.psu = lonely.psu
  )

  se <- sqrt(max(0, v[1L, 1L]))
  se_srs <- .se_srs_cov(c_hat, sc$var_x, sc$var_y, n_d)

  list(
    covariance = c_hat,
    se = se,
    se_srs = se_srs,
    var_x = sc$var_x,
    var_y = sc$var_y,
    n = n_d,
    n_weighted = W
  )
}


# ── .covariance_pair_replicate() ──────────────────────────────────────────────
#
# Replicate-weight domain covariance. Recomputes C^{(r)} per replicate with
# per-replicate xbar^{(r)} / ybar^{(r)} and runs .svy_rep_var() on the
# replicate estimates.
#
# @param design  A survey_replicate object.
# @param x_col   Character: column name of the first variable.
# @param y_col   Character: column name of the second variable.
# @param domain  Numeric 0/1 vector (full length).
# @param na.rm   Logical scalar; echoed only.
# @return Named list: covariance, se, se_srs, var_x, var_y, n, n_weighted.
.covariance_pair_replicate <- function(design, x_col, y_col, domain, na.rm) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]
  x_all <- data[[x_col]]
  y_all <- data[[y_col]]

  sc <- .score_covariance(x_all, y_all, w, domain)
  n_d <- sc$n_d

  if (n_d == 0L || !is.finite(sc$W) || sc$W <= 0) {
    return(list(
      covariance = NaN,
      se = NaN,
      se_srs = NaN,
      var_x = NaN,
      var_y = NaN,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_d < 2L) {
    return(list(
      covariance = NaN,
      se = NaN,
      se_srs = NaN,
      var_x = NaN,
      var_y = NaN,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  c_hat <- sc$c_hat
  active <- sc$active

  rep_mat <- as.matrix(data[, vars$repweights, drop = FALSE])
  n_rep <- ncol(rep_mat)
  kish <- n_d / (n_d - 1L)

  rep_active <- rep_mat[active, , drop = FALSE]
  x_active <- x_all[active]
  y_active <- y_all[active]

  rep_W <- colSums(rep_active)
  rep_X <- colSums(rep_active * x_active)
  rep_Y <- colSums(rep_active * y_active)
  rep_xbar <- ifelse(rep_W > 0, rep_X / rep_W, NA_real_)
  rep_ybar <- ifelse(rep_W > 0, rep_Y / rep_W, NA_real_)

  rep_c <- vapply(
    seq_len(n_rep),
    function(r) {
      Wr <- rep_W[[r]]
      if (!is.finite(Wr) || Wr <= 0) {
        return(NA_real_)
      }
      xb_r <- rep_xbar[[r]]
      yb_r <- rep_ybar[[r]]
      dx_r <- x_active - xb_r
      dy_r <- y_active - yb_r
      scale_x_r <- (abs(xb_r) + 1)^2
      scale_y_r <- (abs(yb_r) + 1)^2
      dx_r[dx_r^2 < scale_x_r * .Machine$double.eps] <- 0
      dy_r[dy_r^2 < scale_y_r * .Machine$double.eps] <- 0
      prod_r <- dx_r * dy_r
      scale_xy_r <- (abs(xb_r) + 1) * (abs(yb_r) + 1)
      prod_r[abs(prod_r) < scale_xy_r * .Machine$double.eps] <- 0
      num <- sum(rep_active[, r] * prod_r) * kish
      num / Wr
    },
    numeric(1L)
  )

  na_dropped <- sum(is.na(rep_c))
  na_frac <- na_dropped / n_rep
  if (
    isTRUE(
      .nonprob_rep_na_warn(design, na_frac, na_dropped, n_rep, vars$scale)
    )
  ) {
    return(list(
      covariance = NA_real_,
      se = NA_real_,
      se_srs = NA_real_,
      var_x = NA_real_,
      var_y = NA_real_,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  v <- .svy_rep_var(
    rep_c,
    scale = vars$scale,
    rscales = if (!is.null(vars$rscales)) vars$rscales else rep(1L, n_rep),
    mse = isTRUE(vars$mse),
    coef = c_hat
  )

  se <- sqrt(max(0, v))
  se_srs <- .se_srs_cov(c_hat, sc$var_x, sc$var_y, n_d)

  list(
    covariance = c_hat,
    se = se,
    se_srs = se_srs,
    var_x = sc$var_x,
    var_y = sc$var_y,
    n = n_d,
    n_weighted = sc$W
  )
}


# ── .covariance_pair_twophase() ───────────────────────────────────────────────
#
# Two-phase domain covariance. Builds the score on the Phase 2 rows (using
# the calibrated weights w_full / pi2_full) and runs the score's weighted-
# mean influence through .twophasevar(). Phase-1-only rows contribute zero.
#
# @param design  A survey_twophase object.
# @param x_col   Character: column name of the first variable.
# @param y_col   Character: column name of the second variable.
# @param domain  Numeric 0/1 vector (full length).
# @param na.rm   Logical scalar; echoed only.
# @return Named list: covariance, se, se_srs, var_x, var_y, n, n_weighted.
.covariance_pair_twophase <- function(design, x_col, y_col, domain, na.rm) {
  data <- design@data
  ph1_vars <- design@variables$phase1
  subset <- data[[design@variables$subset]] # logical, full length

  w_full <- data[[ph1_vars$weights]]
  pi2_full <- .compute_phase2_probs(design, subset)
  cal_wt_full <- w_full / pi2_full

  dom_ph2 <- domain[subset]
  cal_ph2 <- cal_wt_full[subset]
  x_ph2 <- data[[x_col]][subset]
  y_ph2 <- data[[y_col]][subset]

  sc <- .score_covariance(x_ph2, y_ph2, cal_ph2, dom_ph2)
  n_d <- sc$n_d

  if (n_d == 0L || !is.finite(sc$W) || sc$W <= 0) {
    return(list(
      covariance = NaN,
      se = NaN,
      se_srs = NaN,
      var_x = NaN,
      var_y = NaN,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_d < 2L) {
    return(list(
      covariance = NaN,
      se = NaN,
      se_srs = NaN,
      var_x = NaN,
      var_y = NaN,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  c_hat <- sc$c_hat
  W <- sc$W
  score_ph2 <- sc$score
  active_num_ph2 <- as.numeric(sc$active)

  infl_ph2 <- cal_ph2 * active_num_ph2 * (score_ph2 - c_hat) / W
  n_total <- nrow(data)
  influence <- numeric(n_total)
  ph2_idx <- which(subset)
  influence[ph2_idx] <- infl_ph2

  lonely.psu <- getOption("survey.lonely.psu", "remove")
  v_raw <- .twophasevar(influence, design, lonely.psu)
  v_scalar <- if (is.matrix(v_raw)) drop(v_raw)[1L] else as.numeric(v_raw)

  se <- sqrt(max(0, v_scalar))
  se_srs <- .se_srs_cov(c_hat, sc$var_x, sc$var_y, n_d)

  list(
    covariance = c_hat,
    se = se,
    se_srs = se_srs,
    var_x = sc$var_x,
    var_y = sc$var_y,
    n = n_d,
    n_weighted = W
  )
}


# ── .covariance_pair_nonprob() ────────────────────────────────────────────────
#
# Non-probability (calibrated) domain covariance. Uses the HT Taylor
# linearization of the weighted mean of the score, matching the variance-
# of-variance treatment in .nonprob_variance_cell():
#   z_i = a_i * (x_i - xbar)(y_i - ybar) * n/(n-1)
#   Var(C_hat) = n/(n-1) * sum(w_i^2 * (z_i - C_hat)^2) / W^2
#
# @param design  A survey_nonprob object.
# @param x_col   Character: column name of the first variable.
# @param y_col   Character: column name of the second variable.
# @param domain  Numeric 0/1 vector (full length).
# @param na.rm   Logical scalar; echoed only.
# @return Named list: covariance, se, se_srs, var_x, var_y, n, n_weighted.
.covariance_pair_nonprob <- function(design, x_col, y_col, domain, na.rm) {
  data <- design@data
  vars <- design@variables
  w <- data[[vars$weights]]
  x_all <- data[[x_col]]
  y_all <- data[[y_col]]

  sc <- .score_covariance(x_all, y_all, w, domain)
  n_d <- sc$n_d

  if (n_d == 0L || !is.finite(sc$W) || sc$W <= 0) {
    return(list(
      covariance = NaN,
      se = NaN,
      se_srs = NaN,
      var_x = NaN,
      var_y = NaN,
      n = 0L,
      n_weighted = 0
    ))
  }

  if (n_d < 2L) {
    return(list(
      covariance = NaN,
      se = NaN,
      se_srs = NaN,
      var_x = NaN,
      var_y = NaN,
      n = n_d,
      n_weighted = sc$W
    ))
  }

  c_hat <- sc$c_hat
  W <- sc$W
  active <- sc$active
  w_sub <- w[active]
  score_sub <- sc$score[active]

  var_chat <- (n_d / (n_d - 1L)) *
    sum(w_sub^2 * (score_sub - c_hat)^2) /
    W^2
  se <- sqrt(max(0, var_chat))
  se_srs <- .se_srs_cov(c_hat, sc$var_x, sc$var_y, n_d)

  list(
    covariance = c_hat,
    se = se,
    se_srs = se_srs,
    var_x = sc$var_x,
    var_y = sc$var_y,
    n = n_d,
    n_weighted = W
  )
}


# ── .covariance_pair_result() ─────────────────────────────────────────────────
#
# Dispatch to the correct per-pair covariance estimator by design class.
# Supports survey_taylor, survey_replicate, survey_twophase, and
# survey_nonprob.
#
# @param design  Any survey design object.
# @param x_col   Character: column name of the first variable.
# @param y_col   Character: column name of the second variable.
# @param domain  Numeric 0/1 vector (full length).
# @param na.rm   Logical scalar; echoed only.
# @return Named list: covariance, se, se_srs, var_x, var_y, n, n_weighted.
.covariance_pair_result <- function(design, x_col, y_col, domain, na.rm) {
  if (S7::S7_inherits(design, survey_taylor)) {
    .covariance_pair_taylor(design, x_col, y_col, domain, na.rm)
  } else if (S7::S7_inherits(design, survey_replicate)) {
    .covariance_pair_replicate(design, x_col, y_col, domain, na.rm)
  } else if (S7::S7_inherits(design, survey_twophase)) {
    .covariance_pair_twophase(design, x_col, y_col, domain, na.rm)
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    if (!is.null(design@variables$repweights)) {
      .covariance_pair_replicate(design, x_col, y_col, domain, na.rm)
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
      .covariance_pair_nonprob(design, x_col, y_col, domain, na.rm)
    }
  } else {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Unsupported design class {.cls {class(design)[[1L]]}} ",
          "in {.fn get_covariance}."
        )
      ),
      class = "surveycore_error_unsupported_class"
    )
  }
}


# ── .attach_covariance_labels() ───────────────────────────────────────────────
#
# Attach `label` attributes to every column in a survey_covariance result
# tibble, for gt integration. Preserves the .meta attribute and S3 class.
#
# @param result        A survey_covariance tibble (post-name-style rename).
# @param conf_level    Numeric scalar. Used to interpolate CI bound labels.
# @param name_style    Character(1). "surveycore" or "broom".
# @param group_vars    Character vector of group variable names (raw).
# @param design        The survey design object (used for group-var labels).
# @return The tibble with labels attached to every column, unchanged
#         otherwise.
.attach_covariance_labels <- function(
  result,
  conf_level,
  name_style,
  group_vars,
  design
) {
  saved_meta <- attr(result, ".meta")
  saved_class <- class(result)

  conf_pct <- formatC(100 * conf_level, digits = 0, format = "f")

  if (identical(name_style, "broom")) {
    lbl_map <- c(
      var1 = "Variable 1",
      var2 = "Variable 2",
      estimate = "Estimate",
      std.error = "Std. error",
      var = "Variance of estimate",
      conf.low = "Conf. low",
      conf.high = "Conf. high",
      cv = "CV",
      moe = "MOE",
      deff = "Deff",
      n = "n",
      n.weighted = "n.weighted",
      n_weighted = "n.weighted"
    )
  } else {
    lbl_map <- c(
      var1 = "Variable 1",
      var2 = "Variable 2",
      covariance = "Covariance",
      se = "SE",
      var = "Variance of estimate",
      ci_low = paste0(conf_pct, "% CI low"),
      ci_high = paste0(conf_pct, "% CI high"),
      cv = "CV",
      moe = "Margin of error",
      deff = "Design effect",
      n = "N",
      n_weighted = "Weighted N"
    )
  }

  for (nm in names(result)) {
    if (nm %in% names(lbl_map)) {
      attr(result[[nm]], "label") <- lbl_map[[nm]]
    } else if (nm %in% group_vars) {
      gv_label <- design@metadata@variable_labels[[nm]] %||%
        attr(design@data[[nm]], "label", exact = TRUE) %||%
        nm
      attr(result[[nm]], "label") <- gv_label
    }
  }

  attr(result, ".meta") <- saved_meta
  class(result) <- saved_class
  result
}
