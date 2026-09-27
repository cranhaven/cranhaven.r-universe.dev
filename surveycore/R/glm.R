# R/glm.R
#
# Phase 2: Survey-weighted GLM
#
# Contents:
#   survey_glm_fit       — S7 class definition + validator
#   survey_glm()         — constructor (Steps 1–6)
#   .glm_score()         — per-observation score matrix (n × p)
#   .glm_sandwich_vcov() — bread · meat · bread sandwich assembler
#   .taylor_var_score_matrix()  — Taylor variance of p-dimensional score total
#   .glm_vcov_dispatch() — variance dispatch for all 4 design classes
#   .glm_degrees_of_freedom()   — design-based residual df (with clamping)
#
# Spec: plans/spec-phase-2.md §II–IV, §VIII
# Error classes: plans/error-messages.md rows 65–87

#' GLM Standards
#'
#' @srrstats {RE1.0} survey_glm() requires a formula interface: the first
#'   argument is a standard R formula (e.g. y ~ x1 + x2), and no alternative
#'   matrix interface is provided.
#'
#' @srrstats {RE1.1} survey_glm() documents how formula interfaces are
#'   converted to model matrices in its @details section: formula is passed to
#'   stats::model.matrix() via stats::glm(), producing an n x p model matrix
#'   with treatment contrasts for factors.
#'
#' @srrstats {RE1.2} survey_glm() documents expected predictor types in its
#'   @details section: numeric, integer, logical, factor, and character are
#'   all accepted; character is coerced to factor by stats::model.matrix().
#'
#' @srrstats {RE1.3} The coefficient vector (fit@coefficients) and vcov matrix
#'   (fit@vcov) carry names produced by stats::model.matrix(). The model frame
#'   preserves row names from the rows used in fitting. Documented in
#'   survey_glm() @details ("Row and column names").
#'
#' @srrstats {RE1.3a} Rows excluded by na.action (na.omit) do not appear in
#'   the model frame; documented in survey_glm() @details ("Row and column
#'   names"). No other input metadata is silently dropped.
#'
#' @srrstats {RE1.4} Input assumptions for survey_glm() are documented in
#'   @details ("Input assumptions"): one row per sampled unit, positive finite
#'   weights, formula variables must be columns of design@data, no automatic
#'   centering or scaling applied.
#'
#' @srrstats {RE2.0} survey_glm() documents data transformations in @details
#'   ("Data transformations"): no automatic transformation is applied; factor
#'   encoding is handled by stats::model.matrix(); link function
#'   transformations are applied by the family object.
#'
#' @srrstats {RE2.1} survey_glm() accepts an na.action argument (forwarded to
#'   stats::glm()) controlling how missing values in predictor and response
#'   data are handled; default is na.omit.
#'
#' @srrstats {RE2.2} survey_glm() provides na.action to control
#'   missing-value handling in model frame variables. Two options are
#'   documented: na.omit (default) and na.fail. See @param na.action and
#'   @details ("Missing values").
#'
#' @srrstats {RE2.4} survey_glm() detects perfect collinearity in the model
#'   matrix pre-fitting: stats::glm() produces NA coefficients for aliased
#'   columns; surveycore checks for any NA in coef(fit) immediately after
#'   fitting and throws surveycore_error_singular_model_matrix.
#'
#' @srrstats {RE2.4a} Perfect collinearity among predictor variables (e.g.,
#'   x2 = 2 * x1) produces NA in stats::coef(fit), which is detected and
#'   thrown as surveycore_error_singular_model_matrix.
#'
#' @srrstats {RE2.4b} Perfect collinearity between the response and a
#'   predictor (e.g., y ~ y) causes numerical rank deficiency detected via
#'   the NA coefficient check, thrown as surveycore_error_singular_model_matrix.
#'
#' @srrstats {RE3.0} Convergence warnings from the underlying stats::glm()
#'   fit are propagated unchanged to the user; surveycore does not suppress
#'   them.
#'
#' @srrstats {RE3.1} survey_glm() accepts quiet = FALSE (default) or
#'   quiet = TRUE to suppress convergence warnings. The convergence status is
#'   always stored in fit@converged regardless of quiet.
#'
#' @srrstats {RE3.2} Convergence thresholds inherit stats::glm.control()
#'   defaults (epsilon = 1e-8, maxit = 25), which are well-established and
#'   documented in ?glm.control.
#'
#' @srrstats {RE3.3} Users may supply a control argument to survey_glm()
#'   (forwarded to stats::glm()), allowing explicit setting of epsilon and
#'   maxit via glm.control().
#'
#' @srrstats {RE4.0} survey_glm() returns a survey_glm_fit S7 object that
#'   stores coefficients, vcov, formula, design, and the underlying glm fit.
#'
#' @srrstats {RE5.0} Scaling relationships are documented in survey_glm()
#'   @details ("Performance"): O(n*p^2) for score matrix computation, O(p^3)
#'   for bread matrix. Dominant cost is typically stats::glm() IRLS.
#'
#' @noRd
NULL


# ===========================================================================
# Section 1: survey_glm_fit S7 class
# ===========================================================================

#' Survey-Weighted GLM Fit Object
#'
#' S7 class produced by [survey_glm()]. Holds all regression output from a
#' survey-weighted generalised linear model: design-based coefficient
#' estimates, variance-covariance matrix, fitted values, residuals, and
#' model metadata.
#'
#' @param coefficients Named numeric vector of length `p`.
#' @param vcov `p x p` design-based variance-covariance matrix.
#' @param fitted_values Numeric vector of length `n` (response scale).
#' @param residuals Working residuals from IRLS, length `n`.
#' @param weights Survey weights used in fitting, length `n`.
#' @param design The original [survey_base] survey design object.
#' @param degf Raw design degrees of freedom (positive scalar). For
#'   `survey_taylor` designs (including SRS, which is absorbed into Taylor):
#'   number of PSUs minus number of strata. For `survey_replicate` designs:
#'   number of replicates minus one. For `survey_twophase`: Phase 1 PSUs
#'   minus Phase 1 strata. For `survey_nonprob`: `Inf` (no design-based
#'   df). This is *not* the residual degrees of freedom used for
#'   t-statistics and confidence intervals; those are computed as
#'   `degf - (p - 1)` where `p` is the number of model coefficients.
#' @param family GLM family object (e.g. `gaussian()`, `binomial()`).
#' @param formula Model formula.
#' @param null_deviance Null model deviance.
#' @param deviance Residual deviance.
#' @param df_null Classical null df (`fit$df.null` from `stats::glm()`).
#' @param df_residual Classical residual df (`fit$df.residual`, i.e. `n - p`).
#'   Used for the deviance display; **not** the design-based residual df.
#' @param converged Logical; whether IRLS converged.
#' @param call The `survey_glm()` call (language object or `NULL`).
#' @param fit_ Internal raw `stats::glm()` result; `NULL` after serialisation.
#' @param term_assign Integer vector: `attr(model.matrix(fit_), "assign")`
#'   captured at fit time. Maps design-matrix columns to formula terms (0 =
#'   intercept; positive values index `attr(terms(formula), "term.labels")`).
#'   Required by `get_anova()`'s serialization-safe Wald path (spec §3.3.1):
#'   after `@fit_` is stripped via `saveRDS()`, the term-to-column map
#'   survives in this slot. Default `integer(0)`.
#'
#' @return A `survey_glm_fit` object.
#'
#' @examples
#' # survey_glm_fit objects are created by survey_glm(), not directly
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' fit <- survey_glm(d, age ~ sex)
#' fit@coefficients
#' @seealso [survey_glm()] to create a `survey_glm_fit`.
#' @family constructors
#' @export
survey_glm_fit <- S7::new_class(
  "survey_glm_fit",
  properties = list(
    coefficients = S7::new_property(S7::class_numeric),
    vcov = S7::new_property(S7::class_any),
    fitted_values = S7::new_property(S7::class_numeric),
    residuals = S7::new_property(S7::class_numeric),
    weights = S7::new_property(S7::class_numeric),
    design = S7::new_property(class = survey_base),
    degf = S7::new_property(S7::class_numeric),
    family = S7::new_property(S7::class_list),
    formula = S7::new_property(default = NULL),
    null_deviance = S7::new_property(S7::class_numeric),
    deviance = S7::new_property(S7::class_numeric),
    df_null = S7::new_property(S7::class_numeric),
    df_residual = S7::new_property(S7::class_numeric),
    converged = S7::new_property(S7::class_logical),
    call = S7::new_property(default = NULL),
    fit_ = S7::new_property(default = NULL),
    term_assign = S7::new_property(S7::class_integer, default = integer(0))
  ),
  validator = function(self) {
    p <- length(self@coefficients)
    if (p == 0L) {
      "coefficients must be non-empty"
    } else if (!identical(dim(self@vcov), c(p, p))) {
      paste0("vcov must be ", p, "x", p, " (same dimension as coefficients)")
    } else if (length(self@fitted_values) == 0L) {
      "fitted_values must be non-empty"
    } else if (length(self@residuals) != length(self@fitted_values)) {
      "residuals and fitted_values must have the same length"
    } else if (length(self@weights) != length(self@fitted_values)) {
      "weights and fitted_values must have the same length"
    } else if (length(self@degf) != 1L || self@degf <= 0) {
      "degf must be a single positive number"
    } else if (
      !is.null(self@formula) &&
        !inherits(self@formula, "formula")
    ) {
      "formula must be a formula object or NULL"
    } else {
      NULL
    }
  }
)


# ===========================================================================
# Section 2: Internal helpers
# ===========================================================================

# ── .glm_score() ──────────────────────────────────────────────────────────────
#
# Compute the per-observation score matrix for the Binder (1983) sandwich.
#
# The correct Binder score for obs i is:
#   u_i = survey_wt_i * x_i * (y_i - mu_i)
#
# This is computed as fit$weights * working_residual, because:
#   fit$weights  = survey_wt_i * IRLS_wt_i  (final IRLS weights from stats::glm)
#   working_resid = (y_i - mu_i) / IRLS_wt_i
#   product      = survey_wt_i * (y_i - mu_i)  ✓
#
# For Gaussian/identity, IRLS_wt = 1 so fit$weights = survey_wt; the score
# reduces to survey_wt * (y - mu). For binomial logit, IRLS_wt = mu*(1-mu)
# and working_resid = (y-mu)/(mu*(1-mu)), giving the same correct result.
# Using survey_wt alone (with working residuals) is wrong for non-Gaussian
# families because it introduces a spurious 1/IRLS_wt factor.
#
# @param fit    A stats::glm() result (fitted model with survey weights).
# @param design The survey_base design object.
# @param row_mask Integer or logical vector indexing the rows of design@data
#   that were used in fitting. NULL means all rows were used.
# @param domain_mask Logical vector length nrow(design@data). TRUE = in-domain.
# @return n × p matrix (full design row count); zero rows for out-of-domain
#   or NA-excluded observations.
#' @noRd
.glm_score <- function(fit, design, row_mask = NULL, domain_mask = NULL) {
  n_full <- nrow(design@data)
  p <- length(stats::coef(fit))

  # Build full-design score matrix, zero for excluded rows
  mm_full <- matrix(0.0, nrow = n_full, ncol = p)
  res_full <- numeric(n_full)
  wt_full <- numeric(n_full) # will hold fit$weights at fit rows

  if (is.null(row_mask)) {
    row_idx <- seq_len(n_full)
  } else {
    row_idx <- row_mask
  }

  mm_fit <- stats::model.matrix(fit) # n_fit × p
  res_fit <- stats::residuals(fit, type = "working") # n_fit
  # fit$weights = survey_wt * IRLS_wt_final (final IRLS working weights)
  wt_fit <- fit$weights # n_fit

  mm_full[row_idx, ] <- mm_fit
  res_full[row_idx] <- res_fit
  wt_full[row_idx] <- wt_fit

  # Apply domain mask: out-of-domain contributions are zero
  if (!is.null(domain_mask)) {
    zero_rows <- !domain_mask
    mm_full[zero_rows, ] <- 0.0
    res_full[zero_rows] <- 0.0
    wt_full[zero_rows] <- 0.0
  }

  # u_i = (survey_wt_i * IRLS_wt_i) * x_i * wr_i
  #      = survey_wt_i * (y_i - mu_i) * x_i   [correct Binder score]
  mm_full * (wt_full * res_full)
}


# ── .get_glm_weights() ────────────────────────────────────────────────────────
#
# Return the full n_full-length weight vector for GLM fitting and score
# computation.
#
# For survey_twophase: calibrated weight = w_ph1 / pi2|1. Non-phase-2 rows
#   are set to 0 (not NA) so that mm_full * w_full computations in
#   .glm_score() produce 0 rather than NA for excluded rows.
# For all other design types: the standard weight column from design@data.
#
# @param design A survey design object.
# @return Numeric vector of length nrow(design@data).
#' @noRd
.get_glm_weights <- function(design) {
  if (S7::S7_inherits(design, survey_twophase)) {
    data <- design@data
    subset <- data[[design@variables$subset]]
    w_ph1 <- data[[design@variables$phase1$weights]]
    pi2 <- .compute_phase2_probs(design, subset)
    cal_wt <- w_ph1 / pi2
    cal_wt[!subset] <- 0 # non-phase-2 rows contribute zero to scores
    cal_wt
  } else {
    design@data[[design@variables$weights]]
  }
}


# ── .glm_sandwich_vcov() ──────────────────────────────────────────────────────
#
# Assemble the sandwich variance-covariance matrix:
#   Var(β̂) = bread · meat · bread
#
# bread = summary(fit)$cov.unscaled = (X'W~X)^(-1) from the IRLS information
# matrix — correct for ALL GLM families (uses working weights, not just
# survey weights). For Gaussian/identity, reduces to (X'WX)^(-1).
#
# @param meat_vcov p x p meat matrix = Var_design(sum u_i).
# @param bread     p x p matrix from summary(fit)$cov.unscaled.
# @return p x p variance-covariance matrix.
.glm_sandwich_vcov <- function(meat_vcov, bread) {
  bread %*% meat_vcov %*% bread
}


# ── .taylor_var_score_matrix() ────────────────────────────────────────────────
#
# Compute the design-based variance of the p-dimensional score total vector
# for a Taylor series design, using the Phase 0 variance machinery.
#
# Passes the n × p score matrix to .svy_recvar() treating each column as a
# survey-weighted total. Returns the p × p variance (meat) matrix.
#
# @param score_matrix n × p score matrix from .glm_score() (pre-weighted).
# @param design       A survey_taylor object.
# @return p × p meat matrix.
#' @noRd
.taylor_var_score_matrix <- function(score_matrix, design) {
  mats <- .build_cluster_matrices(design@data, design@variables)
  lonely.psu <- getOption("survey.lonely.psu", "remove")

  .svy_recvar(
    score_matrix,
    mats$clusters_mat,
    mats$strata_mat,
    mats$fpcs,
    lonely.psu = lonely.psu
  )
}


# ── .twophase_var_score_matrix() ──────────────────────────────────────────────
#
# Compute the p × p meat matrix for a twophase design by calling
# .twophasevar() for each pair of score columns using the polarization
# identity: Cov(A,B) = (Var(A+B) - Var(A) - Var(B)) / 2.
#
# @param score_matrix n × p matrix from .glm_score().
# @param design       A survey_twophase object.
# @return p × p meat matrix.
#' @noRd
.twophase_var_score_matrix <- function(score_matrix, design) {
  p <- ncol(score_matrix)
  meat <- matrix(0.0, p, p)

  # Diagonal: Var(col j)
  for (j in seq_len(p)) {
    meat[j, j] <- .twophasevar(score_matrix[, j], design)
  }

  # Off-diagonal: use polarization identity
  if (p > 1L) {
    for (j in seq_len(p - 1L)) {
      for (k in (j + 1L):p) {
        sum_jk <- score_matrix[, j] + score_matrix[, k]
        cov_jk <- (.twophasevar(sum_jk, design) -
          meat[j, j] -
          meat[k, k]) /
          2
        meat[j, k] <- cov_jk
        meat[k, j] <- cov_jk
      }
    }
  }
  meat
}


# ── .glm_vcov_dispatch() ──────────────────────────────────────────────────────
#
# Dispatch to the correct variance estimation path based on design class.
# Returns the p × p design-based variance-covariance matrix for coefficients.
#
# @param fit         stats::glm() result.
# @param design      Survey design object.
# @param row_mask    Integer index of rows used in fitting (from na.action).
# @param domain_mask Logical full-length domain indicator.
# @return p × p matrix.
#' @noRd
.glm_vcov_dispatch <- function(fit, design, row_mask, domain_mask) {
  bread <- summary(fit)$cov.unscaled

  if (S7::S7_inherits(design, survey_taylor)) {
    score_mat <- .glm_score(fit, design, row_mask, domain_mask)
    meat <- .taylor_var_score_matrix(score_mat, design)
    .glm_sandwich_vcov(meat, bread)
  } else if (S7::S7_inherits(design, survey_twophase)) {
    score_mat <- .glm_score(fit, design, row_mask, domain_mask)
    meat <- .twophase_var_score_matrix(score_mat, design)
    .glm_sandwich_vcov(meat, bread)
  } else if (S7::S7_inherits(design, survey_replicate)) {
    .glm_replicate_vcov(fit, design, row_mask, domain_mask)
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    if (!is.null(design@variables$repweights)) {
      .glm_replicate_vcov(fit, design, row_mask, domain_mask)
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
      .glm_calibrated_vcov(fit, design, row_mask, domain_mask)
    }
  } else {
    cli::cli_abort(
      c(
        "x" = "Unsupported design class {.cls {class(design)[[1L]]}} in GLM.",
        "i" = paste0(
          "Use {.fn as_survey}, {.fn as_survey_replicate}, ",
          "or {.fn as_survey_twophase}."
        )
      ),
      class = "surveycore_error_unsupported_class"
    )
  }
}


# ── .glm_replicate_vcov() ─────────────────────────────────────────────────────
#
# Replicate-weight variance for GLM:
#   1. For each replicate r: refit GLM with repweights_r → β̂_r
#   2. Deviation: d_r = β̂_r − β̂
#   3. Var(β̂) = Σ_r c_r * d_r %*% t(d_r)
#
# If a replicate refit fails to converge, warn and use d_r = 0 (zero
# deviation), per spec §8.3.
#
# @param fit         Full-sample stats::glm() result.
# @param design      A survey_replicate object.
# @param row_mask    Rows used in full-sample fit.
# @param domain_mask Domain membership (for in-domain-only refitting).
# @return p × p matrix.
#' @noRd
.glm_replicate_vcov <- function(fit, design, row_mask, domain_mask) {
  vars <- design@variables
  data <- design@data
  rep_mat <- as.matrix(data[, vars$repweights, drop = FALSE])
  n_rep <- ncol(rep_mat)
  coef_0 <- stats::coef(fit)
  p <- length(coef_0)

  scale <- vars$scale
  rscales <- if (!is.null(vars$rscales)) vars$rscales else rep(1.0, n_rep)
  mse <- isTRUE(vars$mse)

  # Determine fitting rows (intersection of row_mask and domain_mask)
  if (is.null(row_mask)) {
    fit_rows <- seq_len(nrow(data))
  } else {
    fit_rows <- row_mask
  }
  # Domain restriction (already applied for main fit; refit on same rows)
  if (!is.null(domain_mask)) {
    fit_rows <- fit_rows[domain_mask[fit_rows]]
  }

  fit_data <- data[fit_rows, , drop = FALSE]

  # Accumulate variance: sigma = scale * Σ rscales_r * d_r d_r'
  sigma <- matrix(0.0, p, p)
  center <- if (mse) coef_0 else NULL
  rep_coefs <- matrix(NA_real_, nrow = n_rep, ncol = p)

  for (r in seq_len(n_rep)) {
    wr <- rep_mat[fit_rows, r]
    fit_r <- tryCatch(
      suppressWarnings(
        do.call(
          stats::glm,
          list(
            formula = stats::formula(fit),
            family = stats::family(fit),
            data = fit_data,
            weights = wr,
            na.action = stats::na.omit
          )
        )
      ),
      error = function(e) NULL
    )

    if (is.null(fit_r) || !fit_r$converged) {
      if (!is.null(fit_r) && !fit_r$converged) {
        cli::cli_warn(
          c(
            "!" = "Replicate {r} of {n_rep} did not converge.",
            "i" = "Using zero deviation for this replicate."
          ),
          class = "surveycore_warning_glm_convergence"
        )
      }
      rep_coefs[r, ] <- coef_0 # zero deviation
    } else {
      rep_coefs[r, ] <- stats::coef(fit_r)
    }
  }

  if (!mse) {
    ok <- !apply(rep_coefs, 1L, anyNA)
    center <- colMeans(rep_coefs[ok & rscales > 0, , drop = FALSE])
  }

  for (r in seq_len(n_rep)) {
    d_r <- rep_coefs[r, ] - center
    sigma <- sigma + rscales[r] * outer(d_r, d_r)
  }
  sigma * scale
}


# ── .glm_calibrated_vcov() ────────────────────────────────────────────────────
#
# SRS score-based sandwich for survey_nonprob designs:
#   meat = (1 - f) * n * S²_u
#
# where u_i = w_i * x_i * e_i (pre-weighted scores from .glm_score()) and
# S²_u is the p × p sample covariance matrix of the score rows.
#
# Uses the full p × p sample covariance matrix — off-diagonal terms are
# required when bread is non-diagonal (multiple predictors).
#
# survey_nonprob has no FPC, so f = 0 (infinite population).
#
# @param fit         Full-sample stats::glm() result.
# @param design      A survey_nonprob object.
# @param row_mask    Rows used in fitting.
# @param domain_mask Domain indicator.
# @return p × p matrix.
#' @noRd
.glm_calibrated_vcov <- function(fit, design, row_mask, domain_mask) {
  bread <- summary(fit)$cov.unscaled
  score_mat <- .glm_score(fit, design, row_mask, domain_mask)

  n_full <- nrow(design@data)

  # Use only rows that contributed to the fit for variance estimation
  fit_rows <- if (is.null(row_mask)) seq_len(n_full) else row_mask
  if (!is.null(domain_mask)) {
    fit_rows <- fit_rows[domain_mask[fit_rows]]
  }
  n_fit <- length(fit_rows)

  # survey_nonprob has no FPC → f = 0 (infinite population assumption)
  f <- 0

  # p × p sample covariance of score columns (includes off-diagonals).
  # score_used rows = u_i = w_i * x_i * e_i (pre-weighted scores).
  # SRS sandwich: Var(Σ u_i) = (1-f) * n * S²_u.
  score_used <- score_mat[fit_rows, , drop = FALSE]
  meat <- (1 - f) * n_fit * stats::var(score_used)

  .glm_sandwich_vcov(meat, bread)
}


# ── .glm_degrees_of_freedom() ─────────────────────────────────────────────────
#
# Compute design-based degf and warn if clamping is needed.
#
# @param design     Survey design object.
# @param n_coef     Total coefficients p (including intercept).
# @return Numeric(1): max(1, .degf(design) - (p - 1)).
#' @noRd
.glm_degrees_of_freedom <- function(design, n_coef) {
  degf_raw <- .degf(design)
  df_resid <- degf_raw - (n_coef - 1L)
  if (df_resid <= 0) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "Design degrees of freedom (",
          degf_raw,
          ") minus model ",
          "parameters (",
          n_coef - 1L,
          ") is \u2264 0. ",
          "Clamping {.code df_residual = 1}."
        ),
        "i" = "CI bounds and p-values are conservative."
      ),
      class = "surveycore_warning_insufficient_df"
    )
  }
  degf_raw # store the raw degf; clamping happens in .glm_confint()
}


# ===========================================================================
# Section 3: survey_glm() constructor
# ===========================================================================

#' Fit a Survey-Weighted Generalised Linear Model
#'
#' Fits a GLM to survey data, producing design-based coefficient estimates
#' and variance-covariance matrix via the Binder (1983) sandwich estimator.
#' All four concrete surveycore design classes are supported
#' (`survey_taylor`, `survey_replicate`, `survey_twophase`, `survey_nonprob`).
#' `survey_collection` inputs are rejected; call `survey_glm()` on each
#' element individually.
#'
#' @param design A survey design object created by [as_survey()],
#'   [as_survey_replicate()], [as_survey_twophase()], or
#'   [as_survey_nonprob()].
#' @param formula A model formula in standard R notation
#'   (e.g. `y ~ x1 + x2`). Mutually exclusive with `response`/`predictors`.
#'   If `NULL` and `response` is also `NULL`, errors with
#'   `surveycore_error_formula_missing`.
#' @param response Character string naming the outcome variable.
#'   Programmatic alternative to `formula`. Mutually exclusive with
#'   `formula`. Use with `predictors` to build a model formula via
#'   `reformulate(predictors, response)`. Suitable for `lapply()` iteration.
#' @param predictors Character vector of predictor variable names. Used with
#'   `response` to build the model formula. If `response` is supplied and
#'   `predictors` is `NULL`, an intercept-only model is fitted.
#' @param family A GLM family object specifying the error distribution and
#'   link function. Default `gaussian()`. Any family accepted by
#'   [stats::glm()] is supported. For `binomial()` and `quasibinomial()`
#'   families, the "non-integer #successes" warning is suppressed because
#'   survey weights are non-integer by design.
#' @param na.action How to handle `NA` values in the model frame.
#'   Default `na.omit` (silently drops rows with any `NA` in model
#'   variables). `na.fail` errors with `surveycore_error_na_in_data` listing
#'   the offending columns and NA counts. Note: `na.action` applies only to
#'   model frame variables; survey weights are validated separately.
#' @param start Starting values for the coefficient vector.
#' @param etastart Starting values for the linear predictor.
#' @param mustart Starting values for the mean.
#' @param control A list of GLM control parameters passed to
#'   [stats::glm.control()].
#' @param quiet Logical. If `TRUE`, suppresses convergence warnings emitted by
#'   `survey_glm()` and its internal replicate-weight refitting loop.
#'   Convergence status is always stored in `fit@converged` regardless of this
#'   setting, so non-convergence can still be detected programmatically.
#'   Default `FALSE`.
#'
#' @return A [survey_glm_fit] S7 object.
#'
#' @details
#' **Variance estimation:** Uses the Binder (1983) sandwich estimator, which
#' decomposes into per-observation score vectors passed to the Phase 0
#' variance machinery. The bread `(X'WX)^(-1)` accounts for IRLS working
#' weights and is correct for all GLM families including binomial and
#' Poisson.
#'
#' **`binomial()` family:** Wraps the `stats::glm()` call in
#' `suppressWarnings()` to suppress the "non-integer #successes" warning
#' that fires for every survey-weighted binomial model.
#'
#' **Domain estimation:** Use `surveytidy::filter()` before calling
#' `survey_glm()`. The GLM is fit on in-domain rows only; variance
#' estimation uses the full design for correct design-based SEs.
#'
#' **Multinomial response:** `cbind()` on the LHS of `formula` is not
#' supported. Multinomial logistic regression is deferred to a later phase.
#'
#' **Formula to model matrix:** `survey_glm()` passes the formula to
#' `stats::model.matrix()` via `stats::glm()`. Factor and character predictors
#' are dummy-coded using `model.matrix()` default contrasts (treatment coding:
#' first level as reference). Numeric predictors enter as-is. Interaction
#' terms (`:`, `*`) and inline transformations (`log()`, `I()`) are supported
#' as in any standard R formula. The resulting model matrix is `n x p` where
#' `p` is the number of coefficients including the intercept.
#'
#' **Predictor variable types:** Predictors may be numeric, integer, logical,
#' factor, or character. Character predictors are coerced to factor by
#' `stats::model.matrix()`. Ordered factors use polynomial contrasts by
#' default. All other R types (list columns, complex, raw) will produce an
#' error from `stats::model.matrix()`.
#'
#' **Input assumptions:** surveycore assumes (1) each row of `design@data`
#' represents one sampled unit; (2) survey weights are positive and finite
#' for all rows (validated at construction time); (3) the model formula
#' variables are columns of `design@data`; (4) the design is correctly
#' specified before calling `survey_glm()`. No centering, scaling, or
#' other pre-processing is applied to predictor variables beyond what the
#' formula specifies.
#'
#' **Data transformations:** No automatic transformation is applied to
#' predictor or response variables. Factor encoding is handled by
#' `stats::model.matrix()` using the active contrasts. Link function
#' transformations (e.g. `log` link in `poisson()`) are applied by the
#' family object, not by surveycore. To apply custom transformations, use
#' `I()` or `log()` etc. inside the formula.
#'
#' **Row and column names:** The coefficient vector returned in
#' `fit@coefficients` carries the names produced by `stats::model.matrix()`
#' (e.g. `"(Intercept)"`, `"sexFemale"`, `"age"`). `fit@vcov` carries the
#' same names on rows and columns. `model.frame.survey_glm_fit()` returns the
#' model frame with row names matching the rows used in fitting (i.e. the
#' row names of `design@data` after applying `na.action`). Rows excluded by
#' `na.action = na.omit` do not appear in the model frame.
#'
#' **Missing values:** `na.action` controls handling of `NA` in model frame
#' variables (predictors and response). `na.omit` (default) silently drops
#' rows with any `NA`; the variance estimator uses the full design for
#' correct sandwich SEs. `na.fail` stops with an informative error listing
#' all variables containing `NA` and the row count for each. Survey weights
#' are validated separately at construction time and must not contain `NA`.
#'
#' **Performance:** Runtime scales as O(*n* · *p*²) for the score matrix
#' computation and O(*p*³) for the bread matrix (solve). For Taylor designs,
#' variance estimation adds O(*n* · *H* · *p*²) where *H* is the number of
#' strata. For replicate designs it adds O(*R* · *n* · *p*) where *R* is the
#' number of replicates. The dominant cost for large *n* is typically the
#' `stats::glm()` IRLS fit (O(*n* · *p*² · *I*) per IRLS iteration).
#'
#' @examples
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#'
#' # Linear model: respondent age predicted by education and sex
#' fit <- survey_glm(d, age ~ educ + sex)
#' fit@coefficients
#' fit@vcov
#'
#' # Programmatic interface — suitable for lapply()
#' results <- lapply(c("age", "educ"), function(v) {
#'   survey_glm(d, response = v, predictors = "sex")
#' })
#' @references
#' Binder, D.A. (1983) On the variances of asymptotically normal estimators
#' from complex surveys. \emph{International Statistical Review}
#' \bold{51}(3), 279--292.
#'
#' Binder, D.A. (1991) Use of estimating functions for interval estimation
#' from complex surveys. \emph{Proceedings of the American Statistical
#' Association, Section on Survey Research Methods}, 34--42.
#'
#' Lumley, T. and Scott, A. (2014) Tests in surveys with complex sampling.
#' \emph{Journal of the Royal Statistical Society: Series B}
#' \bold{76}(2), 431--452.
#'
#' @family constructors
#' @export
survey_glm <- function(
  design,
  formula = NULL,
  response = NULL,
  predictors = NULL,
  family = stats::gaussian(),
  na.action = stats::na.omit,
  start = NULL,
  etastart = NULL,
  mustart = NULL,
  control = list(),
  quiet = FALSE
) {
  if (S7::S7_inherits(design, survey_collection)) {
    fn_name <- "survey_glm"
    cli::cli_abort(
      c(
        "x" = "{.fn {fn_name}} does not yet support {.cls survey_collection} inputs.",
        "i" = paste0(
          "Run {.fn {fn_name}} on each survey individually, or see ",
          "{.topic survey_collection} for the current dispatch coverage."
        )
      ),
      class = "surveycore_error_collection_not_supported_by_fn"
    )
  }
  # ── Step 1: Validate inputs ────────────────────────────────────────────────
  .check_unsupported_class(design, "survey_glm")

  # Warn if design has @groups set
  if (length(design@groups) > 0L) {
    cli::cli_warn(
      c(
        "!" = "{.fn survey_glm} does not support grouped designs.",
        "i" = paste0(
          "The {.field @groups} property is ignored. ",
          "Use {.fn surveytidy::group_by} after fitting to group results."
        )
      ),
      class = "surveycore_warning_groups_ignored_in_glm"
    )
  }

  # Formula resolution
  has_formula <- !is.null(formula)
  has_response <- !is.null(response)
  has_predictors <- !is.null(predictors)

  if (has_formula && (has_response || has_predictors)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg formula} and {.arg response}/{.arg predictors} ",
          "are mutually exclusive."
        ),
        "i" = paste0(
          "Specify the model using either {.arg formula} or ",
          "{.arg response}/{.arg predictors}, not both."
        )
      ),
      class = "surveycore_error_formula_conflict"
    )
  }

  if (!has_formula && !has_response && !has_predictors) {
    cli::cli_abort(
      c("x" = "{.arg formula} is required."),
      class = "surveycore_error_formula_missing"
    )
  }

  if (!has_formula && !has_response && has_predictors) {
    cli::cli_abort(
      c(
        "x" = "{.arg formula} is required.",
        "i" = "{.arg response} is required when using {.arg predictors}."
      ),
      class = "surveycore_error_formula_missing"
    )
  }

  # Build formula from programmatic interface
  if (!has_formula && has_response) {
    formula <- stats::reformulate(
      termlabels = if (!is.null(predictors)) predictors else "1",
      response = response
    )
    # Reset environment: reformulate() inherits survey_glm()'s frame, which
    # would embed all local variables in the stored object. Use baseenv() so
    # the stored formula is portable and CRAN-policy compliant (no reliance
    # on .GlobalEnv).
    environment(formula) <- baseenv()
  }

  # Validate formula type
  if (!inherits(formula, "formula")) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg formula} must be a formula object, ",
          "not {.cls {class(formula)[1]}}."
        )
      ),
      class = "surveycore_error_formula_invalid"
    )
  }

  # Check for cbind() LHS (multinomial — unsupported)
  lhs <- formula[[2L]]
  if (is.call(lhs) && identical(lhs[[1L]], quote(cbind))) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.code cbind()} on the left-hand side of {.arg formula} ",
          "is not supported."
        ),
        "i" = paste0(
          "Multinomial logistic regression is deferred to a later phase. ",
          "Use a single binary or continuous response variable."
        )
      ),
      class = "surveycore_error_cbind_response_unsupported"
    )
  }

  # Validate response variables exist in design@data
  resp_vars <- all.vars(formula[[2L]])
  missing_resp <- setdiff(resp_vars, names(design@data))
  if (length(missing_resp) > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Response variable {.field {missing_resp[1L]}} not found ",
          "in survey data."
        )
      ),
      class = "surveycore_error_response_not_found"
    )
  }

  # Warn if response is a design variable
  design_vars <- c(
    design@variables$ids,
    design@variables$weights,
    design@variables$strata,
    design@variables$fpc
  )
  design_vars <- design_vars[!is.null(design_vars)]
  for (rv in resp_vars) {
    if (rv %in% design_vars) {
      role <- if (rv == design@variables$weights) {
        "weight"
      } else if (rv %in% design@variables$ids) {
        "id"
      } else if (rv == design@variables$strata) {
        "stratum"
      } else {
        "fpc"
      }
      cli::cli_warn(
        c(
          "!" = paste0(
            "Response variable {.field {rv}} is a design variable ",
            "({.field {role}}). Results may be misleading."
          )
        ),
        class = "surveycore_warning_response_is_design_var"
      )
    }
  }

  # Validate predictor variables exist in design@data
  pred_vars <- setdiff(all.vars(formula[[3L]]), resp_vars)
  missing_preds <- setdiff(pred_vars, names(design@data))
  if (length(missing_preds) > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "Predictor {.field {missing_preds[1L]}} not found ",
          "in survey data."
        ),
        "i" = paste0(
          "Available columns: ",
          "{.field {head(names(design@data), 10L)}}."
        )
      ),
      class = "surveycore_error_predictor_not_found"
    )
  }

  # ── Step 2: Apply domain ───────────────────────────────────────────────────
  domain_mask <- .apply_domain(design)

  # For twophase designs, restrict fitting to phase-2 (observed) rows only.
  # Phase-1-only rows have no outcome or predictor measurements.
  if (S7::S7_inherits(design, survey_twophase)) {
    subset_col <- design@variables$subset
    subset_mask <- design@data[[subset_col]]
    domain_mask <- domain_mask & subset_mask
  }

  fit_data <- design@data[domain_mask, , drop = FALSE]

  if (nrow(fit_data) == 0L) {
    cli::cli_abort(
      c(
        "x" = "Active domain contains no in-domain rows.",
        "i" = paste0(
          "Apply a less restrictive {.fn surveytidy::filter} before ",
          "calling {.fn survey_glm}."
        )
      ),
      class = "surveycore_error_empty_domain"
    )
  }

  # Row indices (in full design@data) that are in-domain
  domain_idx <- which(domain_mask)

  # ── Step 3: Check weights, apply na.action ─────────────────────────────────
  wt_all <- .get_glm_weights(design)
  wt_var <- if (S7::S7_inherits(design, survey_twophase)) {
    NULL # calibrated weight, no single column name
  } else {
    design@variables$weights
  }
  wt_fit <- wt_all[domain_idx]

  # Check for NA weights
  n_na_wt <- sum(is.na(wt_fit))
  if (n_na_wt > 0L) {
    msg_na_x <- if (S7::S7_inherits(design, survey_twophase)) {
      "Calibrated weights contain {n_na_wt} NA value(s)."
    } else {
      "Weight column {.field {wt_var}} contains {n_na_wt} NA value(s)."
    }
    cli::cli_abort(
      c(
        "x" = msg_na_x,
        "i" = paste0(
          "Survey weights must be fully observed. Remove rows with missing ",
          "weights or impute before calling {.fn survey_glm}."
        )
      ),
      class = "surveycore_error_na_weights"
    )
  }

  # Warn for non-positive weights
  # nocov start
  # Defensive: all S7 design validators already reject non-positive weights, so
  # this branch is unreachable via the public API in normal use. Kept as a
  # safety net for corrupted design objects.
  n_nonpos <- sum(wt_fit <= 0, na.rm = TRUE)
  if (n_nonpos > 0L) {
    msg_nonpos_x <- if (S7::S7_inherits(design, survey_twophase)) {
      "Calibrated weights contain {n_nonpos} non-positive value(s)."
    } else {
      paste0(
        "Weight column {.field {wt_var}} contains ",
        "{n_nonpos} non-positive value(s)."
      )
    }
    cli::cli_warn(
      c(
        "!" = msg_nonpos_x,
        "i" = paste0(
          "Zero-weight rows are excluded from fitting by {.fn stats::glm}. ",
          "Negative weights are statistically invalid."
        )
      ),
      class = "surveycore_warning_nonpositive_weights"
    )
  }
  # nocov end

  # Check for na.action = na.fail: pre-detect NAs and throw typed error
  na_action_fn <- match.fun(na.action)
  is_na_fail <- identical(na_action_fn, stats::na.fail)
  if (is_na_fail) {
    # Build model frame to find which variables have NAs
    model_vars <- unique(c(resp_vars, pred_vars))
    na_info <- vapply(
      model_vars,
      function(v) {
        col <- fit_data[[v]]
        n_miss <- sum(is.na(col))
        if (n_miss > 0L) paste0(v, " (", n_miss, " NA)") else NA_character_
      },
      character(1L)
    )
    na_info <- na_info[!is.na(na_info)]
    if (length(na_info) > 0L) {
      n_na_cols <- length(na_info)
      cli::cli_abort(
        c(
          "x" = paste0(
            "{n_na_cols} column{?s} in the model {?has/have} NA values ",
            "with {.arg na.action = na.fail}: {.field {na_info}}."
          ),
          "v" = paste0(
            "Set {.arg na.action = na.omit} to drop rows with NA, ",
            "or remove them manually before calling {.fn survey_glm}."
          )
        ),
        class = "surveycore_error_na_in_data"
      )
    }
  }

  # ── Step 4: Fit weighted GLM ───────────────────────────────────────────────
  fam <- family
  is_binomial <- identical(fam$family, "binomial") ||
    identical(fam$family, "quasibinomial")

  # Use do.call so wt_fit is passed as an already-evaluated numeric vector.
  # stats::glm() uses model.frame.default which evaluates the weights argument
  # in environment(formula), not in survey_glm()'s local frame. do.call()
  # embeds the vector value directly, bypassing the symbol-lookup failure.
  glm_args <- list(
    formula = formula,
    family = fam,
    data = fit_data,
    weights = wt_fit,
    na.action = na.action,
    start = start,
    etastart = etastart,
    mustart = mustart,
    control = do.call(stats::glm.control, control)
  )

  # suppressWarnings: binomial → suppress non-integer #successes;
  # quiet  → also suppress glm.fit convergence warnings from stats
  fit <- if (is_binomial || quiet) {
    suppressWarnings(do.call(stats::glm, glm_args))
  } else {
    do.call(stats::glm, glm_args)
  }

  # Convergence warning (suppressed when quiet = TRUE; status still in
  # @converged)
  if (!fit$converged && !quiet) {
    cli::cli_warn(
      c(
        "!" = "{.fn survey_glm} did not converge.",
        "i" = "Increase {.arg control$maxit} or simplify the model."
      ),
      class = "surveycore_warning_glm_convergence"
    )
  }

  # Singular / aliased model matrix
  if (any(is.na(stats::coef(fit)))) {
    cli::cli_abort(
      c(
        "x" = "Model matrix is singular.",
        "i" = "Check for perfect collinearity or empty factor levels."
      ),
      class = "surveycore_error_singular_model_matrix"
    )
  }

  # Perfect separation (binomial)
  if (is_binomial) {
    fv <- stats::fitted(fit)
    if (any(fv <= .Machine$double.eps | fv >= 1 - .Machine$double.eps)) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "Fitted probabilities are numerically 0 or 1. ",
            "Perfect or quasi-complete separation may have occurred."
          )
        ),
        class = "surveycore_warning_perfect_separation"
      )
    }
  }

  # ── Step 5: Compute design-based variance ─────────────────────────────────
  # Determine which rows of design@data were used in the fit (after na.action)
  # stats::glm() drops rows with NA via na.action; use the na.action attribute
  # on the model frame to find which rows were kept.
  mf <- stats::model.frame(fit)
  na_idx <- attr(mf, "na.action") # indices of dropped rows (in fit_data)

  # Translate to indices in full design@data
  if (!is.null(na_idx)) {
    used_in_fit <- domain_idx[-na_idx]
  } else {
    used_in_fit <- domain_idx
  }

  # Build full-length domain indicator for score zeroing
  domain_for_score <- logical(nrow(design@data))
  domain_for_score[used_in_fit] <- TRUE

  vcov_mat <- if (quiet) {
    withCallingHandlers(
      .glm_vcov_dispatch(
        fit,
        design,
        row_mask = used_in_fit,
        domain_mask = domain_for_score
      ),
      surveycore_warning_glm_convergence = function(w) {
        invokeRestart("muffleWarning")
      }
    )
  } else {
    .glm_vcov_dispatch(
      fit,
      design,
      row_mask = used_in_fit,
      domain_mask = domain_for_score
    )
  }

  # Name the vcov matrix
  coef_names <- names(stats::coef(fit))
  dimnames(vcov_mat) <- list(coef_names, coef_names)

  # ── Step 6: Check degf and assemble survey_glm_fit ────────────────────────
  degf_val <- .glm_degrees_of_freedom(design, n_coef = length(coef_names))

  survey_glm_fit(
    coefficients = stats::coef(fit),
    vcov = vcov_mat,
    fitted_values = as.numeric(stats::fitted(fit)),
    residuals = as.numeric(stats::residuals(fit, type = "working")),
    weights = if (!is.null(na_idx)) {
      as.numeric(wt_fit[-na_idx])
    } else {
      as.numeric(wt_fit)
    },
    design = design,
    degf = degf_val,
    family = as.list(fam),
    formula = formula,
    null_deviance = fit$null.deviance,
    deviance = fit$deviance,
    df_null = as.numeric(fit$df.null),
    df_residual = as.numeric(fit$df.residual),
    converged = isTRUE(fit$converged),
    call = match.call(),
    fit_ = fit,
    term_assign = as.integer(attr(stats::model.matrix(fit), "assign"))
  )
}
