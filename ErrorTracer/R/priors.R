# R/priors.R — extract_priors(): regularized or standard regression model → 
# brms prior specification

# ******************************************************************************
# S3 generic
# ______________________________________________________________________________

#' Extract brms prior specification from a regularized or standard regression model
#'
#' Converts a fitted model object into a \code{brms} prior specification
#' suitable for \code{\link{et_fit}}.  The coefficient estimates (or
#' importance weights for \pkg{ranger}) from the regularized or standard fit are used as
#' informative prior means, so the Bayesian model starts close to the
#' regularized or standard solution while remaining open to data-driven revision.
#'
#' @param model A fitted model.  Supported classes:
#'   \itemize{
#'     \item \code{cv.glmnet} / \code{glmnet} — elastic net / lasso
#'     \item \code{lm} — ordinary least squares
#'     \item \code{glm} — generalized linear model
#'     \item \code{ranger} — random forest (importance-scaled flat priors)
#'   }
#' @param multiplier Numeric scalar.  Prior SD is set to
#'   \code{multiplier * |coef|} (for signed-coefficient methods) or
#'   \code{multiplier * importance_normalised} (for ranger).  Default 2.0.
#' @param min_sd Numeric scalar.  Minimum prior SD to avoid degenerate
#'   (spike) priors on near-zero coefficients.  Default 0.1.
#' @param intercept_prior_sd Optional prior SD for the intercept term.  The
#'   default \code{NULL} emits \emph{no} explicit intercept prior and lets
#'   \code{brms} pick its data-aware default
#'   (\code{student_t(3, median(y), mad(y))}).  If you supply a numeric value,
#'   the prior becomes \code{normal(0, intercept_prior_sd)} --- this is only
#'   appropriate when the response has been centred (or nearly so) before
#'   fitting, since \code{brms}'s \code{class = "Intercept"} refers to the
#'   intercept at the centre of the predictors, whose posterior mean equals
#'   \code{E[y]} there.  Supplying a small \code{intercept_prior_sd} for an
#'   uncentred response will pull the intercept toward zero and cripple the
#'   fit.
#' @param sigma_prior_scale Scale parameter for the half-Cauchy prior on the
#'   residual SD sigma.  Default 1.0.
#' @param ... Additional arguments passed to methods.
#' @return An \code{et_prior_spec} list containing:
#'   \describe{
#'     \item{\code{prior}}{A \code{brmsprior} object for use in \code{brms::brm()}.}
#'     \item{\code{pred_names}}{Character vector of included predictor names.}
#'     \item{\code{coefs}}{Named numeric vector of regularized coefficients
#'       (NULL for ranger, which uses importance instead).}
#'     \item{\code{method}}{Character: the dispatch method used.}
#'     \item{\code{multiplier}, \code{min_sd}}{Settings echo.}
#'   }
#' @examples
#' fit_lm <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#' ps <- extract_priors(fit_lm, multiplier = 2, min_sd = 0.1)
#' print(ps)
#' @export
extract_priors <- function(model, multiplier = 2.0, min_sd = 0.1,
                           intercept_prior_sd = NULL, sigma_prior_scale = 1.0,
                           ...) {
  UseMethod("extract_priors")
}

# ******************************************************************************
# Method: lm
# ______________________________________________________________________________

#' @rdname extract_priors
#' @export
extract_priors.lm <- function(model, multiplier = 2.0, min_sd = 0.1,
                               intercept_prior_sd = NULL, sigma_prior_scale = 1.0,
                               ...) {
  coef_summary <- summary(model)$coefficients
  # Drop the intercept row — handled separately
  coef_rows <- coef_summary[rownames(coef_summary) != "(Intercept)", , drop = FALSE]

  if (nrow(coef_rows) == 0) stop("No non-intercept predictors found in lm model.")

  pred_names <- rownames(coef_rows)
  coef_ests <- coef_rows[, "Estimate"]
  coef_ses <- coef_rows[, "Std. Error"]

  .build_prior_spec(
    pred_names = pred_names,
    prior_means = coef_ests,
    prior_sds = pmax(multiplier * coef_ses, min_sd),
    coefs = stats::setNames(coef_ests, pred_names),
    method = "lm",
    multiplier = multiplier,
    min_sd = min_sd,
    intercept_prior_sd = intercept_prior_sd,
    sigma_prior_scale = sigma_prior_scale
  )
}

# ******************************************************************************
# Method: glm
# ______________________________________________________________________________

#' @rdname extract_priors
#' @export
extract_priors.glm <- function(model, multiplier = 2.0, min_sd = 0.1,
                                intercept_prior_sd = NULL, sigma_prior_scale = 1.0,
                                ...) {
  coef_summary <- summary(model)$coefficients
  coef_rows <- coef_summary[rownames(coef_summary) != "(Intercept)", , drop = FALSE]

  if (nrow(coef_rows) == 0) stop("No non-intercept predictors found in glm model.")

  pred_names <- rownames(coef_rows)
  coef_ests <- coef_rows[, "Estimate"]
  coef_ses <- coef_rows[, "Std. Error"]

  .build_prior_spec(
    pred_names = pred_names,
    prior_means = coef_ests,
    prior_sds = pmax(multiplier * coef_ses, min_sd),
    coefs = stats::setNames(coef_ests, pred_names),
    method = "glm",
    multiplier = multiplier,
    min_sd = min_sd,
    intercept_prior_sd = intercept_prior_sd,
    sigma_prior_scale = sigma_prior_scale
  )
}

# ******************************************************************************
# Method: cv.glmnet
# ______________________________________________________________________________

#' @rdname extract_priors
#' @param lambda Which lambda to extract coefficients from when the model is a
#'   \code{cv.glmnet} object.  Either \code{"lambda.min"} (default) or
#'   \code{"lambda.1se"}.
#' @export
extract_priors.cv.glmnet <- function(model, multiplier = 2.0, min_sd = 0.1,
                                      intercept_prior_sd = NULL,
                                      sigma_prior_scale = 1.0,
                                      lambda = "lambda.min", ...) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required for extract_priors.cv.glmnet().")
  }
  lam_val <- model[[lambda]]
  coef_mat <- glmnet::coef.glmnet(model, s = lam_val)
  .extract_glmnet_coefs(
    coef_mat  = coef_mat,
    multiplier = multiplier,
    min_sd = min_sd,
    intercept_prior_sd = intercept_prior_sd,
    sigma_prior_scale = sigma_prior_scale
  )
}

# ******************************************************************************
# Method: glmnet
# ______________________________________________________________________________

#' @rdname extract_priors
#' @param s Index (integer) or value of lambda to extract coefficients at when
#'   the model is a plain \code{glmnet} object.  Defaults to the first lambda
#'   (smallest regularisation).
#' @export
extract_priors.glmnet <- function(model, multiplier = 2.0, min_sd = 0.1,
                                   intercept_prior_sd = NULL,
                                   sigma_prior_scale = 1.0,
                                   s = 1L, ...) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required for extract_priors.glmnet().")
  }
  lam_val <- if (is.integer(s) || (is.numeric(s) && s == round(s))) {
    model$lambda[s]
  } else {
    s
  }
  coef_mat <- glmnet::coef.glmnet(model, s = lam_val)
  .extract_glmnet_coefs(
    coef_mat = coef_mat,
    multiplier = multiplier,
    min_sd = min_sd,
    intercept_prior_sd = intercept_prior_sd,
    sigma_prior_scale  = sigma_prior_scale
  )
}

# ******************************************************************************
# Method: ranger
# ______________________________________________________________________________

#' @rdname extract_priors
#' @details
#' For \code{ranger} models, signed coefficients are not available.  Priors
#' are centred at zero (direction unknown) and the prior SD for each predictor
#' is set to \code{multiplier * importance_normalised}, where importance is
#' normalised to the \code{[min_sd, 1]} interval.  Only variables with
#' positive permutation importance are included.
#' @export
extract_priors.ranger <- function(model, multiplier = 2.0, min_sd = 0.1,
                                   intercept_prior_sd = NULL,
                                   sigma_prior_scale = 1.0, ...) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("Package 'ranger' is required for extract_priors.ranger().")
  }
  imp <- model$variable.importance
  if (is.null(imp)) {
    stop("ranger model has no variable importance. ",
         "Refit with importance = 'permutation' or 'impurity'.")
  }

  # Keep only variables with positive importance
  imp <- imp[imp > 0]
  if (length(imp) == 0) stop("No variables with positive importance in ranger model.")

  pred_names <- names(imp)

  # Normalise importance to [0, 1] then scale
  imp_norm  <- imp / max(imp)
  prior_sds <- pmax(multiplier * imp_norm, min_sd)
  prior_means <- rep(0, length(pred_names))

  .build_prior_spec(
    pred_names = pred_names,
    prior_means = prior_means,
    prior_sds = prior_sds,
    coefs = NULL,          # no signed coefficients
    method = "ranger",
    multiplier = multiplier,
    min_sd = min_sd,
    intercept_prior_sd = intercept_prior_sd,
    sigma_prior_scale = sigma_prior_scale
  )
}

# ******************************************************************************
# Internal helpers
# ______________________________________________________________________________

# Shared code for cv.glmnet and glmnet coefficient extraction
.extract_glmnet_coefs <- function(coef_mat, multiplier, min_sd,
                                   intercept_prior_sd, sigma_prior_scale) {
  # coef_mat is a dgCMatrix with rows = (Intercept, pred1, pred2, ...)
  coef_vec <- as.numeric(coef_mat)
  names(coef_vec) <- rownames(coef_mat)

  # Drop intercept and zero coefficients
  coef_vec <- coef_vec[names(coef_vec) != "(Intercept)"]
  coef_nz <- coef_vec[coef_vec != 0]

  if (length(coef_nz) == 0) {
    stop("All glmnet coefficients are zero at the chosen lambda. ",
         "Try a smaller lambda (less regularisation).")
  }

  pred_names <- names(coef_nz)
  prior_means <- as.numeric(coef_nz)
  prior_sds <- pmax(multiplier * abs(coef_nz), min_sd)

  .build_prior_spec(
    pred_names = pred_names,
    prior_means = prior_means,
    prior_sds = prior_sds,
    coefs = stats::setNames(as.numeric(coef_nz), pred_names),
    method = "glmnet",
    multiplier = multiplier,
    min_sd = min_sd,
    intercept_prior_sd = intercept_prior_sd,
    sigma_prior_scale = sigma_prior_scale
  )
}

# Assemble a brms prior list and return an et_prior_spec object
.build_prior_spec <- function(pred_names, prior_means, prior_sds, coefs,
                               method, multiplier, min_sd,
                               intercept_prior_sd, sigma_prior_scale) {
  if (length(pred_names) == 0) stop("No predictors to build priors for.")

  prior_list <- vector("list", length(pred_names))
  for (j in seq_along(pred_names)) {
    prior_list[[j]] <- brms::set_prior(
      paste0("normal(", round(prior_means[j], 6), ", ", round(prior_sds[j], 6), ")"),
      class = "b",
      coef  = pred_names[j]
    )
  }

  # Intercept: only emit an explicit prior when the user supplied a scale.
  # A NULL default lets brms use its data-aware default
  # (student_t(3, median(y), mad(y))), which is robust across response scales.
  # A numeric value preserves the previous behaviour -- normal(0, sd) -- which
  # is only appropriate for a centred response; see extract_priors() docs.
  if (!is.null(intercept_prior_sd)) {
    prior_list <- c(prior_list, list(brms::set_prior(
      paste0("normal(0, ", round(intercept_prior_sd, 4), ")"),
      class = "Intercept"
    )))
  }

  # Sigma: half-Cauchy
  prior_list <- c(prior_list, list(brms::set_prior(
    paste0("cauchy(0, ", round(sigma_prior_scale, 4), ")"),
    class = "sigma"
  )))

  combined_prior <- do.call(c, prior_list)

  structure(
    list(
      prior = combined_prior,
      pred_names = pred_names,
      coefs = coefs,
      method = method,
      multiplier = multiplier,
      min_sd = min_sd,
      intercept_prior_sd = intercept_prior_sd,
      sigma_prior_scale = sigma_prior_scale
    ),
    class = "et_prior_spec"
  )
}

# ******************************************************************************
# S3 methods for et_prior_spec
# ______________________________________________________________________________

#' @export
print.et_prior_spec <- function(x, ...) {
  cat("ErrorTracer prior specification\n")
  cat("  Method      :", x$method, "\n")
  cat("  Predictors  :", length(x$pred_names), "\n")
  cat("  Multiplier  :", x$multiplier, "\n")
  cat("  Min SD      :", x$min_sd, "\n")
  if (!is.null(x$coefs)) {
    cat("  Coefficients:\n")
    for (j in seq_along(x$pred_names)) {
      cat(sprintf("    %-20s  mean = %8.4f  sd = %8.4f\n",
                  x$pred_names[j],
                  x$coefs[j],
                  max(x$multiplier * abs(x$coefs[j]), x$min_sd)))
    }
  } else {
    cat("  Predictors  :", paste(x$pred_names, collapse = ", "), "\n")
  }
  invisible(x)
}
