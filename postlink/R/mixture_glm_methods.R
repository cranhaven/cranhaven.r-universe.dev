#' Extract Variance-Covariance Matrix from a glmMixture Object
#'
#' Returns the variance-covariance matrix of the main parameters of a fitted
#' \code{glmMixture} object. The matrix is estimated using a sandwich estimator
#' to account for the mixture structure.
#'
#' @param object An object of class \code{glmMixture}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A matrix of the estimated covariances between the parameter estimates.
#' Row and column names correspond to the parameter names (coefficients, dispersion, etc.).
#'
#' @examples
#' # Load the LIFE-M demo dataset
#' data(lifem)
#'
#' # Phase 1: Adjustment Specification
#' # We model the correct match indicator via logistic regression using
#' # name commonness scores (commf, comml) and a 5% expected mismatch rate.
#' adj_object <- adjMixture(
#'  linked.data = lifem,
#'  m.formula = ~ commf + comml,
#'  m.rate = 0.05,
#'  safe.matches = hndlnk
#' )
#'
#' # Phase 2: Estimation & Inference
#' # Fit a Gaussian regression model utilizing a cubic polynomial for year of birth.
#' fit <- plglm(
#'  age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'  family = "gaussian",
#'  adjustment = adj_object
#' )
#'
#' vcov(fit)
#'
#' @export
vcov.glmMixture <- function(object, ...) {
 return(object$var)
}

#' Confidence Intervals for glmMixture Objects
#'
#' Computes Wald confidence intervals for one or more parameters in a \code{glmMixture} object.
#'
#' @param object An object of class \code{glmMixture}.
#' @param parm A specification of which parameters are to be given confidence intervals,
#' either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level The confidence level required.
#' @param ... Additional arguments (currently ignored).
#'
#' @details
#' The intervals are calculated based on the sandwich variance estimator:
#' \code{Estimate +/- z_crit * SE}.
#' For Gaussian and Gamma families, a t-distribution is used with residual degrees of freedom.
#' For Binomial and Poisson families, a standard normal distribution is used.
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence limits for each parameter.
#'
#' @examples
#' # Load the LIFE-M demo dataset
#' data(lifem)
#'
#' # Phase 1: Adjustment Specification
#' # We model the correct match indicator via logistic regression using
#' # name commonness scores (commf, comml) and a 5% expected mismatch rate.
#' adj_object <- adjMixture(
#'  linked.data = lifem,
#'  m.formula = ~ commf + comml,
#'  m.rate = 0.05,
#'  safe.matches = hndlnk
#' )
#'
#' # Phase 2: Estimation & Inference
#' # Fit a Gaussian regression model utilizing a cubic polynomial for year of birth.
#' fit <- plglm(
#'  age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'  family = "gaussian",
#'  adjustment = adj_object
#' )
#'
#' confint(fit)
#'
#' @export
confint.glmMixture <- function(object, parm, level = 0.95, ...) {
 cf <- c(object$coefficients, object$m.coefficients)

 # If dispersion is in the covariance matrix (Gaussian/Gamma), include it in the vector for alignment
 vc <- object$var
 all_names <- rownames(vc)

 # Construct a full parameter vector matching the vcov matrix structure
 # We extract estimates directly from the vcov names to ensure alignment
 # (Since object$coefficients and m.coefficients are separated)

 # Identify indices
 p <- length(object$coefficients)
 beta_idx <- 1:p

 # Check for dispersion/shape in vcov
 has_dispersion <- object$family$family %in% c("gaussian", "Gamma")
 disp_idx <- if (has_dispersion) p + 1 else integer(0)

 # Gamma indices
 m_coef_len <- length(object$m.coefficients)
 gamma_start <- if (has_dispersion) p + 2 else p + 1
 gamma_idx <- if (m_coef_len > 0) seq(gamma_start, length.out = m_coef_len) else integer(0)

 # Flatten coefficients to match vcov order
 est_vec <- numeric(nrow(vc))
 names(est_vec) <- rownames(vc)

 est_vec[beta_idx] <- object$coefficients
 if (has_dispersion) est_vec[disp_idx] <- object$dispersion
 if (m_coef_len > 0) est_vec[gamma_idx] <- object$m.coefficients

 pnames <- names(est_vec)

 if (missing(parm)) {
  parm <- pnames
 } else if (is.numeric(parm)) {
  parm <- pnames[parm]
 }

 # Select critical value
 a <- (1 - level) / 2
 a <- c(a, 1 - a)

 if (object$family$family %in% c("gaussian", "Gamma")) {
  crit <- stats::qt(1 - (1 - level) / 2, df = object$df.residual)
 } else {
  crit <- stats::qnorm(1 - (1 - level) / 2)
 }

 pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
 ci <- array(NA, dim = c(length(parm), 2), dimnames = list(parm, pct))

 ses <- sqrt(diag(vc))

 ci[] <- est_vec[parm] + ses[parm] %o% c(-crit, crit)

 return(ci)
}

#' Model Predictions for glmMixture Objects
#'
#' Obtains predictions and optionally estimates standard errors of those predictions
#' from a fitted \code{glmMixture} object.
#'
#' @param object An object of class \code{glmMixture}.
#' @param newdata An optional data frame in which to look for variables with which to predict.
#' If omitted, the fitted linear predictors are used.
#' @param type The type of prediction required. The default is on the scale of the linear predictors;
#' the alternative "response" is on the scale of the response variable.
#' @param se.fit Logical switch indicating if standard errors are required.
#' @param interval Type of interval calculation ("none" or "confidence").
#' @param level Confidence level.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict NA.
#' @param ... Additional arguments (currently ignored).
#'
#' @details
#' If \code{newdata} is omitted, the predictions are based on the data used for the fit.
#' In that case, \code{type = "link"} corresponds to \code{object$linear.predictors} and
#' \code{type = "response"} corresponds to \code{object$fitted.values}. If \code{newdata} is supplied, the function manually constructs the design matrix
#' from the terms object stored in the model. Standard errors are computed using the
#' sandwich covariance matrix (\code{object$var}).
#'
#' @return If \code{se.fit = FALSE}, a vector of predictions.
#' If \code{se.fit = TRUE}, a list with components:
#' \item{fit}{Predictions.}
#' \item{se.fit}{Estimated standard errors.}
#' \item{residual.scale}{A scalar giving the square root of the dispersion used in computing the standard errors.}
#' @examples
#' # Load the LIFE-M demo dataset
#' data(lifem)
#'
#' # Phase 1: Adjustment Specification
#' # We model the correct match indicator via logistic regression using
#' # name commonness scores (commf, comml) and a 5% expected mismatch rate.
#' adj_object <- adjMixture(
#'  linked.data = lifem,
#'  m.formula = ~ commf + comml,
#'  m.rate = 0.05,
#'  safe.matches = hndlnk
#' )
#'
#' # Phase 2: Estimation & Inference
#' # Fit a Gaussian regression model utilizing a cubic polynomial for year of birth.
#' fit <- plglm(
#'  age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'  family = "gaussian",
#'  adjustment = adj_object
#' )
#'
#' predict(fit)
#'
#' @export
predict.glmMixture <- function(object, newdata = NULL,
                               type = c("link", "response"),
                               se.fit = FALSE,
                               interval = c("none", "confidence"),
                               level = 0.95,
                               na.action = stats::na.pass,
                               ...) {
 type <- match.arg(type)
 interval <- match.arg(interval)
 compute_se <- se.fit || interval == "confidence"

 # Extract terms
 if (!is.null(object$terms)) {
  tt <- object$terms
 } else if (!is.null(object$model)) {
  tt <- attr(object$model, "terms")
 } else {
  stop("Model terms not found. Please refit the model with 'model = TRUE'.")
 }

 # Setup Design Matrix (X) & Linear Predictor (eta)
 if (is.null(newdata)) {
  eta <- object$linear.predictors

  if (compute_se) {
   X <- object$x
   if (is.null(X)) {
    if (!is.null(object$model)) {
     X <- stats::model.matrix(tt, object$model)
    } else {
     stop("Original model matrix not found. Please refit with 'x = TRUE' or provide 'newdata'.")
    }
   }
  }
 } else {
  # Remove response variable from terms for new data
  tt <- stats::delete.response(tt)
  m <- stats::model.frame(tt, data = newdata, na.action = na.action)
  X <- stats::model.matrix(tt, m)

  beta <- object$coefficients
  if (ncol(X) != length(beta)) stop("Dimension mismatch: 'newdata' design matrix does not match coefficients.")
  eta <- as.vector(X %*% beta)
 }

 # Base Predictions
 pred <- switch(type,
                link = eta,
                response = object$family$linkinv(eta))

 # Standard Errors & Intervals
 if (compute_se) {
  # Extract variance submatrix for Beta only
  p <- length(object$coefficients)
  cov_beta <- object$var[1:p, 1:p, drop = FALSE]

  # SE on the link scale: sqrt(diag(X %*% Sigma %*% t(X)))
  se_link <- sqrt(rowSums((X %*% cov_beta) * X))

  # SE on the requested output scale (using delta method if response)
  if (type == "response") {
   se_output <- se_link * abs(object$family$mu.eta(eta))
  } else {
   se_output <- se_link
  }

  # Calculate Confidence Intervals
  if (interval == "confidence") {
   a <- (1 - level) / 2
   if (object$family$family %in% c("gaussian", "Gamma")) {
    crit <- stats::qt(1 - a, df = object$df.residual)
   } else {
    crit <- stats::qnorm(1 - a)
   }

   # Calculate intervals on the link scale first
   lwr_link <- eta - crit * se_link
   upr_link <- eta + crit * se_link

   # Transform bounds to response scale if requested
   if (type == "response") {
    lwr <- object$family$linkinv(lwr_link)
    upr <- object$family$linkinv(upr_link)
   } else {
    lwr <- lwr_link
    upr <- upr_link
   }

   fit_matrix <- cbind(fit = pred, lwr = lwr, upr = upr)

   if (se.fit) {
    return(list(fit = fit_matrix,
                se.fit = se_output,
                residual.scale = sqrt(object$dispersion)))
   } else {
    return(fit_matrix)
   }
  }

  # Return just predictions and SEs
  return(list(fit = pred,
              se.fit = se_output,
              residual.scale = sqrt(object$dispersion)))
 }

 # Return basic predictions
 return(pred)
}

#' Print a glmMixture Object
#'
#' @param x An object of class \code{glmMixture}.
#' @param digits The number of significant digits to use.
#' @param ... Additional arguments.
#' @examples
#' # Load the LIFE-M demo dataset
#' data(lifem)
#'
#' # Phase 1: Adjustment Specification
#' # We model the correct match indicator via logistic regression using
#' # name commonness scores (commf, comml) and a 5% expected mismatch rate.
#' adj_object <- adjMixture(
#'  linked.data = lifem,
#'  m.formula = ~ commf + comml,
#'  m.rate = 0.05,
#'  safe.matches = hndlnk
#' )
#'
#' # Phase 2: Estimation & Inference
#' # Fit a Gaussian regression model utilizing a cubic polynomial for year of birth.
#' fit <- plglm(
#'  age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'  family = "gaussian",
#'  adjustment = adj_object
#' )
#'
#' print(fit)
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.glmMixture <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {

 cat("\nCall:  ", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
 cat("Coefficients (Outcome Model):\n")
 print.default(format(x$coefficients, digits = digits), print.gap = 2L, quote = FALSE)
 if (length(x$m.coefficients) > 0) {
  cat("\nCoefficients (Mismatch Model):\n")
  print.default(format(x$m.coefficients, digits = digits), print.gap = 2L, quote = FALSE)
 }
 cat("\n")
 if (!is.null(x$dispersion) && x$family$family %in% c("gaussian", "Gamma")) {
  cat("Dispersion parameter estimate: ", format(signif(x$dispersion, digits)), "\n")
 }
 cat("\n")
 invisible(x)
}

#' Summarizing GLM Mixture Fits
#'
#' \code{summary} method for class \code{glmMixture}.
#'
#' @param object An object of class \code{glmMixture}.
#' @param dispersion The dispersion parameter for the family used. If NULL, it is inferred from object.
#' @param ... Additional arguments.
#'
#' @return An object of class \code{summary.glmMixture} containing:
#' \item{call}{The component from object.}
#' \item{family}{The component from object.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{coefficients}{Matrix of coefficients for the outcome model.}
#' \item{m.coefficients}{Matrix of coefficients for the mismatch model.}
#' \item{dispersion}{Estimated dispersion parameter.}
#' \item{cov.unscaled}{The estimated covariance matrix.}
#' \item{match.prob}{The posterior match probabilities.}
#' @examples
#' # Load the LIFE-M demo dataset
#' data(lifem)
#'
#' # Phase 1: Adjustment Specification
#' # We model the correct match indicator via logistic regression using
#' # name commonness scores (commf, comml) and a 5% expected mismatch rate.
#' adj_object <- adjMixture(
#'  linked.data = lifem,
#'  m.formula = ~ commf + comml,
#'  m.rate = 0.05,
#'  safe.matches = hndlnk
#' )
#'
#' # Phase 2: Estimation & Inference
#' # Fit a Gaussian regression model utilizing a cubic polynomial for year of birth.
#' fit <- plglm(
#'  age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'  family = "gaussian",
#'  adjustment = adj_object
#' )
#'
#' summary(fit)
#'
#' @export
summary.glmMixture <- function(object, dispersion = NULL, ...) {

 # Setup
 p <- length(object$coefficients)
 m_p <- length(object$m.coefficients)

 # Calculate Standard Errors
 vc <- object$var
 std_errs <- sqrt(diag(vc))

 # Outcome Model Table
 beta_se <- std_errs[1:p]
 beta_est <- object$coefficients

 if (object$family$family %in% c("gaussian", "Gamma")) {
  t_val <- beta_est / beta_se
  p_val <- 2 * stats::pt(-abs(t_val), df = object$df.residual)
  coef_mat <- cbind(Estimate = beta_est,
                    `Std. Error` = beta_se,
                    `t value` = t_val,
                    `Pr(>|t|)` = p_val)
 } else {
  z_val <- beta_est / beta_se
  p_val <- 2 * stats::pnorm(-abs(z_val))
  coef_mat <- cbind(Estimate = beta_est,
                    `Std. Error` = beta_se,
                    `z value` = z_val,
                    `Pr(>|z|)` = p_val)
 }
 rownames(coef_mat) <- names(object$coefficients)

 # Mismatch Model Table
 m_coef_mat <- NULL
 if (m_p > 0) {
  # Locate mismatch SEs in the vector
  # Usually at the end of the vcov diagonal
  # Offset = p (beta) + (1 if dispersion estimated)
  has_disp <- object$family$family %in% c("gaussian", "Gamma")
  offset <- p + (if (has_disp) 1 else 0)

  m_est <- object$m.coefficients
  m_se <- std_errs[(offset + 1):(offset + m_p)]

  z_val_m <- m_est / m_se
  p_val_m <- 2 * stats::pnorm(-abs(z_val_m)) # Mismatch model usually asymptotic/Wald

  m_coef_mat <- cbind(m_est, m_se, z_val_m, p_val_m)
  rownames(m_coef_mat) <- names(object$m.coefficients)
  colnames(m_coef_mat) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
 }

 # Dispersion
 if (is.null(dispersion)) {
  dispersion <- object$dispersion
 }

 res <- list(call = object$call,
             family = object$family,
             df.residual = object$df.residual,
             coefficients = coef_mat,
             m.coefficients = m_coef_mat,
             dispersion = dispersion,
             cov.unscaled = object$var,
             match.prob = object$match.prob)

 class(res) <- "summary.glmMixture"
 return(res)
}

#' @noRd
#' @export
print.summary.glmMixture <- function(x, digits = max(3L, getOption("digits") - 3L),
                                     signif.stars = getOption("show.signif.stars"), ...) {

 cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")

 cat("\nOutcome Model Coefficients:\n")
 stats::printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
                     signif.legend = FALSE, ...)

 if (!is.null(x$m.coefficients)) {
  cat("\nMismatch Model Coefficients:\n")
  stats::printCoefmat(x$m.coefficients, digits = digits, signif.stars = signif.stars,
                      na.print = "NA", ...)
 }

 if (x$family$family %in% c("gaussian", "Gamma")) {
  cat("\n(Dispersion parameter for ", x$family$family, " family taken to be ",
      format(x$dispersion, digits = digits), ")\n\n", sep = "")
 } else {
  cat("\n(Dispersion parameter for ", x$family$family, " family taken to be 1)\n\n", sep = "")
 }

 # Average Posterior Match Probability
 if (!is.null(x$match.prob)) {
  cat("Average Correct Match Probability:", format(mean(x$match.prob), digits = digits), "\n")
 }

 cat("\n")
 invisible(x)
}
