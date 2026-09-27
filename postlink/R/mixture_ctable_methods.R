#' Print Method for Adjusted Contingency Tables
#'
#' @description
#' Prints the estimated contingency table (corrected for linkage error) and a summary
#' of the adjustment parameters used by the mixture model.
#'
#' @param x An object of class \code{ctableMixture}.
#' @param digits Integer; the number of significant digits to use when printing
#' numeric values. Defaults to 3.
#' @param ... Additional arguments passed to \code{\link{print.default}}.
#'
#' @return The argument \code{x}, invisibly.
#'
#' @examples
#' set.seed(125)
#' n <- 300
#'
#' # 1. Simulate true categorical data with dependency
#' exposure <- sample(c("low", "high"), n, replace = TRUE)
#'
#' # Induce dependency - High exposure -> higher disease probability
#' prob_disease <- ifelse(exposure == "high", 0.7, 0.3)
#' true_disease <- ifelse(runif(n) < prob_disease, "yes", "no")
#'
#' # 2. Induce 15% linkage error
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' obs_disease <- true_disease
#' obs_disease[mis_idx] <- sample(obs_disease[mis_idx])
#'
#' linked_df <- data.frame(exposure = exposure, disease = obs_disease)
#'
#' # 3. Fit the adjusted contingency table model
#' adj <- adjMixture(linked.data = linked_df, m.rate = 0.15)
#' fit <- plctable(~ exposure + disease, adjustment = adj)
#'
#' # 4. Explicitly call the print method
#' print(fit)
#'
#' @seealso \code{\link{plctable}}, \code{\link{ctableMixture}}
#' @export
print.ctableMixture <- function(x, digits = 3, ...) {
 cat("\nCall:\n")
 dput(x$call)

 cat("\n--- Adjusted Contingency Table (Estimated Correct Counts) ---\n")
 # Print the effective counts (ftable) rounded for readability
 print(round(x$ftable, digits))

 cat("\n--- Linkage Error Adjustment ---\n")
 cat("Assumed Mismatch Rate (alpha):", format(x$adjustment$m.rate, digits = digits), "\n")

 if (isTRUE(x$converged)) {
  cat("Status: Converged in", length(x$objective), "iterations.\n")
 } else {
  cat("Status: Not converged (reached max.iter).\n")
 }

 invisible(x)
}

#' Extract Variance-Covariance Matrix from ctableMixture Objects
#'
#' @description
#' Extracts the estimated variance-covariance matrix of the cell probabilities
#' from a fitted \code{ctableMixture} object. The variance is estimated using
#' the observed information matrix (via the Hessian of the mixture log-likelihood).
#'
#' @param object An object of class \code{ctableMixture}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A matrix of the estimated covariances between the cell probability estimates.
#' The row and column names correspond to the cells of the table in row-major order
#' (e.g., "(Row1, Col1)", "(Row1, Col2)", ...).
#'
#' @examples
#' set.seed(125)
#' n <- 300
#'
#' # 1. Simulate true categorical data with dependency
#' exposure <- sample(c("low", "high"), n, replace = TRUE)
#'
#' # Induce dependency - High exposure -> higher disease probability
#' prob_disease <- ifelse(exposure == "high", 0.7, 0.3)
#' true_disease <- ifelse(runif(n) < prob_disease, "yes", "no")
#'
#' # 2. Induce 15% linkage error
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' obs_disease <- true_disease
#' obs_disease[mis_idx] <- sample(obs_disease[mis_idx])
#'
#' linked_df <- data.frame(exposure = exposure, disease = obs_disease)
#'
#' # 3. Fit the adjusted contingency table model
#' adj <- adjMixture(linked.data = linked_df, m.rate = 0.15)
#' fit <- plctable(~ exposure + disease, adjustment = adj)
#'
#' # 4. Extract the variance-covariance matrix of the cell probabilities
#' vmat <- vcov(fit)
#' print(vmat)
#'
#' @export
vcov.ctableMixture <- function(object, ...) {
 return(object$var)
}

#' Confidence Intervals for Adjusted Cell Probabilities
#'
#' @description
#' Computes Wald-type confidence intervals for the estimated cell probabilities
#' of the correctly matched population.
#'
#' @param object An object of class \code{ctableMixture}.
#' @param parm A specification of which parameters are to be given confidence intervals.
#' If missing, all parameters (cells) are considered.
#' @param level The confidence level required. Defaults to 0.95.
#' @param ... Additional arguments (currently ignored).
#'
#' @details
#' The intervals are calculated using the standard error estimates derived from
#' \code{\link{vcov.ctableMixture}}. The lower and upper bounds are truncated
#' at 0 and 1, respectively, to ensure valid probability estimates.
#'
#' @return A matrix with columns giving lower and upper confidence limits for each parameter.
#'
#' @examples
#' set.seed(125)
#' n <- 300
#'
#' # 1. Simulate true categorical data with dependency
#' exposure <- sample(c("low", "high"), n, replace = TRUE)
#'
#' # Induce dependency - High exposure -> higher disease probability
#' prob_disease <- ifelse(exposure == "high", 0.7, 0.3)
#' true_disease <- ifelse(runif(n) < prob_disease, "yes", "no")
#'
#' # 2. Induce 15% linkage error
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' obs_disease <- true_disease
#' obs_disease[mis_idx] <- sample(obs_disease[mis_idx])
#'
#' linked_df <- data.frame(exposure = exposure, disease = obs_disease)
#'
#' # 3. Fit the adjusted contingency table model
#' adj <- adjMixture(linked.data = linked_df, m.rate = 0.15)
#' fit <- plctable(~ exposure + disease, adjustment = adj)
#'
#' # 4. Compute confidence intervals
#' # 95% CI for all cell probabilities
#' confint(fit)
#'
#' # 90% CI for specific cells by name
#' confint(fit, parm = c("(low, yes)", "(high, no)"), level = 0.90)
#'
#' @export
confint.ctableMixture <- function(object, parm, level = 0.95, ...) {

 # Extract Estimates and SEs
 # Flatten phat to match the order of vcov (Row-Major)
 est_vec <- c(t(object$phat))
 se_vec  <- sqrt(diag(object$var))

 # Calculate Critical Value
 alpha_level <- (1 - level) / 2
 z_crit <- stats::qnorm(1 - alpha_level)

 # Compute Wald Intervals
 lower <- est_vec - z_crit * se_vec
 upper <- est_vec + z_crit * se_vec

 # Truncate at [0, 1] as probabilities cannot exceed these bounds
 lower <- pmax(0, lower)
 upper <- pmin(1, upper)

 # Format Output
 ci_mat <- cbind(lower, upper)
 pct_labs <- paste(format(100 * c(alpha_level, 1 - alpha_level), trim = TRUE,
                          scientific = FALSE, digits = 3), "%")
 colnames(ci_mat) <- pct_labs
 rownames(ci_mat) <- colnames(object$var) # Use the (Row, Col) labels

 # Subset if 'parm' is provided
 if (!missing(parm)) {
  if (is.character(parm)) {
   ci_mat <- ci_mat[parm, , drop = FALSE]
  } else if (is.numeric(parm)) {
   ci_mat <- ci_mat[parm, , drop = FALSE]
  }
 }

 return(ci_mat)
}

#' Summary Method for Adjusted Contingency Tables
#'
#' @description
#' Provides a detailed summary of the \code{ctableMixture} model fit, including
#' the estimated cell probabilities with standard errors, convergence status,
#' and a Chi-squared test of independence performed on the adjusted counts.
#'
#' @param object An object of class \code{ctableMixture}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An object of class \code{summary.ctableMixture} containing:
#' \item{call}{The function call.}
#' \item{m.rate}{The assumed mismatch rate.}
#' \item{ftable}{The estimated contingency table of correctly matched counts.}
#' \item{coefficients}{A matrix containing estimates, standard errors, z-values, and p-values for cell probabilities.}
#' \item{chisq}{The result of a Pearson's Chi-squared test on the adjusted table.}
#' \item{converged}{Logical indicating if the EM algorithm converged.}
#' \item{iterations}{Number of iterations performed.}
#'
#' @examples
#' set.seed(125)
#' n <- 300
#'
#' # 1. Simulate true categorical data with dependency
#' exposure <- sample(c("low", "high"), n, replace = TRUE)
#'
#' # Induce dependency - High exposure -> higher disease probability
#' prob_disease <- ifelse(exposure == "high", 0.7, 0.3)
#' true_disease <- ifelse(runif(n) < prob_disease, "yes", "no")
#'
#' # 2. Induce 15% linkage error
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' obs_disease <- true_disease
#' obs_disease[mis_idx] <- sample(obs_disease[mis_idx])
#'
#' linked_df <- data.frame(exposure = exposure, disease = obs_disease)
#'
#' # 3. Fit the adjusted contingency table model
#' adj <- adjMixture(linked.data = linked_df, m.rate = 0.15)
#' fit <- plctable(~ exposure + disease, adjustment = adj)
#'
#' # 4. Generate the detailed summary object
#' sum_fit <- summary(fit)
#'
#' # 5. Access specific components of the summary
#' print(sum_fit$coefficients)
#' print(sum_fit$chisq)
#'
#' @export
summary.ctableMixture <- function(object, ...) {

 # Parameter Summary (Cell Probabilities)
 # Extract vectorized estimates and standard errors
 est_vec <- c(t(object$phat))
 se_vec  <- sqrt(diag(object$var))

 # Construct coefficient matrix
 coef_mat <- cbind(
  Estimate = est_vec,
  `Std. Error` = se_vec,
  `z value` = est_vec / se_vec,
  `Pr(>|z|)` = 2 * (1 - stats::pnorm(abs(est_vec / se_vec)))
 )
 rownames(coef_mat) <- colnames(object$var)

 # Independence Test on Adjusted Data
 # We perform a Chi-squared test on the estimated correct counts (ftable).
 # note: This treats estimated counts as observed for the sake of the test statistic.
 # The p-value is approximate as it does not account for the variance of the
 # adjustment process itself, but it provides the corrected association metric.
 chisq_res <- suppressWarnings(stats::chisq.test(object$ftable))

 # Aggregate Results
 res <- list(
  call = object$call,
  m.rate = object$adjustment$m.rate,
  ftable = object$ftable,
  coefficients = coef_mat,
  chisq = chisq_res,
  converged = object$converged,
  iterations = length(object$objective)
 )

 class(res) <- "summary.ctableMixture"
 return(res)
}

#' @noRd
#' @export
print.summary.ctableMixture <- function(x, digits = 3, ...) {

 cat("\nCall:\n")
 dput(x$call)

 cat("\n")
 cat("Adjustment for Linkage Error (Mixture Model)\n")
 cat("------------------------------------------------------------\n")
 cat("Assumed Mismatch Rate:", format(x$m.rate, digits = digits), "\n")
 cat("EM Convergence:", x$converged, paste0("(", x$iterations, " iter)"), "\n")

 cat("\n")
 cat("Estimated Cell Probabilities (Correctly Matched Population)\n")
 cat("------------------------------------------------------------\n")
 stats::printCoefmat(x$coefficients, digits = digits, P.values = TRUE,
                     has.Pvalue = TRUE, signif.stars = TRUE)

 cat("\n")
 cat("Inference for Independence (Based on Adjusted Table)\n")
 cat("------------------------------------------------------------\n")
 cat("Pearson's Chi-squared Statistic\n")
 cat("X-squared =", format(x$chisq$statistic, digits = digits),
     ", df =", x$chisq$parameter, "\n")
 cat("note: P-value is not available. The standard Chi-squared distribution \n")
 cat("does not account for uncertainty in the linkage error adjustment.\n")

 cat("\nAdjusted Counts (Rounded):\n")
 print(round(x$ftable, 1))

 invisible(x)
}
