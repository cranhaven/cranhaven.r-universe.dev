#' Methods for Bayesian Mixture Survival Regression Fits
#'
#' @description
#' S3 methods for objects returned by \code{survregMixBayes()}, including
#' printing, summarizing fitted models, computing credible intervals and
#' posterior covariance matrices, generating predictions, and pooling Cox
#' regression fits across posterior match classifications.
#'
#' @name mixture_bayessurvreg_methods
#' @keywords internal
NULL

#' Print a survMixBayes Model Object
#'
#' Prints the model call and posterior mean regression coefficients for the
#' first mixture component of the fitted survival model. In this package,
#' component 1 is interpreted as the correct-match component and component 2
#' as the incorrect-match component.
#'
#' @param x An object of class \code{survMixBayes}.
#' @param digits Minimum number of significant digits to show.
#' @param ... Further arguments (unused).
#'
#' @return The input \code{x}, invisibly.
#'
#' @examples
#' \donttest{
#' set.seed(301)
#' n <- 150
#' trt <- rbinom(n, 1, 0.5)
#'
#' # Simulate Weibull AFT data
#' true_time <- rweibull(n, shape = 1.5, scale = exp(1 + 0.8 * trt))
#' cens_time <- rexp(n, rate = 0.1)
#' true_obs_time <- pmin(true_time, cens_time)
#' true_status <- as.integer(true_time <= cens_time)
#'
#' # Induce linkage mismatch errors in approximately 20% of records
#' is_mismatch <- rbinom(n, 1, 0.2)
#' obs_time <- true_obs_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' obs_status[mismatch_idx] <- obs_status[shuffled]
#'
#' linked_df <- data.frame(time = obs_time, status = obs_status, trt = trt)
#' adj <- adjMixBayes(linked.data = linked_df)
#'
#' fit <- plsurvreg(
#'   survival::Surv(time, status) ~ trt,
#'   dist = "weibull",
#'   adjustment = adj,
#'   control = list(iterations = 200, burnin.iterations = 100, seed = 123)
#' )
#'
#' print(fit)
#' }
#'
#' @export
#' @method print survMixBayes
print.survMixBayes <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("Bayesian two-component mixture survival regression\n")
  cat("Family:", x$dist, "\n\n")
  if (!is.null(x$call)) {
    cat("Call:\n")
    print(x$call)
    cat("\n")
  }
  b <- x$estimates$coefficients
  if (is.matrix(b)) {
    cat("Coefficients (posterior means, component 1 = correct-match):\n")
    print(round(colMeans(b), digits = digits))
  }
  invisible(x)
}

#' Summary Method for survMixBayes Models
#'
#' Computes posterior summaries for the regression coefficients, mixing weight,
#' and component-specific distribution parameters in a fitted
#' \code{survMixBayes} model. Throughout, component 1 is interpreted as the
#' correct-match component and component 2 as the incorrect-match component.
#'
#' @param object An object of class \code{survMixBayes}.
#' @param probs Numeric vector of probabilities used to compute posterior
#'   quantiles for the model parameters. The default,
#'   \code{c(0.025, 0.5, 0.975)}, gives a posterior median and a 95\%
#'   credible interval.
#' @param ... Further arguments (unused).
#'
#' @return An object of class \code{summary.survMixBayes} containing posterior
#'   quantile summaries for the regression coefficients in both mixture
#'   components, the mixing weight, and any family-specific distribution
#'   parameters included in the fitted model. Component 1 corresponds to the
#'   correct-match component and component 2 to the incorrect-match component.
#'
#' @examples
#' \donttest{
#' set.seed(301)
#' n <- 150
#' trt <- rbinom(n, 1, 0.5)
#'
#' # Simulate Weibull AFT data
#' true_time <- rweibull(n, shape = 1.5, scale = exp(1 + 0.8 * trt))
#' cens_time <- rexp(n, rate = 0.1)
#' true_obs_time <- pmin(true_time, cens_time)
#' true_status <- as.integer(true_time <= cens_time)
#'
#' # Induce linkage mismatch errors in approximately 20% of records
#' is_mismatch <- rbinom(n, 1, 0.2)
#' obs_time <- true_obs_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' obs_status[mismatch_idx] <- obs_status[shuffled]
#'
#' linked_df <- data.frame(time = obs_time, status = obs_status, trt = trt)
#' adj <- adjMixBayes(linked.data = linked_df)
#'
#' fit <- plsurvreg(
#'   survival::Surv(time, status) ~ trt,
#'   dist = "weibull",
#'   adjustment = adj,
#'   control = list(iterations = 200, burnin.iterations = 100, seed = 123)
#' )
#'
#' fit_summary <- summary(fit, probs = c(0.025, 0.5, 0.975))
#' print(fit_summary)
#' }
#'
#' @export
#' @method summary survMixBayes
summary.survMixBayes <- function(object, probs = c(0.025, 0.5, 0.975), ...) {
  qfun <- function(x) stats::quantile(x, probs = probs, na.rm = TRUE)
  b1 <- object$estimates$coefficients
  b2 <- object$estimates$m.coefficients
  if (!is.matrix(b1) || !is.matrix(b2)) stop("Expected coefficient draws as matrices.", call. = FALSE)

  s <- list(
    call = object$call,
    family = object$dist,
    coef1 = apply(b1, 2, qfun),
    coef2 = apply(b2, 2, qfun),
    theta = qfun(object$estimates$theta)
  )

  if (!is.null(object$estimates$shape))  s$shape1 <- qfun(object$estimates$shape)
  if (!is.null(object$estimates$m.shape)) s$shape2 <- qfun(object$estimates$m.shape)
  if (!is.null(object$estimates$scale))  s$scale1 <- qfun(object$estimates$scale)
  if (!is.null(object$estimates$m.scale)) s$scale2 <- qfun(object$estimates$m.scale)

  class(s) <- "summary.survMixBayes"
  s
}

#' @noRd
#' @export
print.summary.survMixBayes <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("Summary of Bayesian mixture survival regression\n")
  if (!is.null(x$call)) {
    cat("\nCall:\n")
    print(x$call)
  }
  cat("\nFamily:", x$family, "\n")

  fmt <- function(mat, nm) {
    if (is.null(mat)) return(invisible(NULL))
    cat("\n", nm, " (quantiles):\n", sep = "")
    print(round(t(mat), digits = digits))
  }

  fmt(x$coef1, "Coefficients (component 1 = correct-match)")
  fmt(x$coef2, "Coefficients (component 2 = incorrect-match)")

  cat("\nTheta (mix weight for component 1 = correct-match):\n")
  print(round(x$theta, digits = digits))

  fmt(x$shape1, "Shape (component 1 = correct-match)")
  fmt(x$shape2, "Shape (component 2 = incorrect-match)")
  fmt(x$scale1, "Scale (component 1 = correct-match)")
  fmt(x$scale2, "Scale (component 2 = incorrect-match)")

  invisible(x)
}

#' Credible Intervals for Parameters from a survMixBayes Fit
#'
#' Computes posterior credible intervals for the regression coefficients,
#' mixing weight, and family-specific distribution parameters from a fitted
#' \code{survMixBayes} model. The returned intervals are organized by mixture
#' component and parameter type. Component 1 corresponds to the correct-match
#' component and component 2 to the incorrect-match component.
#'
#' @param object An object of class \code{survMixBayes}.
#' @param parm Optional character vector selecting which interval blocks to
#'   return. For example, \code{"theta"} returns only the credible interval for
#'   the mixing weight. If \code{NULL}, credible intervals are returned for all
#'   available parameter blocks.
#' @param level Probability level for the credible intervals. Defaults to
#'   \code{0.95}.
#' @param ... Further arguments (unused).
#'
#' @return A named list of credible intervals. Elements \code{coef1} and
#'   \code{coef2} are matrices with one row per regression coefficient and two
#'   columns giving the lower and upper interval bounds for components 1 and 2,
#'   respectively, where component 1 is the correct-match component and
#'   component 2 is the incorrect-match component. Elements such as \code{theta}, \code{shape1},
#'   \code{shape2}, \code{scale1}, and \code{scale2} are numeric vectors of
#'   length 2 containing the lower and upper credible interval bounds for the
#'   corresponding scalar parameters.
#'
#' @examples
#' \donttest{
#' set.seed(301)
#' n <- 150
#' trt <- rbinom(n, 1, 0.5)
#'
#' # Simulate Weibull AFT data
#' true_time <- rweibull(n, shape = 1.5, scale = exp(1 + 0.8 * trt))
#' cens_time <- rexp(n, rate = 0.1)
#' true_obs_time <- pmin(true_time, cens_time)
#' true_status <- as.integer(true_time <= cens_time)
#'
#' # Induce linkage mismatch errors in approximately 20% of records
#' is_mismatch <- rbinom(n, 1, 0.2)
#' obs_time <- true_obs_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' obs_status[mismatch_idx] <- obs_status[shuffled]
#'
#' linked_df <- data.frame(time = obs_time, status = obs_status, trt = trt)
#'
#' adj <- adjMixBayes(linked.data = linked_df)
#'
#' fit <- plsurvreg(
#'   survival::Surv(time, status) ~ trt,
#'   dist = "weibull",
#'   adjustment = adj,
#'   control = list(iterations = 200, burnin.iterations = 100, seed = 123)
#' )
#'
#' # Calculate 95% credible intervals for all parameters
#' confint(fit, level = 0.95)
#'
#' # Extract credible intervals specifically for the mixing weight
#' confint(fit, parm = "theta", level = 0.90)
#' }
#'
#' @export
#' @method confint survMixBayes
confint.survMixBayes <- function(object, parm = NULL, level = 0.95, ...) {
 alpha <- (1 - level) / 2
 probs <- c(alpha, 1 - alpha)

 out <- list()

 b1 <- object$estimates$coefficients
 b2 <- object$estimates$m.coefficients
 if (is.matrix(b1)) out$coef1 <- t(apply(b1, 2, stats::quantile, probs = probs, na.rm = TRUE))
 if (is.matrix(b2)) out$coef2 <- t(apply(b2, 2, stats::quantile, probs = probs, na.rm = TRUE))

 out$theta <- stats::quantile(object$estimates$theta, probs = probs, na.rm = TRUE)

 if (!is.null(object$estimates$shape))   out$shape1 <- stats::quantile(object$estimates$shape,   probs = probs, na.rm = TRUE)
 if (!is.null(object$estimates$m.shape)) out$shape2 <- stats::quantile(object$estimates$m.shape, probs = probs, na.rm = TRUE)
 if (!is.null(object$estimates$scale))   out$scale1 <- stats::quantile(object$estimates$scale,   probs = probs, na.rm = TRUE)
 if (!is.null(object$estimates$m.scale)) out$scale2 <- stats::quantile(object$estimates$m.scale, probs = probs, na.rm = TRUE)

 # Optional filtering (simple and safe): allow selecting list elements by name
 if (!is.null(parm)) {
  if (is.character(parm)) out <- out[names(out) %in% parm]
 }

 out
}

#' Posterior Covariance Matrix for survMixBayes Coefficients
#'
#' Returns the empirical posterior covariance matrix of the regression
#' coefficients for component 1 of a fitted \code{survMixBayes} model.
#' In this package, component 1 is interpreted as the correct-match component.
#'
#' @param object A \code{survMixBayes} model object.
#' @param ... Further arguments (unused).
#'
#' @return Posterior covariance matrix of the regression coefficients for
#'   component 1, interpreted as the correct-match component.
#'
#' @examples
#' \donttest{
#' set.seed(301)
#' n <- 150
#' trt <- rbinom(n, 1, 0.5)
#'
#' # Simulate Weibull AFT data
#' true_time <- rweibull(n, shape = 1.5, scale = exp(1 + 0.8 * trt))
#' cens_time <- rexp(n, rate = 0.1)
#' true_obs_time <- pmin(true_time, cens_time)
#' true_status <- as.integer(true_time <= cens_time)
#'
#' # Induce linkage mismatch errors in approximately 20% of records
#' is_mismatch <- rbinom(n, 1, 0.2)
#' obs_time <- true_obs_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' obs_status[mismatch_idx] <- obs_status[shuffled]
#'
#' linked_df <- data.frame(time = obs_time, status = obs_status, trt = trt)
#'
#' adj <- adjMixBayes(linked.data = linked_df)
#'
#' fit <- plsurvreg(
#'   survival::Surv(time, status) ~ trt,
#'   dist = "weibull",
#'   adjustment = adj,
#'   control = list(iterations = 200, burnin.iterations = 100, seed = 123)
#' )
#'
#' # Extract the empirical posterior covariance matrix for component 1
#' vcov_mat <- vcov(fit)
#' print(vcov_mat)
#' }
#'
#' @export
#' @method vcov survMixBayes
vcov.survMixBayes <- function(object, ...) {
  b <- object$estimates$coefficients
  if (!is.matrix(b)) stop("Coefficient draws are not available.", call. = FALSE)
  stats::cov(b)
}

#' Predictions from a survMixBayes Model
#'
#' Computes posterior predictions for each latent component of a
#' \code{survMixBayes} model. By default, predictions are returned on the
#' linear predictor scale for both components.
#'
#' Component 1 is interpreted as the correct-match component and
#' component 2 as the incorrect-match component (after label-switching
#' correction).
#'
#' @param object A \code{survMixBayes} model object.
#' @param newdata A numeric matrix of new observations (\eqn{n_{new} \times K})
#'   with columns aligned to the design matrix used for fitting.
#'   If \code{NULL}, the fitted design matrix stored in \code{object$X} is used.
#' @param se.fit Logical; if \code{TRUE}, also return posterior SD of predictions.
#' @param interval Either \code{"none"} or \code{"credible"}, indicating whether
#'   to compute credible intervals.
#' @param level Probability level for the credible interval (default 0.95).
#' @param ... Not used.
#'
#' @return A list with two components, \code{component1} and \code{component2},
#'   corresponding to the two latent mixture components.
#'   If \code{se.fit = FALSE} and \code{interval = "none"}, each element is a
#'   numeric vector of posterior mean linear predictors.
#'   Otherwise, each element is a matrix containing the fitted values and,
#'   optionally, posterior SDs and credible interval bounds.
#'
#' @examples
#' \donttest{
#' set.seed(301)
#' n <- 150
#' trt <- rbinom(n, 1, 0.5)
#'
#' # Simulate Weibull AFT data
#' true_time <- rweibull(n, shape = 1.5, scale = exp(1 + 0.8 * trt))
#' cens_time <- rexp(n, rate = 0.1)
#' true_obs_time <- pmin(true_time, cens_time)
#' true_status <- as.integer(true_time <= cens_time)
#'
#' # Induce linkage mismatch errors in approximately 20% of records
#' is_mismatch <- rbinom(n, 1, 0.2)
#' obs_time <- true_obs_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' obs_status[mismatch_idx] <- obs_status[shuffled]
#'
#' linked_df <- data.frame(time = obs_time, status = obs_status, trt = trt)
#' adj <- adjMixBayes(linked.data = linked_df)
#'
#' fit <- plsurvreg(
#'   survival::Surv(time, status) ~ trt,
#'   dist = "weibull",
#'   adjustment = adj,
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#'
#' # Create a design matrix for new covariate values
#' newdata <- stats::model.matrix(~ trt, data = data.frame(trt = c(0, 1)))
#'
#' # Predict posterior mean linear predictors for each latent component
#' preds <- predict(fit, newdata = newdata, se.fit = TRUE, interval = "credible")
#' print(preds$component1)
#' print(preds$component2)
#' }
#'
#' @export
#' @method predict survMixBayes
predict.survMixBayes <- function(object, newdata = NULL,
                                 se.fit = FALSE,
                                 interval = c("none", "credible"),
                                 level = 0.95,
                                 ...) {
 interval <- match.arg(interval)

 if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
  stop("`level` must be a single number strictly between 0 and 1.", call. = FALSE)
 }

 X <- newdata
 if (is.null(X)) {
  if (!is.null(object$X)) {
   X <- object$X
  }
 }

 if (is.null(X)) {
  stop("`newdata` must be provided (object does not store X).", call. = FALSE)
 }

 if (!is.matrix(X) || !is.numeric(X)) {
  stop(
   "`newdata` must be a numeric matrix with columns aligned to the design matrix used for fitting.",
   call. = FALSE
  )
 }

 b1 <- object$estimates$coefficients
 b2 <- object$estimates$m.coefficients

 if (!is.matrix(b1) || !is.matrix(b2)) {
  stop(
   "Posterior coefficient draws are not stored in the expected matrix format.",
   call. = FALSE
  )
 }

 if (ncol(X) != ncol(b1) || ncol(X) != ncol(b2)) {
  stop(
   "The number of columns in `newdata` does not match the fitted design matrix.",
   call. = FALSE
  )
 }

 pred1 <- X %*% t(b1)
 pred2 <- X %*% t(b2)

 summarize_component <- function(all_predictions, se.fit, interval, level) {
  fit <- rowMeans(all_predictions)

  if (!se.fit && interval == "none") {
   return(as.vector(fit))
  }

  out <- cbind(fit = fit)

  if (se.fit) {
   out <- cbind(out, se.fit = apply(all_predictions, 1, stats::sd))
  }

  if (interval == "credible") {
   alpha <- 1 - level
   ci <- t(apply(
    all_predictions, 1,
    stats::quantile,
    probs = c(alpha / 2, 1 - alpha / 2)
   ))
   out <- cbind(out, lower = ci[, 1], upper = ci[, 2])

   if (se.fit) {
    colnames(out) <- c(
     "fit", "se.fit",
     paste0(alpha / 2 * 100, " %"),
     paste0((1 - alpha / 2) * 100, " %")
    )
   } else {
    colnames(out) <- c(
     "fit",
     paste0(alpha / 2 * 100, " %"),
     paste0((1 - alpha / 2) * 100, " %")
    )
   }
  } else {
   colnames(out) <- c("fit", "se.fit")
  }

  out
 }

 list(
  component1 = summarize_component(pred1, se.fit, interval, level),
  component2 = summarize_component(pred2, se.fit, interval, level)
 )
}

#' Pool Regression Fits Across Posterior Draws of Correct-Match Classifications
#'
#' @description
#' Use posterior draws of the latent match indicators from \code{survregMixBayes()}
#' to repeatedly identify which records are treated as correct matches, refit a
#' Cox proportional hazards model on those records, and pool the resulting
#' estimates using multiple-imputation pooling rules.
#'
#' Each retained posterior draw defines one subset of records classified as
#' correct matches. The function fits the specified \code{survival::coxph()}
#' model to that subset, extracts the estimated coefficients and covariance
#' matrix, and combines the results across draws using Rubin's rules.
#'
#' @param object A \code{survMixBayes} model object containing posterior draws of
#'   the latent match indicators.
#' @param data A data.frame with all candidate records in the same row order as used in the model.
#' @param formula Model formula for refitting on each draw (required), typically
#'   of the form \code{survival::Surv(time, event) ~ ...}.
#' @param min_n Minimum number of records required to fit the model for a given
#'   posterior draw. The default is \code{p + 2}, where \code{p} is the number
#'   of non-intercept columns in the model matrix.
#' @param quietly If \code{TRUE}, draws that lead to fitting errors are skipped
#'   without printing the full error message.
#' @param ties Method for handling tied event times in \code{survival::coxph()}.
#'   Default is \code{"efron"}.
#' @param ... Additional arguments passed to \code{survival::coxph()}.
#'
#' @return An object of class \code{c("mi_link_pool_survreg", "mi_link_pool")}
#'   containing pooled coefficient estimates, standard errors, confidence
#'   intervals, and related summary information.
#'
#' @examples
#' \donttest{
#' set.seed(301)
#' n <- 150
#' trt <- rbinom(n, 1, 0.5)
#'
#' # Simulate Weibull AFT data
#' true_time <- rweibull(n, shape = 1.5, scale = exp(1 + 0.8 * trt))
#' cens_time <- rexp(n, rate = 0.1)
#' true_obs_time <- pmin(true_time, cens_time)
#' true_status <- as.integer(true_time <= cens_time)
#'
#' # Induce linkage mismatch errors in approximately 20% of records
#' is_mismatch <- rbinom(n, 1, 0.2)
#' obs_time <- true_obs_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' obs_status[mismatch_idx] <- obs_status[shuffled]
#'
#' linked_df <- data.frame(time = obs_time, status = obs_status, trt = trt)
#' adj <- adjMixBayes(linked.data = linked_df)
#'
#' fit <- plsurvreg(
#'   survival::Surv(time, status) ~ trt,
#'   dist = "weibull",
#'   adjustment = adj,
#'   control = list(iterations = 200, burnin.iterations = 100, seed = 123)
#' )
#'
#' pooled_obj <- mi_with(
#'   object = fit,
#'   data = linked_df,
#'   formula = survival::Surv(time, status) ~ trt
#' )
#'
#' print(pooled_obj)
#' }
#'
#' @export
#' @method mi_with survMixBayes
#' @importFrom stats coef vcov cov qt model.frame model.matrix
#' @importFrom survival coxph Surv
mi_with.survMixBayes <- function(object, data, formula,
                                 min_n = NULL, quietly = TRUE,
                                 ties = "efron", ...) {

 if (missing(formula) || is.null(formula)) {
  if (!is.null(object$call$formula)) {
   formula <- eval(object$call$formula, envir = parent.frame())
  } else {
   stop(
    "Formula not found: please provide it explicitly or ensure object$call$formula exists.",
    call. = FALSE
   )
  }
 }

 if (!inherits(formula, "formula")) {
  stop("`formula` must be a valid formula object.", call. = FALSE)
 }

 if (missing(data) || is.null(data) || !is.data.frame(data)) {
  stop("`data` must be provided as a data.frame.", call. = FALSE)
 }

 z_samples <- object$m_samples

 collapse_z_g <- function(z_samples, g) {
  if (is.list(z_samples)) {
   parts <- lapply(z_samples, function(Z) {
    if (is.null(dim(Z))) {
     stop("Each list element of `z_samples` must be a matrix-like object.", call. = FALSE)
    }
    Z[g, ]
   })
   as.integer(do.call(c, parts))
  } else {
   if (is.null(dim(z_samples)) || length(dim(z_samples)) != 2L) {
    stop("`z_samples` must be an S x N matrix, or a list of such matrices.", call. = FALSE)
   }
   as.integer(z_samples[g, ])
  }
 }

 fit_once <- function(df) {
  fit <- survival::coxph(formula = formula, data = df, ties = ties, ...)
  list(coef = stats::coef(fit), vcov = stats::vcov(fit))
 }

 mf <- stats::model.frame(formula, data = data)
 Xtmp <- stats::model.matrix(formula, mf)
 if ("(Intercept)" %in% colnames(Xtmp)) {
  Xtmp <- Xtmp[, colnames(Xtmp) != "(Intercept)", drop = FALSE]
 }
 p <- ncol(Xtmp)
 if (is.null(min_n)) min_n <- p + 2L

 S <- if (is.list(z_samples)) nrow(z_samples[[1]]) else nrow(z_samples)
 if (length(S) == 0L || is.null(S) || !is.finite(S)) {
  stop("Unable to infer the number of posterior draws (S).", call. = FALSE)
 }

 coefs_list <- list()
 vcovs_list <- list()
 kept <- logical(S)

 for (s in seq_len(S)) {
  zs <- collapse_z_g(z_samples, s)

  if (!all(zs %in% c(1L, 2L))) {
   stop("`m_samples` must contain only 1/2 indicators.", call. = FALSE)
  }

  idx <- which(zs == 1L)
  if (length(idx) < min_n) next

  dat_s <- data[idx, , drop = FALSE]

  res <- try(fit_once(dat_s), silent = quietly)
  if (!inherits(res, "try-error") &&
      is.numeric(res$coef) &&
      is.matrix(res$vcov) &&
      length(res$coef) > 0L) {
   coefs_list[[length(coefs_list) + 1L]] <- res$coef
   vcovs_list[[length(vcovs_list) + 1L]] <- res$vcov
   kept[s] <- TRUE
  }
 }

 m <- length(coefs_list)
 if (m == 0L) {
  stop(
   "No valid imputations: all draws failed or had too few component-1 records.",
   call. = FALSE
  )
 }

 parnames <- names(coefs_list[[1L]])
 coefs_mat <- do.call(rbind, lapply(coefs_list, function(b) b[parnames]))

 Ubar <- Reduce("+", vcovs_list) / m
 B <- stats::cov(coefs_mat)
 Tmat <- Ubar + (1 + 1 / m) * B

 qbar <- colMeans(coefs_mat)
 se <- sqrt(diag(Tmat))
 lambda <- diag((1 + 1 / m) * B) / diag(Tmat)

 r <- diag((1 + 1 / m) * B) / diag(Ubar)
 r[!is.finite(r)] <- 0
 dfold <- (m - 1) * (1 + 1 / pmax(r, .Machine$double.eps))^2
 df <- pmax(dfold, 3)

 tcrit <- stats::qt(0.975, df = df)
 lwr <- qbar - tcrit * se
 upr <- qbar + tcrit * se
 ci95 <- cbind(lwr = lwr, upr = upr)
 rownames(ci95) <- names(qbar)

 out <- list(
  m          = m,
  coef       = qbar,
  vcov       = Tmat,
  se         = se,
  ci95       = ci95,
  Ubar       = Ubar,
  B          = B,
  lambda     = lambda,
  df         = df,
  kept_draws = which(kept),
  dist       = object$dist,
  refit      = "coxph",
  call       = object$call
 )

 class(out) <- c("mi_link_pool_survreg", "mi_link_pool")
 out
}

#' Print Pooled Cox Regression Results
#'
#' @param x An object of class \code{mi_link_pool_survreg}, typically returned by
#'   \code{mi_with()} for a \code{survMixBayes} fit.
#' @param digits the number of significant digits to print.
#' @param ... further arguments (unused).
#'
#' @return The input \code{x}, invisibly.
#'
#' @examples
#' \donttest{
#' set.seed(301)
#' n <- 150
#' trt <- rbinom(n, 1, 0.5)
#'
#' # Simulate Weibull AFT data
#' true_time <- rweibull(n, shape = 1.5, scale = exp(1 + 0.8 * trt))
#' cens_time <- rexp(n, rate = 0.1)
#' true_obs_time <- pmin(true_time, cens_time)
#' true_status <- as.integer(true_time <= cens_time)
#'
#' # Induce linkage mismatch errors in approximately 20% of records
#' is_mismatch <- rbinom(n, 1, 0.2)
#' obs_time <- true_obs_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' obs_status[mismatch_idx] <- obs_status[shuffled]
#'
#' linked_df <- data.frame(time = obs_time, status = obs_status, trt = trt)
#' adj <- adjMixBayes(linked.data = linked_df)
#'
#' fit <- plsurvreg(
#'   survival::Surv(time, status) ~ trt,
#'   dist = "weibull",
#'   adjustment = adj,
#'   control = list(iterations = 200, burnin.iterations = 100, seed = 123)
#' )
#'
#' pooled_obj <- mi_with(
#'   object = fit,
#'   data = linked_df,
#'   formula = survival::Surv(time, status) ~ trt
#' )
#'
#' print(pooled_obj, digits = 4)
#' }
#'
#' @export
#' @method print mi_link_pool_survreg
print.mi_link_pool_survreg <- function(x,
                                       digits = max(3L, getOption("digits") - 2L),
                                       ...) {
 cat("Pooled Cox regression results across posterior match classifications:\n")
 cat("  Retained imputations (m):", x$m, "\n")
 cat("  Mixture model distribution:", x$dist, "\n")
 cat("  Refit model: coxph\n\n")

 tab <- cbind(
  Estimate  = x$coef,
  Std.Error = x$se,
  CI.lwr    = x$ci95[, "lwr"],
  CI.upr    = x$ci95[, "upr"],
  df        = x$df
 )

 stats::printCoefmat(tab, digits = digits, has.Pvalue = FALSE, ...)
 invisible(x)
}
