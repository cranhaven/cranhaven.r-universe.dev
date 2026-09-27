#' Extract Variance-Covariance Matrix from a coxphMixture Object
#'
#' @description
#' Extracts the variance-covariance matrix of the main parameters from a fitted
#' \code{coxphMixture} object. The matrix is estimated using Louis' method (1982)
#' to account for the missing data structure (latent match status) inherent in the
#' mixture model.
#'
#' @param object An object of class \code{coxphMixture}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A matrix of the estimated covariances between the parameter estimates.
#' The rows and columns correspond to the outcome model coefficients (\code{beta})
#' and the mismatch model coefficients (\code{gamma}).
#'
#' @references
#' Louis, T. A. (1982). Finding the observed information matrix when using the
#' EM algorithm. \emph{Journal of the Royal Statistical Society: Series B
#' (Methodological)}, 44(2), 226-233.
#'
#' @examples
#' library(survival)
#' set.seed(201)
#'
#' # Simulate survival data (N = 200)
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rbinom(n, 1, 0.5)
#' true_time <- rexp(n, rate = exp(0.5 * x1 - 0.5 * x2))
#' cens_time <- rexp(n, rate = 0.5)
#'
#' # Simulate auxiliary match scores and linkage errors
#' # Lower match scores correspond to a higher probability of mismatch
#' match_score <- runif(n, 0.5, 1.0)
#' is_mismatch <- rbinom(n, 1, prob = 1 - match_score)
#'
#' # Induce linkage errors by shuffling covariates of mismatched records
#' linked_x1 <- x1
#' linked_x2 <- x2
#' mis_idx <- which(is_mismatch == 1)
#' shuffled_idx <- sample(mis_idx)
#' linked_x1[mis_idx] <- x1[shuffled_idx]
#' linked_x2[mis_idx] <- x2[shuffled_idx]
#'
#' linked_data <- data.frame(
#'   time = pmin(true_time, cens_time),
#'   status = as.numeric(true_time <= cens_time),
#'   x1 = linked_x1, x2 = linked_x2,
#'   match_score = match_score
#' )
#'
#' # Fit the Cox PH Mixture Model (Slawski et al., 2023)
#' adj <- adjMixture(linked.data = linked_data, m.formula = ~ match_score)
#' fit <- plcoxph(Surv(time, status) ~ x1 + x2, adjustment = adj,
#'                control = list(max.iter = 15))
#'
#' # Extract the variance-covariance matrix
#' # Note: For outcome coefficients (beta) and mismatch coefficients (gamma)
#' vmat <- vcov(fit)
#' print(vmat)
#'
#' @export
vcov.coxphMixture <- function(object, ...) {
 return(object$var)
}

#' Confidence Intervals for coxphMixture Objects
#'
#' @description
#' Computes Wald confidence intervals for the coefficients of the outcome model
#' and the mismatch indicator model.
#'
#' @param object An object of class \code{coxphMixture}.
#' @param parm A specification of which parameters are to be given confidence intervals,
#' either a vector of numbers or names. If missing, all parameters are considered.
#' @param level The confidence level required (default is 0.95).
#' @param ... Additional arguments (currently ignored).
#'
#' @details
#' The intervals are calculated based on the variance-covariance matrix returned
#' by \code{\link{vcov.coxphMixture}}, using the standard normal approximation:
#' \code{Estimate +/- z_crit * SE}.
#'
#' @return A matrix (or vector) with lower and upper confidence limits for each parameter.
#'
#' @examples
#' library(survival)
#' set.seed(201)
#'
#' # Simulate survival data (N = 200)
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rbinom(n, 1, 0.5)
#' true_time <- rexp(n, rate = exp(0.5 * x1 - 0.5 * x2))
#' cens_time <- rexp(n, rate = 0.5)
#'
#' # Simulate auxiliary match scores and linkage errors
#' # Lower match scores correspond to a higher probability of mismatch
#' match_score <- runif(n, 0.5, 1.0)
#' is_mismatch <- rbinom(n, 1, prob = 1 - match_score)
#'
#' # Induce linkage errors by shuffling covariates of mismatched records
#' linked_x1 <- x1
#' linked_x2 <- x2
#' mis_idx <- which(is_mismatch == 1)
#' shuffled_idx <- sample(mis_idx)
#' linked_x1[mis_idx] <- x1[shuffled_idx]
#' linked_x2[mis_idx] <- x2[shuffled_idx]
#'
#' linked_data <- data.frame(
#'   time = pmin(true_time, cens_time),
#'   status = as.numeric(true_time <= cens_time),
#'   x1 = linked_x1, x2 = linked_x2,
#'   match_score = match_score
#' )
#'
#' # Fit the Cox PH Mixture Model (Slawski et al., 2023)
#' adj <- adjMixture(linked.data = linked_data, m.formula = ~ match_score)
#' fit <- plcoxph(Surv(time, status) ~ x1 + x2, adjustment = adj,
#'                control = list(max.iter = 15))
#'
#' # Extract Confidence Intervals
#' confint(fit)
#' confint(fit, parm = "treatment", level = 0.90)
#'
#' @export
confint.coxphMixture <- function(object, parm, level = 0.95, ...) {
 # Aggregate all coefficients
 coefs <- c(object$coefficients, object$m.coefficients)

 # Ensure vcov aligns with coefs
 vc <- vcov(object)

 # Parameter selection
 pnames <- names(coefs)
 if (missing(parm)) {
  parm <- pnames
 } else if (is.numeric(parm)) {
  parm <- pnames[parm]
 }

 # Critical value (Standard Normal for Cox/Large sample)
 a <- (1 - level) / 2
 a <- c(a, 1 - a)
 crit <- stats::qnorm(1 - (1 - level) / 2)

 # Format column names
 pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
 ci <- array(NA, dim = c(length(parm), 2), dimnames = list(parm, pct))

 # Computation
 ses <- sqrt(diag(vc))
 names(ses) <- rownames(vc)

 # Safe matching of parameters to SEs
 idx <- match(parm, names(coefs))
 est_sub <- coefs[idx]
 se_sub <- ses[match(parm, names(ses))]

 ci[] <- est_sub + se_sub %o% c(-crit, crit)

 return(ci)
}

#' Summarizing Cox PH Mixture Fits
#'
#' @description
#' \code{summary} method for class \code{coxphMixture}. Provides a detailed summary
#' of the fitted model, including coefficients, hazard ratios, standard errors,
#' z-statistics, and p-values for both the outcome model and the mismatch model.
#'
#' @param object An object of class \code{coxphMixture}.
#' @param conf.int The confidence level for the confidence intervals of the hazard ratios.
#' @param scale Scale factor for the standard errors (default is 1).
#' @param ... Additional arguments.
#'
#' @return An object of class \code{summary.coxphMixture} containing:
#' \item{call}{The function call.}
#' \item{n}{Total number of observations.}
#' \item{nevent}{Number of events.}
#' \item{coefficients}{Matrix of coefficients for the outcome model.}
#' \item{m.coefficients}{Matrix of coefficients for the mismatch model.}
#' \item{conf.int}{Matrix of confidence intervals for the hazard ratios.}
#' \item{logtest}{Log-likelihood information (Outcome Model).}
#' \item{avgcmr}{The average posterior probability of a correct match.}
#'
#' @examples
#' library(survival)
#' set.seed(201)
#'
#' # Simulate survival data (N = 200)
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rbinom(n, 1, 0.5)
#' true_time <- rexp(n, rate = exp(0.5 * x1 - 0.5 * x2))
#' cens_time <- rexp(n, rate = 0.5)
#'
#' # Simulate auxiliary match scores and linkage errors
#' # Lower match scores correspond to a higher probability of mismatch
#' match_score <- runif(n, 0.5, 1.0)
#' is_mismatch <- rbinom(n, 1, prob = 1 - match_score)
#'
#' # Induce linkage errors by shuffling covariates of mismatched records
#' linked_x1 <- x1
#' linked_x2 <- x2
#' mis_idx <- which(is_mismatch == 1)
#' shuffled_idx <- sample(mis_idx)
#' linked_x1[mis_idx] <- x1[shuffled_idx]
#' linked_x2[mis_idx] <- x2[shuffled_idx]
#'
#' linked_data <- data.frame(
#'   time = pmin(true_time, cens_time),
#'   status = as.numeric(true_time <= cens_time),
#'   x1 = linked_x1, x2 = linked_x2,
#'   match_score = match_score
#' )
#'
#' # Fit the Cox PH Mixture Model (Slawski et al., 2023)
#' adj <- adjMixture(linked.data = linked_data, m.formula = ~ match_score)
#' fit <- plcoxph(Surv(time, status) ~ x1 + x2, adjustment = adj,
#'                control = list(max.iter = 15))
#'
#' # Print detailed statistical summary
#' sum_fit <- summary(fit)
#' print(sum_fit)
#'
#' @export
summary.coxphMixture <- function(object, conf.int = 0.95, scale = 1, ...) {

 # Outcome Model Coefficients
 beta <- object$coefficients
 n_beta <- length(beta)

 # Extract SEs from the top-left block of the variance matrix
 # object$var includes both beta and gamma.
 # We assume the first n_beta rows/cols correspond to beta.
 se_beta <- sqrt(diag(object$var)[1:n_beta])
 z_beta <- beta / se_beta
 p_beta <- 2 * (1 - stats::pnorm(abs(z_beta)))

 coef_mat <- cbind(
  coef = beta,
  `exp(coef)` = exp(beta),
  `se(coef)` = se_beta,
  z = z_beta,
  `Pr(>|z|)` = p_beta
 )
 rownames(coef_mat) <- names(beta)

 # Confidence Intervals for Hazard Ratios
 z_crit <- stats::qnorm((1 + conf.int) / 2, 0, 1)
 tmp <- cbind(
  exp(beta),
  exp(-beta),
  exp(beta - z_crit * se_beta),
  exp(beta + z_crit * se_beta)
 )
 colnames(tmp) <- c("exp(coef)", "exp(-coef)",
                    paste("lower .", round(100 * conf.int, 2), sep = ""),
                    paste("upper .", round(100 * conf.int, 2), sep = ""))
 rownames(tmp) <- names(beta)

 # Mismatch Model Coefficients
 gamma <- object$m.coefficients
 m_coef_mat <- NULL

 if (length(gamma) > 0) {
  # Extract SEs for gamma (remainder of the diagonal)
  se_gamma <- sqrt(diag(object$var)[(n_beta + 1):nrow(object$var)])
  z_gamma <- gamma / se_gamma
  p_gamma <- 2 * (1 - stats::pnorm(abs(z_gamma)))

  m_coef_mat <- cbind(gamma, se_gamma, z_gamma, p_gamma)

  colnames(m_coef_mat) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  rownames(m_coef_mat) <- names(gamma)
 }

 # Construct Result
 res <- list(
  call = object$call,
  n = object$n,
  nevent = object$nevent,
  coefficients = coef_mat,
  m.coefficients = m_coef_mat,
  conf.int = tmp,
  avgcmr = mean(object$match.prob, na.rm = TRUE),
  iter = length(object$objective)
 )

 class(res) <- "summary.coxphMixture"
 return(res)
}

#' @export
print.summary.coxphMixture <- function(x,
                                       digits = max(3L, getOption("digits") - 3L),
                                       signif.stars = getOption("show.signif.stars"),
                                       ...) {
 cat("\nCall:\n")
 dput(x$call)

 cat("\n--- Outcome Model (Cox PH) ---\n")
 stats::printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
                     P.values = TRUE, has.Pvalue = TRUE,
                     signif.legend = FALSE)

 cat("\n--- Hazard Ratios & Confidence Intervals ---\n")
 print(x$conf.int, digits = digits)

 if (!is.null(x$m.coefficients)) {
  cat("\n--- Mismatch Indicator Model ---\n")
  stats::printCoefmat(x$m.coefficients, digits = digits, signif.stars = signif.stars, P.values = TRUE, has.Pvalue = TRUE)
 }

 cat("\nAverage Estimated Correct Match Rate:", format(x$avgcmr, digits = digits), "\n")
 cat("Events:", x$nevent, " / Total:", x$n, "\n")
 cat("Iterations:", x$iter, "\n\n")

 invisible(x)
}

#' Print a coxphMixture Object
#'
#' @param x An object of class \code{coxphMixture}.
#' @param digits The number of significant digits to use.
#' @param ... Additional arguments.
#'
#' @examples
#' library(survival)
#' set.seed(201)
#'
#' # Simulate survival data (N = 200)
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rbinom(n, 1, 0.5)
#' true_time <- rexp(n, rate = exp(0.5 * x1 - 0.5 * x2))
#' cens_time <- rexp(n, rate = 0.5)
#'
#' # Simulate auxiliary match scores and linkage errors
#' # Lower match scores correspond to a higher probability of mismatch
#' match_score <- runif(n, 0.5, 1.0)
#' is_mismatch <- rbinom(n, 1, prob = 1 - match_score)
#'
#' # Induce linkage errors by shuffling covariates of mismatched records
#' linked_x1 <- x1
#' linked_x2 <- x2
#' mis_idx <- which(is_mismatch == 1)
#' shuffled_idx <- sample(mis_idx)
#' linked_x1[mis_idx] <- x1[shuffled_idx]
#' linked_x2[mis_idx] <- x2[shuffled_idx]
#'
#' linked_data <- data.frame(
#'   time = pmin(true_time, cens_time),
#'   status = as.numeric(true_time <= cens_time),
#'   x1 = linked_x1, x2 = linked_x2,
#'   match_score = match_score
#' )
#'
#' # Fit the Cox PH Mixture Model (Slawski et al., 2023)
#' adj <- adjMixture(linked.data = linked_data, m.formula = ~ match_score)
#' fit <- plcoxph(Surv(time, status) ~ x1 + x2, adjustment = adj,
#'                control = list(max.iter = 15))
#'
#' # Explicitly call the print method
#' print(fit)
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @export
print.coxphMixture <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
 cat("Call:\n")
 dput(x$call)

 cat("\nOutcome Model Coefficients:\n")
 print(format(x$coefficients, digits = digits), quote = FALSE)

 if (length(x$m.coefficients) > 0) {
  cat("\nMismatch Model Coefficients:\n")
  print(format(x$m.coefficients, digits = digits), quote = FALSE)
 }

 cat("\nLikelihood ratio test (model=outcome) not available due to pseudo-likelihood.\n")
 cat("n=", x$n, ", number of events=", x$nevent, "\n")
 invisible(x)
}

#' Model Predictions for coxphMixture Objects
#'
#' @description
#' Compute fitted values and predictions for the outcome model component of the mixture.
#' The predictions are conditional on the latent status being a "correct match".
#'
#' @param object An object of class \code{coxphMixture}.
#' @param newdata Optional new data frame. If missing, predictions are for the original data.
#' @param type The type of prediction.
#' \itemize{
#'   \item \code{"lp"}: Linear predictor (eta = X * beta).
#'   \item \code{"risk"}: Risk score (exp(eta)).
#'   \item \code{"expected"}: Expected number of events (approximate).
#'   \item \code{"survival"}: Survival probability at the observed times.
#' }
#' @param se.fit Logical; whether to compute standard errors (based on the sandwich/Louis variance).
#' @param na.action Function to handle missing values in \code{newdata}.
#' @param reference Reference for centering (currently ignored, defaults to uncentered).
#' @param ... Additional arguments passed to methods.
#'
#' @details
#' When \code{newdata} is supplied, the function constructs the model matrix using
#' the terms from the original fit. Standard errors are computed using the
#' estimated variance-covariance matrix of the mixture model coefficients.
#'
#' For \code{type = "expected"} and \code{"survival"}, the function reconstructs the
#' cumulative baseline hazard step function \eqn{\Lambda_0(t)} using the Breslow
#' estimator stored in the object and evaluates it at the time points found in
#' \code{newdata}.
#'
#' @return A vector or matrix of predictions, or a list containing \code{fit} and
#' \code{se.fit} if standard errors are requested.
#'
#' @examples
#' library(survival)
#' set.seed(205)
#'
#' # Simulate auxiliary match scores and linkage errors
#' # Lower match scores correspond to a higher probability of mismatch
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rbinom(n, 1, 0.5)
#' true_time <- rexp(n, rate = exp(0.5 * x1 - 0.5 * x2))
#' cens_time <- rexp(n, rate = 0.5)
#' match_score <- runif(n, 0.5, 1.0)
#'
#' mis_idx <- which(rbinom(n, 1, prob = 1 - match_score) == 1)
#' x1[mis_idx] <- x1[sample(mis_idx)]
#' x2[mis_idx] <- x2[sample(mis_idx)]
#'
#' linked_data <- data.frame(
#'   time = pmin(true_time, cens_time),
#'   status = as.numeric(true_time <= cens_time),
#'   x1 = x1, x2 = x2, match_score = match_score
#' )
#'
#' # Fit the Cox PH Mixture Model
#' # Note: We set `y = TRUE` to store the response for baseline hazard reconstruction
#' adj <- adjMixture(linked.data = linked_data, m.formula = ~ match_score)
#' fit <- plcoxph(Surv(time, status) ~ x1 + x2, adjustment = adj,
#'                y = TRUE, control = list(max.iter = 15))
#'
#' # 1. Extract linear predictors for the original data
#' lp_train <- predict(fit, type = "lp")
#' head(lp_train)
#'
#' # 2. Predict hazard ratios (risk) and expected events for a new cohort
#' new_cohort <- data.frame(
#'   time = c(1.0, 2.5, 0.5), # Required for expected/survival types
#'   x1 = c(0, 1.5, -1),
#'   x2 = c(0, 1, 1)
#' )
#'
#' # Predict risk (exp(lp))
#' risk_scores <- predict(fit, newdata = new_cohort, type = "risk")
#' print(risk_scores)
#'
#' # Predict expected number of events based on the reconstructed baseline hazard
#' exp_events <- predict(fit, newdata = new_cohort, type = "expected")
#' print(exp_events)
#'
#' @importFrom stats terms delete.response model.frame model.matrix predict approxfun
#' @export
predict.coxphMixture <- function(object, newdata,
                                 type = c("lp", "risk", "expected", "survival"),
                                 se.fit = FALSE,
                                 na.action = stats::na.pass,
                                 reference = "strata",
                                 ...) {

 type <- match.arg(type)

 # Safely extract terms (fallback to model frame attributes or call formula)
 trms <- object$terms
 if (is.null(trms)) trms <- attr(object$model, "terms")
 if (is.null(trms)) {
  form <- object$formula
  if (is.null(form) && !is.null(object$call)) {
   if ("formula" %in% names(object$call)) {
    form <- eval(object$call$formula)
   } else {
    try_form <- try(eval(object$call[[2]]), silent = TRUE)
    if (inherits(try_form, "formula")) form <- try_form
   }
  }
  if (!is.null(form)) trms <- stats::terms(form)
 }

 if (is.null(trms)) {
  stop("Cannot extract terms from the model object. Please refit with 'model = TRUE'.")
 }

 # Construct Design Matrix (X) and Identify Time (if needed)
 new_time <- NULL

 if (missing(newdata)) {
  # Use original X if available
  if (!is.null(object$x)) {
   X <- object$x
  } else if (!is.null(object$model)) {
   # Reconstruct from model frame
   trms_no_resp <- stats::delete.response(trms)
   X <- stats::model.matrix(trms_no_resp, object$model)
   if (attr(trms, "intercept") == 1) X <- X[, -1, drop = FALSE]
  } else {
   stop("Original model matrix not found. Please refit with 'x = TRUE' or 'model = TRUE'.")
  }

  # For original data, Lambdahat0 is already aligned 1:1 with observations
  base_haz_vals <- object$Lambdahat0

 } else {
  # Process New Data
  trms_no_resp <- stats::delete.response(trms)
  mf <- stats::model.frame(trms_no_resp, data = newdata, na.action = na.action)
  X <- stats::model.matrix(trms_no_resp, mf)
  if (attr(trms, "intercept") == 1) X <- X[, -1, drop = FALSE]

  # If type is expected/survival, we need the Time variable from newdata
  if (type %in% c("expected", "survival")) {

   # Safely extract time variable name from the Surv() call in the terms
   resp_var <- as.character(attr(trms, "variables")[[2]])
   if (length(resp_var) >= 2 && resp_var[1] == "Surv") {
    time_var_name <- resp_var[2]
   } else {
    stop("Could not parse Surv() call to identify the time variable.")
   }

   if (time_var_name %in% names(newdata)) {
    new_time <- newdata[[time_var_name]]
   } else {
    stop("Could not identify the time variable '", time_var_name, "' in 'newdata'.")
   }

   # Reconstruct Step Function for Baseline Hazard
   if (is.null(object$y)) {
    stop("The training response 'y' is missing from the object. Please refit with 'y = TRUE' to support expected/survival prediction on new data.")
   }

   train_times <- object$y[, "time"]
   ord <- order(train_times)
   sorted_times <- train_times[ord]
   sorted_haz <- object$Lambdahat0[ord]

   # Create right-continuous step function
   haz_step_fun <- stats::approxfun(sorted_times, sorted_haz, method = "constant", rule = 2)
   base_haz_vals <- haz_step_fun(new_time)
  }
 }

 # Compute Linear Predictor and Risk
 beta <- object$coefficients

 # Ensure design matrix aligns with coefficients
 if (!is.null(colnames(X)) && !is.null(names(beta))) {
  common <- intersect(colnames(X), names(beta))
  if (length(common) == length(beta)) {
   X <- X[, names(beta), drop = FALSE]
  }
 }

 if (ncol(X) != length(beta)) {
  stop(paste("Dimension mismatch: Design matrix has", ncol(X),
             "columns but model has", length(beta), "coefficients."))
 }

 lp <- as.vector(X %*% beta)

 if (type == "lp") {
  pred <- lp
 } else if (type == "risk") {
  pred <- exp(lp)
 } else if (type %in% c("expected", "survival")) {

  # Cumulative Hazard = Lambda0(t) * exp(lp)
  expected <- base_haz_vals * exp(lp)

  if (type == "expected") {
   pred <- expected
  } else {
   pred <- exp(-expected)
  }
 }

 # Compute Standard Errors
 if (se.fit) {
  n_beta <- length(beta)
  cov_beta <- object$var[1:n_beta, 1:n_beta, drop = FALSE]

  # Variance of LP: diag(X * Sigma * X')
  var_lp <- rowSums((X %*% cov_beta) * X)
  se_lp <- sqrt(var_lp)

  if (type == "lp") {
   se <- se_lp
  } else if (type == "risk") {
   # Delta method: Var(exp(lp)) = (exp(lp))^2 * Var(lp)
   se <- pred * se_lp
  } else {
   warning("Standard errors for 'expected' and 'survival' types are not currently available for mixture models.")
   se <- rep(NA, length(pred))
  }

  return(list(fit = pred, se.fit = se))
 }

 return(pred)
}
