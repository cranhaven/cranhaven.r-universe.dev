#' @title Variance-Covariance Matrix for coxphELE Objects
#' @description
#' Extracts the variance-covariance matrix of the main parameters (outcome model coefficients)
#' from a fitted \code{coxphELE} model.
#'
#' @param object An object of class \code{coxphELE}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A matrix of the estimated covariances between the parameter estimates.
#'
#' @examples
#' library(survival)
#' set.seed(104)
#' n <- 200
#'
#' # 1. Simulate covariates
#' age_centered <- rnorm(n, 0, 5)
#' treatment <- rbinom(n, 1, 0.5)
#'
#' # 2. Simulate true survival times
#' true_time <- rexp(n, rate = exp(0.05 * age_centered - 0.6 * treatment))
#' cens_time <- rexp(n, rate = 0.2)
#' time <- pmin(true_time, cens_time)
#' status <- as.numeric(true_time <= cens_time)
#'
#' # 3. Induce 15% Exchangeable Linkage Error (ELE)
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' linked_age <- age_centered
#' linked_trt <- treatment
#'
#' # False links drawn uniformly from the target population
#' false_link_idx <- sample(1:n, size = length(mis_idx), replace = TRUE)
#' linked_age[mis_idx] <- age_centered[false_link_idx]
#' linked_trt[mis_idx] <- treatment[false_link_idx]
#'
#' linked_data <- data.frame(time = time, status = status,
#'                           age = linked_age, treatment = linked_trt)
#'
#' # 4. Fit the adjusted Cox PH model
#' adj <- adjELE(linked.data = linked_data, m.rate = 0.15)
#' fit <- plcoxph(Surv(time, status) ~ age + treatment, adjustment = adj)
#'
#' # 5. Extract the sandwich variance-covariance matrix
#' vmat <- vcov(fit)
#' print(vmat)
#'
#' @export
vcov.coxphELE <- function(object, ...) {
  if (is.null(object$var)) {
    stop("Variance-covariance matrix is missing from the fitted object.")
  }
  return(object$var)
}

#' @title Print a coxphELE Object
#' @description
#' Prints a short summary of the fitted CoxPH model with linkage error adjustment,
#' displaying the call (if available) and the estimated coefficients.
#'
#' @param x An object of class \code{coxphELE}.
#' @param digits The number of significant digits to use when printing.
#' @param ... Additional arguments passed to \code{\link{print.default}}.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' library(survival)
#' set.seed(104)
#' n <- 200
#'
#' # 1. Simulate covariates
#' age_centered <- rnorm(n, 0, 5)
#' treatment <- rbinom(n, 1, 0.5)
#'
#' # 2. Simulate true survival times
#' true_time <- rexp(n, rate = exp(0.05 * age_centered - 0.6 * treatment))
#' cens_time <- rexp(n, rate = 0.2)
#' time <- pmin(true_time, cens_time)
#' status <- as.numeric(true_time <= cens_time)
#'
#' # 3. Induce 15% Exchangeable Linkage Error (ELE)
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' linked_age <- age_centered
#' linked_trt <- treatment
#'
#' # False links drawn uniformly from the target population
#' false_link_idx <- sample(1:n, size = length(mis_idx), replace = TRUE)
#' linked_age[mis_idx] <- age_centered[false_link_idx]
#' linked_trt[mis_idx] <- treatment[false_link_idx]
#'
#' linked_data <- data.frame(time = time, status = status,
#'                           age = linked_age, treatment = linked_trt)
#'
#' # 4. Fit the adjusted Cox PH model
#' adj <- adjELE(linked.data = linked_data, m.rate = 0.15)
#' fit <- plcoxph(Surv(time, status) ~ age + treatment, adjustment = adj)
#'
#' # 5. Print the basic model output
#' print(fit)
#'
#' @export
print.coxphELE <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  # Print Call if available (usually added by the wrapper)
  if (!is.null(x$call)) {
    cat("Call:\n")
    dput(x$call)
    cat("\n")
  }

  if (!is.null(x$coefficients)) {
    cat("Coefficients:\n")
    print.default(format(x$coefficients, digits = digits),
                  print.gap = 2L, quote = FALSE, ...)
  }

  # Print event counts if available
  if (!is.null(x$n) && !is.null(x$nevent)) {
    cat("\n")
    cat(sprintf("n= %d, number of events= %d\n", x$n, x$nevent))
  }

  invisible(x)
}

#' @title Summary of a coxphELE Object
#' @description
#' Produces a summary of the fitted CoxPH model with linkage error adjustment,
#' including coefficient estimates, hazard ratios, standard errors, z-statistics,
#' and p-values.
#'
#' @param object An object of class \code{coxphELE}.
#' @param conf.int The confidence level for the confidence intervals (default is 0.95).
#' @param ... Additional arguments (currently ignored).
#'
#' @return An object of class \code{summary.coxphELE} containing:
#' \item{call}{The matched call.}
#' \item{coefficients}{A matrix with columns for coefficients, hazard ratios (exp(coef)),
#' standard errors, z-values, and p-values.}
#' \item{conf.int}{A matrix of confidence intervals for the hazard ratios.}
#' \item{n}{The number of observations.}
#' \item{nevent}{The number of events.}
#'
#' @examples
#' library(survival)
#' set.seed(104)
#' n <- 200
#'
#' # 1. Simulate covariates
#' age_centered <- rnorm(n, 0, 5)
#' treatment <- rbinom(n, 1, 0.5)
#'
#' # 2. Simulate true survival times
#' true_time <- rexp(n, rate = exp(0.05 * age_centered - 0.6 * treatment))
#' cens_time <- rexp(n, rate = 0.2)
#' time <- pmin(true_time, cens_time)
#' status <- as.numeric(true_time <= cens_time)
#'
#' # 3. Induce 15% Exchangeable Linkage Error (ELE)
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' linked_age <- age_centered
#' linked_trt <- treatment
#'
#' # False links drawn uniformly from the target population
#' false_link_idx <- sample(1:n, size = length(mis_idx), replace = TRUE)
#' linked_age[mis_idx] <- age_centered[false_link_idx]
#' linked_trt[mis_idx] <- treatment[false_link_idx]
#'
#' linked_data <- data.frame(time = time, status = status,
#'                           age = linked_age, treatment = linked_trt)
#'
#' # 4. Fit the adjusted Cox PH model
#' adj <- adjELE(linked.data = linked_data, m.rate = 0.15)
#' fit <- plcoxph(Surv(time, status) ~ age + treatment, adjustment = adj)
#'
#' # 5. Generate and print the detailed statistical summary
#' sum_fit <- summary(fit)
#' print(sum_fit)
#'
#' @export
summary.coxphELE <- function(object, conf.int = 0.95, ...) {

  # Extract Estimates
  beta <- object$coefficients
  if (is.null(beta)) stop("Object does not contain coefficients.")

  # Extract Variance and calculate SE
  if (!is.null(object$var)) {
    se <- sqrt(diag(object$var))
  } else {
    se <- rep(NA, length(beta))
  }

  # Calculate Z and P-values
  zval <- beta / se
  pval <- 2 * (1 - stats::pnorm(abs(zval)))

  # Create Coefficient Matrix
  # Structure: coef, exp(coef), se(coef), z, p
  coef_mat <- cbind(
    coef = beta,
    "exp(coef)" = exp(beta),
    "se(coef)" = se,
    z = zval,
    "Pr(>|z|)" = pval
  )

  # Calculate Confidence Intervals for Hazard Ratios
  if (!is.null(conf.int) && !is.na(conf.int)) {
    z_score <- stats::qnorm((1 + conf.int) / 2)
    tmp <- cbind(
      exp(beta),
      exp(beta - z_score * se),
      exp(beta + z_score * se)
    )
    lower_lbl <- paste("lower", format(conf.int, digits=2))
    upper_lbl <- paste("upper", format(conf.int, digits=2))
    colnames(tmp) <- c("exp(coef)", lower_lbl, upper_lbl)
    conf_int_mat <- tmp
  } else {
    conf_int_mat <- NULL
  }

  # Construct Result List
  ans <- list(
    call = object$call,
    coefficients = coef_mat,
    conf.int = conf_int_mat,
    n = object$n,
    nevent = object$nevent
  )

  class(ans) <- "summary.coxphELE"
  return(ans)
}

#' @noRd
#' @export
print.summary.coxphELE <- function(x,
                                   digits = max(3L, getOption("digits") - 3L),
                                   signif.stars = getOption("show.signif.stars"),
                                   ...) {

  if (!is.null(x$call)) {
    cat("Call:\n")
    dput(x$call)
    cat("\n")
  }

  if (!is.null(x$n) && !is.null(x$nevent)) {
    cat(paste("n=", x$n, ", number of events=", x$nevent, "\n\n", sep = ""))
  }

  if (!is.null(x$coefficients)) {
    stats::printCoefmat(x$coefficients,
                        digits = digits,
                        signif.stars = signif.stars,
                        P.values = TRUE,
                        has.Pvalue = TRUE,
                        ...)
  }

  if (!is.null(x$conf.int)) {
    cat("\nConfidence Intervals for Hazard Ratios:\n")
    print(x$conf.int, digits = digits, ...)
  }

  cat("\n")
  invisible(x)
}

#' @title Confidence Intervals for coxphELE Objects
#' @description
#' Computes Wald confidence intervals for the model coefficients.
#'
#' @param object An object of class \code{coxphELE}.
#' @param parm A specification of which parameters are to be given confidence intervals,
#' either a vector of numbers or names. If missing, all parameters are considered.
#' @param level The confidence level required (default 0.95).
#' @param ... Additional arguments (currently ignored).
#'
#' @return A matrix (or vector) with lower and upper confidence
#' limits for each parameter.
#'
#' @examples
#' library(survival)
#' set.seed(104)
#' n <- 200
#'
#' # 1. Simulate covariates
#' age_centered <- rnorm(n, 0, 5)
#' treatment <- rbinom(n, 1, 0.5)
#'
#' # 2. Simulate true survival times
#' true_time <- rexp(n, rate = exp(0.05 * age_centered - 0.6 * treatment))
#' cens_time <- rexp(n, rate = 0.2)
#' time <- pmin(true_time, cens_time)
#' status <- as.numeric(true_time <= cens_time)
#'
#' # 3. Induce 15% Exchangeable Linkage Error (ELE)
#' mis_idx <- sample(1:n, size = floor(0.15 * n))
#' linked_age <- age_centered
#' linked_trt <- treatment
#'
#' # False links drawn uniformly from the target population
#' false_link_idx <- sample(1:n, size = length(mis_idx), replace = TRUE)
#' linked_age[mis_idx] <- age_centered[false_link_idx]
#' linked_trt[mis_idx] <- treatment[false_link_idx]
#'
#' linked_data <- data.frame(time = time, status = status,
#'                           age = linked_age, treatment = linked_trt)
#'
#' # 4. Fit the adjusted Cox PH model
#' adj <- adjELE(linked.data = linked_data, m.rate = 0.15)
#' fit <- plcoxph(Surv(time, status) ~ age + treatment, adjustment = adj)
#'
#' # 5. Compute confidence intervals
#' confint(fit) # 95% CI for all coefficients
#' confint(fit, parm = "treatment", level = 0.90) # 90% CI for a specific parameter
#'
#' @export
confint.coxphELE <- function(object, parm, level = 0.95, ...) {

  cf <- object$coefficients
  pnames <- names(cf)

  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }

  # Standard Errors
  if (is.null(object$var)) stop("Variance matrix missing from object.")
  ses <- sqrt(diag(object$var))
  names(ses) <- pnames

  # Calculate Quantile
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  fac <- stats::qnorm(a)

  # Generate CI
  ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm, pct))

  ci[] <- cf[parm] + ses[parm] %o% fac

  return(ci)
}

#' @title Predictions from a coxphELE Object
#' @description
#' Computes linear predictors or risk scores from a fitted \code{coxphELE} model.
#' If \code{newdata} is provided, the method attempts to construct the model matrix
#' from the call stored in the object.
#'
#' @param object An object of class \code{coxphELE}.
#' @param newdata Optional new data frame to obtain predictions for. If omitted,
#' the linear predictors of the original data are returned.
#' @param type The type of prediction required. Type \code{"lp"} (default) returns
#' the linear predictor. Type \code{"risk"} returns the hazard ratio, \code{exp(lp)}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A numeric vector of predictions.
#'
#' @details
#' If \code{newdata} is supplied, the function attempts to retrieve the formula
#' from \code{object$call} or \code{object$formula}. If the original model was fitted
#' using the internal engine directly without a wrapper that stores the call/formula,
#' prediction on new data will fail.
#'
#' @examples
#' library(survival)
#' set.seed(105)
#'
#' # Simulate linked data subject to mismatch error
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rbinom(n, 1, 0.7)
#' true_time <- rexp(n, rate = exp(0.5 * x1 - 0.5 * x2))
#' cens_time <- rexp(n, rate = 0.5)
#'
#' # Induce 15% linkage error
#' mis_idx <- sample(1:n, size = 0.15 * n)
#' x1[mis_idx] <- x1[sample(mis_idx)]
#' x2[mis_idx] <- x2[sample(mis_idx)]
#'
#' # Linked data
#' linked_data <- data.frame(
#'   time = pmin(true_time, cens_time),
#'   status = as.numeric(true_time <= cens_time),
#'   x1 = x1, x2 = x2
#' )
#'
#' # Fit the adjusted Cox PH model
#' adj <- adjELE(linked.data = linked_data, m.rate = 0.15)
#' fit <- plcoxph(Surv(time, status) ~ x1 + x2, adjustment = adj)
#'
#' # 1. Extract linear predictors for the original data
#' lp_train <- predict(fit, type = "lp")
#' head(lp_train)
#'
#' # 2. Predict hazard ratios (risk) for a new cohort
#' new_cohort <- data.frame(x1 = c(0, 1.5, -1), x2 = c(0, 1, 1))
#' risk_scores <- predict(fit, newdata = new_cohort, type = "risk")
#' print(risk_scores)
#'
#' @export
predict.coxphELE <- function(object, newdata = NULL, type = c("lp", "risk"), ...) {

  type <- match.arg(type)

  # No new data (Use stored linear predictors)
  if (is.null(newdata)) {
    lp <- object$linear.predictors
    if (is.null(lp)) {
      stop("Linear predictors are missing from the fitted object.")
    }
  }
  # New data provided
  else {
    form <- object$formula
    if (is.null(form) && !is.null(object$call)) {
      # Attempt to extract formula from call
      if ("formula" %in% names(object$call)) {
        form <- eval(object$call$formula)
      } else {
        # Positional argument assumption (usually arg 2)
        try_form <- try(eval(object$call[[2]]), silent = TRUE)
        if (inherits(try_form, "formula")) form <- try_form
      }
    }

    if (is.null(form)) {
      stop("Cannot predict on 'newdata': The fitted object does not contain a
           formula or a valid call to reconstruct the model matrix.")
    }

    # Remove response
    form <- stats::delete.response(stats::terms(form))

    # Build Model Matrix
    mf <- stats::model.frame(form, data = newdata, na.action = stats::na.pass)
    X <- stats::model.matrix(form, mf)

    # Handle Intercept
    if (attr(stats::terms(form), "intercept") == 1) {
      int_idx <- match("(Intercept)", colnames(X))
      if (!is.na(int_idx)) {
        X <- X[, -int_idx, drop = FALSE]
      }
    }

    # Validate dimensions
    beta <- object$coefficients
    if (ncol(X) != length(beta)) {
      stop(sprintf("Dimension mismatch: 'newdata' generated %d coefficients,
                   but model has %d.",
                   ncol(X), length(beta)))
    }

    # Match names if possible
    if (!is.null(colnames(X)) && !is.null(names(beta))) {
      common_names <- intersect(colnames(X), names(beta))
      if (length(common_names) == length(beta)) {
        X <- X[, names(beta), drop = FALSE]
      }
    }

    lp <- as.vector(X %*% beta)
  }

  # Return based on type
  if (type == "lp") {
    return(lp)
  } else if (type == "risk") {
    return(exp(lp))
  }
}
