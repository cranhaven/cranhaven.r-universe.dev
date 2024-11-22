#' Fit proportional hazards survival models
#'
#' @param formula A model formula, where the left-hand side is a \link[survival]{Surv} object.
#' @param distribution A parametric distribution for the baseline hazard.
#' Possible values are \code{exponential}, \code{weibull}, \code{gompertz}, \code{rp}, \code{logchazard}, and \code{loghazard}.
#' \code{rp} is equivalent to a \code{logchazard} model with restricted cubic splines (argument \code{rcs = TRUE}).
#' @param df Represents the number of degrees of freedom used for the restricted cubic splines when flexibly modelling the baseline hazard.
#' Only required when \code{rcs = TRUE}.
#' @param powers A vector representing the degree of the fractional polynomials used to model the baseline hazard (with a maximum degree of 2).
#' Only required when \code{rcs = FALSE}.
#' @param rcs Use restricted cubic splines when flexibly modelling the baseline hazard? Defaults to \code{TRUE}, and the alternative is using fractional polynomials.
#' @param data A data frame containing all variables required for fitting the model.
#' Can be a \code{tibble} object.
#' @param from.null A vector of starting values for the null model (used to get improved starting values). This is mostly useful when experiencing issues with default starting values or convergence issues.
#' @param ... Further arguments passed to \link{merlin}.
#'
#' @return An object of class \code{mlsurv}.
#' @export
#'
#' @examples
#' # Weibull model
#' library(survival)
#' data("pbc.merlin", package = "merlin")
#' fit <- mlsurv(
#'   formula = Surv(stime, died) ~ trt,
#'   distribution = "weibull",
#'   data = pbc.merlin
#' )
#' summary(fit)
#'
#' # Royston-Parmar model with 3 degrees of freedom
#' fit <- mlsurv(
#'   formula = Surv(stime, died) ~ trt,
#'   distribution = "rp",
#'   df = 3,
#'   data = pbc.merlin
#' )
#' summary(fit)
#'
#' \dontrun{
#' # Flexible parametric model on the log-hazard scale with fractional polynomials
#' fit <- mlsurv(
#'   formula = Surv(stime, died) ~ trt,
#'   distribution = "loghazard",
#'   powers = c(0, 1),
#'   rcs = FALSE,
#'   data = pbc.merlin
#' )
#' summary(fit)
#' }
mlsurv <- function(formula, distribution, df = NULL, powers = NULL, rcs = TRUE, data, from.null = NULL, ...) {
  ### Check for random effects
  if (grepl("M[[:alnum:]]\\[", x = paste(as.character(formula)[c(2, 1, 3)], collapse = ""))) stop("Random effects 'M*[]' are not currently supported.", call. = FALSE)

  ### Currently supported distributions
  distribution <- tryCatch(
    expr = {
      match.arg(arg = distribution, choices = c("exponential", "weibull", "gompertz", "rp", "logchazard", "loghazard"))
    },
    error = function(e) stop("Distribution not currently supported, see ?mlsurv for more details.", call. = FALSE)
  )

  ### if 'rp' then only 'rcs' is supported
  if (distribution == "rp" & isFALSE(rcs)) stop("Royston-Parmar models require splines 'rcs = TRUE'.", call. = FALSE)

  ### If distribution %in% 'rp', 'logchazard' 'loghazard' then 'df' is required
  if (distribution %in% c("rp", "logchazard", "loghazard")) {
    if (!is.logical(rcs)) stop("'rcs' must be either TRUE or FALSE", call. = FALSE)
    if (rcs) {
      if (is.null(df)) stop("'df' is required for distributions 'rp', 'loghazard', 'logchazard' when using restricted cubic splines ('rcs = TRUE').", call. = FALSE)
      if (length(df) != 1) stop("'df' must be a single value.", call. = FALSE)
      if (!is.numeric(df) | (df < 1 | df > 10)) stop("'df' must be a numeric value between 1 and 10.", call. = FALSE)
    } else {
      if (is.null(powers)) stop("'powers' is required for distributions 'rp', 'loghazard', 'logchazard' when using restricted cubic splines ('rcs = TRUE').", call. = FALSE)
    }
  }

  ### Get better starting values
  # If distribution %in% 'rp', 'loghazard' then also extract timevar, and need to update formula to add rcs (or fp) to rhs of formula
  if (distribution %in% c("rp", "logchazard", "loghazard")) {
    timevar <- as.character(formula[[2]][[2]])
    if (rcs) {
      flexible.term <- paste0("rcs(", timevar, ", df = ", df, ", log = T, event = T)")
    } else {
      flexible.term <- paste0("fp(", timevar, ", powers = c(", paste(powers, collapse = ","), "))")
    }
    ext.formula <- paste0(paste(as.character(formula)[c(2, 1, 3)], collapse = ""), "+", flexible.term)
    null.formula <- paste0(paste(as.character(formula)[c(2, 1)], collapse = ""), flexible.term)
    null.coef <- stats::coef(merlin(model = stats::as.formula(null.formula), family = ifelse(distribution == "logchazard", "rp", distribution), timevar = timevar, data = data, from = from.null))
  } else {
    null.coef <- stats::coef(merlin(model = stats::update.formula(formula, ~1), family = distribution, data = data, from = from.null))
  }
  # Fit empty Cox model (with large eps)
  cox.coef <- stats::coef(survival::coxph(formula = formula, data = data, y = FALSE, eps = 0.1))
  # Define new starting values
  from <- c(cox.coef, null.coef)

  ### Fit model using merlin
  if (distribution %in% c("rp", "loghazard", "logchazard")) {
    fit <- merlin(model = stats::as.formula(ext.formula), family = ifelse(distribution == "logchazard", "rp", distribution), timevar = timevar, data = data, from = from, ...)
  } else {
    fit <- merlin(model = formula, family = distribution, data = data, from = from, ...)
  }
  fit$distribution <- distribution

  ### Return mlsurv object
  class(fit) <- "mlsurv"
  return(fit)
}

#' @title Print \code{mlsurv} Fits
#' @description Print the coefficients from a \code{mlsurv} fit.
#'
#' @param x An object of class \code{mlsurv}.
#' @param digits The number of significant digits to use when printing.
#' @param simplify Should non-interpretable coefficients be hidden (e.g. splines and flexible polynomials terms)? Defaults to TRUE.
#' @param ... Not used.
#'
#' @export
print.mlsurv <- function(x, digits = max(3L, getOption("digits") - 3L), simplify = TRUE, ...) {
  cat("Proportional hazards regression model\n")
  if (x$distribution == "rp") x$distribution.print <- "Flexible parametric (Royston-Parmar)"
  if (x$distribution == "loghazard") x$distribution.print <- "Flexible parametric (log-hazard)"
  if (x$distribution == "logchazard") x$distribution.print <- "Flexible parametric (cumulative log-hazard)"
  if (!(x$distribution %in% c("rp", "loghazard", "logchazard"))) x$distribution.print <- x$distribution
  cat(tools::toTitleCase(x$distribution.print), "baseline hazard\n")
  cat("Data:", x$data, "\n\n")
  cat("Coefficients:\n")
  if (simplify) {
    if (x$distribution %in% c("rp", "loghazard", "logchazard")) x$coefficients <- x$coefficients[!grepl("^_cons|^rcs()|^fp()", names(x$coefficients))]
  }
  print.default(format(stats::coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  invisible(x)
}

#' @title Summarizing \code{mlsurv} Fits
#' @description These functions are all methods for class \code{mlsurv} or \code{summary.mlsurv} objects.
#' @param object An object of class \code{mlsurv}
#' @param sig Significancy level for confidence intervals. Defaults to 0.95.
#' @param ... Not used.
#'
#' @export
summary.mlsurv <- function(object, sig = 0.95, ...) {
  out <- summary.merlin(object = object, sig = sig, ...)
  class(out) <- c("summary.mlsurv", class(out))
  return(out)
}

#' @param x An object of class \code{summary.mlsurv}
#' @param digits The number of significant digits to use when printing.
#' @param simplify Should non-interpretable coefficients be hidden (e.g. splines and flexible polynomials terms)? Defaults to TRUE.
#' @rdname summary.mlsurv
#' @export
print.summary.mlsurv <- function(x, digits = max(3, getOption("digits") - 3), simplify = TRUE, ...) {
  cat("Proportional hazards regression model\n")
  if (x$distribution == "rp") x$distribution.print <- "Flexible parametric (Royston-Parmar)"
  if (x$distribution == "loghazard") x$distribution.print <- "Flexible parametric (log-hazard)"
  if (x$distribution == "logchazard") x$distribution.print <- "Flexible parametric (cumulative log-hazard)"
  if (!(x$distribution %in% c("rp", "loghazard", "logchazard"))) x$distribution.print <- x$distribution
  cat(tools::toTitleCase(x$distribution.print), "baseline hazard\n")
  cat("Log likelihood =", stats::logLik(x))
  cat("\n\n")

  if (simplify) {
    if (x$distribution %in% c("rp", "loghazard", "logchazard")) x$coefftable <- x$coefftable[!grepl("^_cons|^rcs()|^fp()", rownames(x$coefftable)), , drop = FALSE]
  }

  stats::printCoefmat(x$coefftable, digits = digits, na.print = "NA", cs.ind = c(1, 2, 5, 6), tst.ind = 3, zap.ind = 4)

  if (x$convergence != 0) warning("Model did not converge.", call. = FALSE)
}

#' @title Extract Model Coefficients
#' @description \code{coef} extracts model coefficients from a \code{mlsurv} model fit. \code{coefficients} is an alias for it.
#' @param object An object of class \code{mlsurv} or \code{summary.mlsurv}.
#' @param ... Not used.
#' @export
coef.mlsurv <- function(object, ...) object$coefficients

#' @rdname coef.mlsurv
#' @export
coef.summary.mlsurv <- function(object, ...) coef.merlin(object, ...)

#' @title Calculate Variance-Covariance Matrix for a \code{mlsurv} Model Object
#' @description Returns the variance-covariance matrix of all estimated parameters of a fitted \code{mlsurv} model.
#' @param object An object of class \code{mlsurv} or \code{summary.mlsurv}.
#' @param ... Not used.
#' @export
vcov.mlsurv <- function(object, ...) solve(object$hessian)

#' @rdname vcov.mlsurv
#' @export
vcov.summary.mlsurv <- function(object, ...) vcov.merlin(object$hessian)

#' @title Extract Log-Likelihood
#' @description Extract log-likelihood of a \code{mlsurv} model.
#' @param object An object of class \code{mlsurv} or \code{summary.mlsurv}.
#' @param ... Not used.
#' @export
logLik.mlsurv <- function(object, ...) object$loglikelihood

#' @rdname logLik.mlsurv
#' @export
logLik.summary.mlsurv <- function(object, ...) logLik.merlin(object, ...)
