#' Summarise a \code{dsspMod} model
#'
#' @param object an object of class \code{dsspMod}
#' @param prob the desired probability to be covered by the credible intervals.
#'   The default is 0.95.
#' @param robust whether or not to use the median (rather than the mean) to
#'   calculate the estimates that summarise the posterior.
#'   Default to \code{FALSE}.
#' @param mc_se whether or not to include the uncertainty in \code{Estimate}
#'   caused by sampling should be shown in the summary. Defaults to \code{FALSE}.
#' @param ... additional arguments which are ignored.
#'
#' @return An object of class "dsspModsummary". Provides a summary of the the Direct Sampling Spatial Prior (DSSP) model. Includes details of the formula used to fit the model, and a summary of the model (\eqn{eta, delta}) and the covariates.
#' @export
#'
#' @examples
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#'
#' f <- function(x) -x ## log-prior for exponential distribution for the smoothing parameter
#'
#' ## Draw 100 samples from the posterior of eta given the data y.
#' OUTPUT <- DSSP(
#'   formula = log(zinc) ~ 1, data = meuse.all, N = 100,
#'   pars = c(0.001, 0.001), log_prior = f
#' )
#' summary(OUTPUT)
summary.dsspMod <- function(object, prob = 0.95, robust = FALSE, mc_se = FALSE, ...) {
  probs <- validate_ci_bounds(prob)

  variables <- list(eta = object$eta, delta = object$delta)

  measures <- list()
  if (robust) {
    measures$Estimate <- stats::median
    if (mc_se) {
      measures$MCSE <- posterior::mcse_median
    }
    measures$Est.Error <- stats::mad
  } else {
    measures$Estimate <- mean
    if (mc_se) {
      measures$MCSE <- posterior::mcse_mean
    }
    measures$Est.Error <- stats::sd
  }
  full_summary_measures <- c(measures, list(
    ll = function(x) stats::quantile(x, probs = probs[1]),
    ul = function(x) stats::quantile(x, probs = probs[2]),
    ESS = ess
  ))

  full_summary <- lapply(full_summary_measures, function(m) sapply(variables, function(v) m(v)))
  full_summary <- as.data.frame(full_summary)

  prob <- probs[2] - probs[1]

  names(full_summary)[which(names(full_summary) %in% c("ll", "ul"))] <- paste0(c("l-", "u-"), prob * 100, "% CI")
  rownames(full_summary) <- names(variables)

  cov_measures <- c(measures, list(
    min = min,
    `q0.025` = function(x) stats::quantile(x, probs = 0.025),
    `q0.25` = function(x) stats::quantile(x, probs = 0.25),
    `q0.50` = function(x) stats::quantile(x, probs = 0.50),
    `q0.75` = function(x) stats::quantile(x, probs = 0.75),
    `q0.975` = function(x) stats::quantile(x, probs = 0.975),
    max = max
  ))

  n_covariates <- nrow(object$covariates_posterior)
  posterior <- object$covariates_posterior

  cov_list <- stats::setNames(
    split(posterior, seq(nrow(posterior))),
    rownames(posterior)
  )
  cov_summary <- lapply(cov_measures, function(m) sapply(cov_list, function(v) m(v)))
  cov_summary <- as.data.frame(cov_summary)

  out <- list(
    formula = object$formula,
    nobs = object$nobs,
    niter = object$N,
    full_summary = full_summary,
    cov_summary = cov_summary
  )

  class(out) <- "dsspModsummary"
  out
}

print.dsspModsummary <- function(x, digits = 2, ...) {
  cat("Formula: ")
  print(x$formula)
  cat(paste0("Number of observations: ", x$nobs, "\n"))
  cat(paste0("Number of iterations: ", x$niter, "\n\n"))
  cat("Summary of model:\n")
  print_format(x$full_summary)
  if (!is.null(x$cov_summary)) {
    cat("\nSummary of covariates:\n")
    print_format(x$cov_summary)
  }
}
