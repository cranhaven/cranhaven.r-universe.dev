#' Normal Discriminant Function Approach for Estimating Odds Ratio with Exposure
#' Measured in Pools and Potentially Subject to Additive Normal Errors
#'
#' Assumes exposure given covariates and outcome is a normal-errors linear
#' regression. Pooled exposure measurements can be assumed precise or subject to
#' additive normal processing error and/or measurement error. Parameters are
#' estimated using maximum likelihood.
#'
#'
#' @param g Numeric vector of pool sizes, i.e. number of members in each pool.
#' @param y Numeric vector of poolwise Y values (number of cases in each pool).
#' @param xtilde Numeric vector (or list of numeric vectors, if some pools have
#' replicates) with Xtilde values.
#' @param c Numeric matrix with poolwise \strong{C} values (if any), with one
#' row for each pool. Can be a vector if there is only 1 covariate.
#' @param constant_or Logical value for whether to assume a constant odds ratio
#' for X, which means that sigsq_1 = sigsq_0. If \code{NULL}, model is fit with
#' and without this assumption, and a likelihood ratio test is performed to test
#' it.
#' @param errors Character string specifying the errors that X is subject to.
#' Choices are \code{"neither"}, \code{"processing"} for processing error only,
#' \code{"measurement"} for measurement error only, and \code{"both"}.
#' @param start_nonvar_var Numeric vector of length 2 specifying starting value
#' for non-variance terms and variance terms, respectively.
#' @param lower_nonvar_var Numeric vector of length 2 specifying lower bound for
#' non-variance terms and variance terms, respectively.
#' @param upper_nonvar_var Numeric vector of length 2 specifying upper bound for
#' non-variance terms and variance terms, respectively.
#' @param jitter_start Numeric value specifying standard deviation for mean-0
#' normal jitters to add to starting values for a second try at maximizing the
#' log-likelihood, should the initial call to \code{\link[stats]{nlminb}} result
#' in non-convergence. Set to \code{NULL} for no second try.
#' @param nlminb_list List of arguments to pass to \code{\link[stats]{nlminb}}
#' for log-likelihood maximization.
#' @param hessian_list List of arguments to pass to
#' \code{\link[numDeriv]{hessian}} for approximating the Hessian matrix. Only
#' used if \code{estimate_var = TRUE}.
#' @param nlminb_object Object returned from \code{\link[stats]{nlminb}} in a
#' prior call. Useful for bypassing log-likelihood maximization if you just want
#' to re-estimate the Hessian matrix with different options.
#'
#'
#' @return List containing:
#' \enumerate{
#' \item Numeric vector of parameter estimates.
#' \item Variance-covariance matrix.
#' \item Returned \code{\link[stats]{nlminb}} object from maximizing the
#' log-likelihood function.
#' \item Akaike information criterion (AIC).
#' }
#'
#' If \code{constant_or = NULL}, two such lists are returned (one under a
#' constant odds ratio assumption and one not), along with a likelihood ratio
#' test for \code{H0: sigsq_1 = sigsq_0}, which is equivalent to
#' \code{H0: odds ratio is constant}.
#'
#'
#' @examples
#' # Load data frame with (g, Y, X, Xtilde, C) values for 4,996 pools and list
#' # of Xtilde values where 25 subjects have replicates. Xtilde values are
#' # affected by processing error and measurement error. True log-OR = 0.5,
#' # sigsq = 1, sigsq_p = 0.5, sigsq_m = 0.1.
#' data(dat_p_ndfa)
#' dat <- dat_p_ndfa$dat
#' reps <- dat_p_ndfa$reps
#'
#' # Unobservable truth estimator - use precise X's
#' fit.unobservable <- p_ndfa(
#'   g = dat$g,
#'   y = dat$numcases,
#'   xtilde = dat$x,
#'   c = dat$c,
#'   errors = "neither"
#' )
#' fit.unobservable$estimates
#'
#' # Naive estimator - use imprecise Xtilde's, but treat as precise
#' fit.naive <- p_ndfa(
#'   g = dat$g,
#'   y = dat$numcases,
#'   xtilde = dat$xtilde,
#'   c = dat$c,
#'   errors = "neither"
#' )
#' fit.naive$estimates
#'
#' # Corrected estimator - use Xtilde's and account for errors (not using
#' # replicates here)
#' \dontrun{
#' fit.noreps <- p_ndfa(
#'   g = dat$g,
#'   y = dat$numcases,
#'   xtilde = dat$xtilde,
#'   c = dat$c,
#'   errors = "both"
#' )
#' fit.noreps$estimates
#'
#' # Corrected estimator - use Xtilde's including 25 replicates
#' fit.reps <- p_ndfa(
#'   g = dat$g,
#'   y = dat$numcases,
#'   xtilde = reps,
#'   c = dat$c,
#'   errors = "both"
#' )
#' fit.reps$estimates
#'
#' # Same as previous, but allowing for non-constant odds ratio.
#' fit.nonconstant <- p_ndfa(
#'   g = dat$g,
#'   y = dat$numcases,
#'   xtilde = reps,
#'   c = dat$c,
#'   constant_or = FALSE,
#'   errors = "both"
#' )
#' fit.nonconstant$estimates
#'
#' # Visualize estimated log-OR vs. X based on previous model fit
#' p <- plot_ndfa(
#'   estimates = fit.nonconstant$estimates,
#'   varcov = fit.nonconstant$theta.var,
#'   xrange = range(dat$xtilde[dat$g == 1]),
#'   cvals = mean(dat$c / dat$g)
#' )
#' p
#'
#' # Likelihood ratio test for H0: odds ratio is constant.
#' test.constantOR <- p_ndfa(
#'   g = dat$g,
#'   y = dat$numcases,
#'   xtilde = reps,
#'   c = dat$c,
#'   constant_or = NULL,
#'   errors = "both"
#' )
#' test.constantOR$lrt
#' }
#'
#'
#' @references
#' Lyles, R.H., Van Domelen, D.R., Mitchell, E.M. and Schisterman, E.F. (2015)
#' "A discriminant function approach to adjust for processing and measurement
#' error When a biomarker is assayed in pooled samples."
#' \emph{Int. J. Environ. Res. Public Health} \strong{12}(11): 14723--14740.
#'
#' Schisterman, E.F., Vexler, A., Mumford, S.L. and Perkins, N.J. (2010) "Hybrid
#' pooled-unpooled design for cost-efficient measurement of biomarkers."
#' \emph{Stat. Med.} \strong{29}(5): 597--613.
#'
#'
#' @export
p_ndfa <- function(
  g,
  y,
  xtilde,
  c = NULL,
  constant_or = TRUE,
  errors = "processing",
  start_nonvar_var = c(0.01, 1),
  lower_nonvar_var = c(-Inf, 1e-4),
  upper_nonvar_var = c(Inf, Inf),
  jitter_start = 0.01,
  nlminb_list = list(control = list(trace = 1, eval.max = 500, iter.max = 500)),
  hessian_list = list(method.args = list(r = 4)),
  nlminb_object = NULL
) {

  # Check that inputs are valid
  if (! is.null(constant_or) && ! is.logical(constant_or)) {
    stop("The input 'contant_or' should be set to TRUE, FALSE, or NULL.")
  }
  if (! errors %in% c("neither", "processing", "measurement", "both")) {
    stop("The input 'errors' should be set to 'neither', 'processing',
         'measurement', or 'both'.")
  }
  if (! (is.numeric(start_nonvar_var) & length(start_nonvar_var) == 2)) {
    stop("The input 'start_nonvar_var' should be a numeric vector of length 2.")
  }
  if (! (is.numeric(lower_nonvar_var) & length(lower_nonvar_var) == 2)) {
    stop("The input 'lower_nonvar_var' should be a numeric vector of length 2.")
  }
  if (! (is.numeric(upper_nonvar_var) & length(upper_nonvar_var) == 2)) {
    stop("The input 'upper_nonvar_var' should be a numeric vector of length 2.")
  }
  if (! is.null(jitter_start) & jitter_start <= 0) {
    stop("The input 'jitter_start' should be a non-negative value, if specified.")
  }

  if (is.null(constant_or)) {

    # Fit model with constant odds ratio
    fit.constant <- p_ndfa_constant(
      g = g,
      y = y,
      xtilde = xtilde,
      c = c,
      errors = errors,
      start_nonvar_var = start_nonvar_var,
      lower_nonvar_var = lower_nonvar_var,
      upper_nonvar_var = upper_nonvar_var,
      nlminb_list = nlminb_list,
      hessian_list = hessian_list,
      nlminb_object = nlminb_object
    )

    # Fit model with non-constant odds ratio
    fit.nonconstant <- p_ndfa_nonconstant(
      g = g,
      y = y,
      xtilde = xtilde,
      c = c,
      errors = errors,
      start_nonvar_var = start_nonvar_var,
      lower_nonvar_var = lower_nonvar_var,
      upper_nonvar_var = upper_nonvar_var,
      nlminb_list = nlminb_list,
      hessian_list = hessian_list,
      nlminb_object = nlminb_object
    )

    # Likelihood ratio test for H0: sigsq_1 = sigsq_0, which is equivalent to
    # H0: constant odds ratio
    d <- 2 * (-fit.nonconstant$nlminb.object$objective + fit.constant$nlminb.object$objective)
    p <- pchisq(q = d, df = 1, lower.tail = FALSE)
    if (p < 0.05) {
      message <- "H0: Odds ratio is constant rejected at alpha = 0.05."
    } else {
      message <- "H0: Odds ratio is constant not rejected at alpha = 0.05."
    }
    message(message)
    lrt <- list(d = d, p = p, message = message)

    ret.list <- list(fit.constant = fit.constant,
                     fit.nonconstant = fit.nonconstant,
                     lrt = lrt)

  } else if (constant_or) {

    ret.list <- p_ndfa_constant(
      g = g,
      y = y,
      xtilde = xtilde,
      c = c,
      errors = errors,
      start_nonvar_var = start_nonvar_var,
      lower_nonvar_var = lower_nonvar_var,
      upper_nonvar_var = upper_nonvar_var,
      nlminb_list = nlminb_list,
      hessian_list = hessian_list,
      nlminb_object = nlminb_object
    )

  } else {

    ret.list <- p_ndfa_nonconstant(
      g = g,
      y = y,
      xtilde = xtilde,
      c = c,
      errors = errors,
      start_nonvar_var = start_nonvar_var,
      lower_nonvar_var = lower_nonvar_var,
      upper_nonvar_var = upper_nonvar_var,
      nlminb_list = nlminb_list,
      hessian_list = hessian_list,
      nlminb_object = nlminb_object
    )

  }

  return(ret.list)

}
