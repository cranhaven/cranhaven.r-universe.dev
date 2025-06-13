#' Gamma Discriminant Function Approach for Estimating Odds Ratio with Exposure
#' Measured in Pools and Potentially Subject to Multiplicative Lognormal Errors
#'
#' Assumes exposure given covariates and outcome is a constant-scale Gamma
#' regression. Pooled exposure measurements can be assumed precise or subject to
#' multiplicative lognormal processing error and/or measurement error.
#' Parameters are estimated using maximum likelihood.
#'
#'
#' @param g Numeric vector with pool sizes, i.e. number of members in each pool.
#' @param y Numeric vector with poolwise Y values, coded 0 if all members are
#' controls and 1 if all members are cases.
#' @param xtilde Numeric vector (or list of numeric vectors, if some pools have
#' replicates) with Xtilde values.
#' @param c List where each element is a numeric matrix containing the
#' \strong{C} values for members of a particular pool (1 row for each member).
#' @param constant_or Logical value for whether to assume a constant OR for
#' X, which means that gamma_y = 0. If \code{NULL}, model is fit with and
#' without this assumption, and a likelihood ratio test is performed to test it.
#' @param errors Character string specifying the errors that \code{X} is subject
#' to. Choices are \code{"neither"}, \code{"processing"} for processing error
#' only, \code{"measurement"} for measurement error only, and \code{"both"}.
#' @param estimate_var Logical value for whether to return variance-covariance
#' matrix for parameter estimates.
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
#' @param hcubature_list List of arguments to pass to
#' \code{\link[cubature]{hcubature}} for numerical integration.
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
#' test for \code{H0: gamma_y = 0}, which is equivalent to
#' \code{H0: odds ratio is constant}.
#'
#'
#' @examples
#' # Load data frame with (g, Y, X, Xtilde) values for 496 pools, list of C
#' # values for members of each pool, and list of Xtilde values where 25
#' # single-specimen pools have replicates. Xtilde values are affected by
#' # processing error and measurement error. True log-OR = 0.5, sigsq_p = 0.25,
#' # sigsq_m = 0.1.
#' data(dat_p_gdfa)
#' dat <- dat_p_gdfa$dat
#' reps <- dat_p_gdfa$reps
#' c.list <- dat_p_gdfa$c.list
#'
#' # Unobservable truth estimator - use precise X's
#' fit.unobservable <- p_gdfa(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = dat$x,
#'   c = c.list,
#'   errors = "neither"
#' )
#' fit.unobservable$estimates
#'
#' # Naive estimator - use imprecise Xtilde's, but treat as precise
#' fit.naive <- p_gdfa(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = dat$xtilde,
#'   c = c.list,
#'   errors = "neither"
#' )
#' fit.naive$estimates
#'
#' # Corrected estimator - use Xtilde's and account for errors (not using
#' # replicates here)
#' \dontrun{
#' fit.noreps <- p_gdfa(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = dat$xtilde,
#'   c = c.list,
#'   errors = "both"
#' )
#' fit.noreps$estimates
#'
#' # Corrected estimator - use Xtilde's including 25 replicates
#' fit.reps <- p_gdfa(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = reps,
#'   c = c.list,
#'   errors = "both"
#' )
#' fit.reps$estimates
#'
#' # Same as previous, but allowing for non-constant odds ratio.
#' fit.nonconstant <- p_gdfa(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = reps,
#'   c = c.list,
#'   constant_or = FALSE,
#'   errors = "both",
#'   hcubature_list = list(tol = 1e-4)
#' )
#' fit.nonconstant$estimates
#'
#' # Visualize estimated log-OR vs. X based on previous model fit
#' p <- plot_gdfa(
#'   estimates = fit.nonconstant$estimates,
#'   varcov = fit.nonconstant$theta.var,
#'   xrange = range(dat$xtilde[dat$g == 1]),
#'   cvals = mean(unlist(c))
#' )
#' p
#' }
#'
#' @references
#' Lyles, R.H., Van Domelen, D.R., Mitchell, E.M. and Schisterman, E.F. (2015)
#' "A discriminant function approach to adjust for processing and measurement
#' error When a biomarker is assayed in pooled samples."
#' \emph{Int. J. Environ. Res. Public Health} \strong{12}(11): 14723--14740.
#'
#' Mitchell, E.M, Lyles, R.H., and Schisterman, E.F. (2015) "Positing, fitting,
#' and selecting regression models for pooled biomarker data." \emph{Stat. Med}
#' \strong{34}(17): 2544--2558.
#'
#' Schisterman, E.F., Vexler, A., Mumford, S.L. and Perkins, N.J. (2010) "Hybrid
#' pooled-unpooled design for cost-efficient measurement of biomarkers."
#' \emph{Stat. Med.} \strong{29}(5): 597--613.
#'
#' Whitcomb, B.W., Perkins, N.J., Zhang, Z., Ye, A., and Lyles, R. H. (2012)
#' "Assessment of skewed exposure in case-control studies with pooling."
#' \emph{Stat. Med.} \strong{31}: 2461--2472.
#'
#'
#' @export
p_gdfa <- function(
  g,
  y,
  xtilde,
  c = NULL,
  constant_or = TRUE,
  errors = "processing",
  estimate_var = TRUE,
  start_nonvar_var = c(0.01, 1),
  lower_nonvar_var = c(-Inf, 1e-4),
  upper_nonvar_var = c(Inf, Inf),
  jitter_start = 0.01,
  hcubature_list = list(tol = 1e-8),
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
  if (! is.logical(estimate_var)) {
    stop("The input 'estimate_var' should be TRUE or FALSE.")
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

  if (is.null(constant_or)) {

    # Fit model with constant odds ratio
    fit.constant <- p_gdfa_constant(
      g = g,
      y = y,
      xtilde = xtilde,
      c = c,
      errors = errors,
      estimate_var = estimate_var,
      start_nonvar_var = start_nonvar_var,
      lower_nonvar_var = lower_nonvar_var,
      upper_nonvar_var = upper_nonvar_var,
      hcubature_list = hcubature_list,
      nlminb_list = nlminb_list,
      hessian_list = hessian_list,
      nlminb_object = nlminb_object
    )

    # Fit model with non-constant odds ratio
    fit.nonconstant <- p_gdfa_nonconstant(
      g = g,
      y = y,
      xtilde = xtilde,
      c = c,
      errors = errors,
      estimate_var = estimate_var,
      start_nonvar_var = start_nonvar_var,
      lower_nonvar_var = lower_nonvar_var,
      upper_nonvar_var = upper_nonvar_var,
      hcubature_list = hcubature_list,
      nlminb_list = nlminb_list,
      hessian_list = hessian_list,
      nlminb_object = nlminb_object
    )

    # Likelihood ratio test for H0: gamma_y = 0, which is equivalent to
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

    ret.list <- p_gdfa_constant(
      g = g,
      y = y,
      xtilde = xtilde,
      c = c,
      errors = errors,
      estimate_var = estimate_var,
      start_nonvar_var = start_nonvar_var,
      lower_nonvar_var = lower_nonvar_var,
      upper_nonvar_var = upper_nonvar_var,
      hcubature_list = hcubature_list,
      nlminb_list = nlminb_list,
      hessian_list = hessian_list,
      nlminb_object = nlminb_object
    )

  } else {

    ret.list <- p_gdfa_nonconstant(
      g = g,
      y = y,
      xtilde = xtilde,
      c = c,
      errors = errors,
      estimate_var = estimate_var,
      start_nonvar_var = start_nonvar_var,
      lower_nonvar_var = lower_nonvar_var,
      upper_nonvar_var = upper_nonvar_var,
      hcubature_list = hcubature_list,
      nlminb_list = nlminb_list,
      hessian_list = hessian_list,
      nlminb_object = nlminb_object
    )

  }

  return(ret.list)

}
