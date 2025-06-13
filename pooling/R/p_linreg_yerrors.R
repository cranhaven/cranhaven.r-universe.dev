#' Linear Regression of Y vs. Covariates with Y Measured in Pools and
#' (Potentially) Subject to Additive Normal Errors
#'
#' Assumes outcome given covariates is a normal-errors linear regression. Pooled
#' outcome measurements can be assumed precise or subject to additive normal
#' processing error and/or measurement error. Replicates are supported.
#'
#' The individual-level model of interest for Y|\strong{X} is:
#'
#' Y = beta_0 + \strong{beta_x}^T \strong{X} + e, e ~ N(0, sigsq)
#'
#' The implied model for summed Y*|\strong{X*} in a pool with g members is:
#'
#' Y* = g beta_0 + \strong{beta_x}^T \strong{X*} + e*, e* ~ N(0, g sigsq)
#'
#' The assay targets Ybar, the mean Y value for each pool, from which the sum Y*
#' can be calculated as Y* = g Ybar. But the Ybar's may be subject to processing
#' error and/or measurement error. Suppose Ybartilde is the imprecise version of
#' Ybar from the assay. If both errors are present, the assumed error structure
#' is:
#'
#' Ybartilde = Ybar + e_p I(g > 1) + e_m, e_p ~ N(0, sigsq_p),
#' e_m ~ N(0, sigsq_m)
#'
#' with the processing error e_p and measurement error e_m assumed independent
#' of each other. This motivates a maximum likelihood analysis for estimating
#' \strong{theta} = (beta_0, \strong{beta_x}^T)^T based on observed
#' (Ytilde*, \strong{X}*) values, where Ytilde* = g Ytildebar.
#'
#'
#' @param g Numeric vector with pool sizes, i.e. number of members in each pool.
#' @param ytilde Numeric vector (or list of numeric vectors, if some pools have
#' replicates) with poolwise sum \code{Ytilde} values.
#' @param x Numeric matrix with poolwise \strong{\code{X}} values (if any), with
#' one row for each pool. Can be a vector if there is only 1 covariate.
#' @param errors Character string specifying the errors that \code{Y} is subject
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
#' @param nlminb_list List of arguments to pass to \code{\link[stats]{nlminb}}
#' for log-likelihood maximization.
#' @param hessian_list List of arguments to pass to
#' \code{\link[numDeriv]{hessian}}.
#'
#'
#' @return
#' List containing:
#' \enumerate{
#' \item Numeric vector of parameter estimates.
#' \item Variance-covariance matrix (if \code{estimate_var = TRUE}).
#' \item Returned \code{\link[stats]{nlminb}} object from maximizing the
#' log-likelihood function.
#' \item Akaike information criterion (AIC).
#' }
#'
#'
#' @references
#' Schisterman, E.F., Vexler, A., Mumford, S.L. and Perkins, N.J. (2010) "Hybrid
#' pooled-unpooled design for cost-efficient measurement of biomarkers."
#' \emph{Stat. Med.} \strong{29}(5): 597--613.
#'
#'
#' @examples
#' # Load dataset containing data frame with (g, X1*, X2*, Y*, Ytilde*) values
#' # for 500 pools each of size 1, 2, and 3, and list of Ytilde values where 20
#' # of the single-specimen pools have replicates. Ytilde values are affected by
#' # processing error and measurement error; true parameter values are
#' # beta_0 = 0.25, beta_x1 = 0.5, beta_x2 = 0.25, sigsq = 1.
#' data(dat_p_linreg_yerrors)
#' dat <- dat_p_linreg_yerrors$dat
#' reps <- dat_p_linreg_yerrors$reps
#'
#' # Fit Ytilde* vs. (X1*, X2*) ignoring errors in Ytilde (leads to loss of
#' # precision and overestimated sigsq, but no bias).
#' fit.naive <- p_linreg_yerrors(
#'   g = dat$g,
#'   y = dat$y,
#'   x = dat[, c("x1", "x2")],
#'   errors = "neither"
#' )
#' fit.naive$theta.hat
#'
#' # Account for errors in Ytilde*, without using replicates
#' fit.corrected.noreps <- p_linreg_yerrors(
#'   g = dat$g,
#'   y = dat$ytilde,
#'   x = dat[, c("x1", "x2")],
#'   errors = "both"
#' )
#' fit.corrected.noreps$theta.hat
#'
#' # Account for errors in Ytilde*, incorporating the 20 replicates
#' fit.corrected.reps <- p_linreg_yerrors(
#'   g = dat$g,
#'   y = reps,
#'   x = dat[, c("x1", "x2")],
#'   errors = "both"
#' )
#' fit.corrected.reps$theta.hat
#'
#' # In this trial, incorporating replicates resulted in much better estimates
#' # of sigsq (truly 1), sigsq_p (truly 0.4), and sigsq_m (truly = 0.2) but very
#' # similar regression coefficient estimates.
#' fit.corrected.noreps$theta.hat
#' fit.corrected.reps$theta.hat
#'
#'
#' @export
p_linreg_yerrors <- function(
  g,
  ytilde,
  x = NULL,
  errors = "processing",
  estimate_var = TRUE,
  start_nonvar_var = c(0.01, 1),
  lower_nonvar_var = c(-Inf, 1e-4),
  upper_nonvar_var = c(Inf, Inf),
  nlminb_list = list(control = list(trace = 1, eval.max = 500, iter.max = 500)),
  hessian_list = list(method.args = list(r = 4))
) {

  # Check that inputs are valid
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

  # Sample size
  n <- length(ytilde)

  # Get number of X variables (and assign names)
  if (is.null(x)) {
    n.xvars <- 0
    x.varnames <- NULL
  } else {
    x.varname <- deparse(substitute(x))
    if (! is.matrix(x)) {
      x <- as.matrix(x)
    }
    n.xvars <- ncol(x)
    x.varnames <- colnames(x)
    if (is.null(x.varnames)) {
      if (n.xvars == 1) {
        if (length(grep("$", x.varname, fixed = TRUE)) > 0) {
          x.varname <- substr(x.varname,
                              start = which(unlist(strsplit(x.varname, "")) == "$") + 1,
                              stop = nchar(x.varname))
        }
        x.varnames <- x.varname
      } else {
        x.varnames <- paste("x", 1: n.xvars, sep = "")
      }
    }
  }

  # Get number of betas
  n.betas <- 1 + n.xvars

  # Create vector indicating which observations are pools
  Ig <- ifelse(g > 1, 1, 0)

  # Create matrix of (g, X) values
  gx <- cbind(g, x)

  # Separate into replicates and singles
  class.ytilde <- class(ytilde)
  if (class.ytilde == "list") {
    k <- sapply(ytilde, length)
    which.r <- which(k > 1)
    n.r <- length(which.r)
    some.r <- n.r > 0
    if (some.r) {

      # Replicates
      g.r <- g[which.r]
      Ig.r <- Ig[which.r]
      k.r <- k[which.r]
      ytilde.r <- ytilde[which.r]
      gx.r <- gx[which.r, , drop = FALSE]

    }
    n <- n - n.r
    some.s <- n > 0
    if (some.s) {

      # Singles
      g <- g[-which.r]
      Ig <- Ig[-which.r]
      ytilde <- unlist(ytilde[-which.r])
      gx <- gx[-which.r, , drop = FALSE]

    }
  } else {
    some.r <- FALSE
    some.s <- TRUE
  }

  # Get indices for parameters being estimated and create labels
  loc.betas <- 1: n.betas
  beta.labels <- paste("beta", c("0", x.varnames), sep = "_")

  loc.sigsq <- n.betas + 1

  if (errors == "neither") {
    theta.labels <- c(beta.labels, "sigsq")
  } else if (errors == "processing") {
    theta.labels <- c(beta.labels, "sigsq", "sigsq_p")
  } else if (errors == "measurement") {
    theta.labels <- c(beta.labels, "sigsq", "sigsq_m")
  } else if (errors == "both") {
    theta.labels <- c(beta.labels, "sigsq", "sigsq_p", "sigsq_m")
  }

  # Log-likelihood function
  llf <- function(f.theta) {

    # Extract parameters
    f.betas <- matrix(f.theta[loc.betas], ncol = 1)
    f.sigsq <- f.theta[loc.sigsq]

    if (errors == "neither") {
      f.sigsq_p <- 0
      f.sigsq_m <- 0
    } else if (errors == "measurement") {
      f.sigsq_p <- 0
      f.sigsq_m <- f.theta[loc.sigsq + 1]
    } else if (errors == "processing") {
      f.sigsq_p <- f.theta[loc.sigsq + 1]
      f.sigsq_m <- 0
    } else if (errors == "both") {
      f.sigsq_p <- f.theta[loc.sigsq + 1]
      f.sigsq_m <- f.theta[loc.sigsq + 2]
    }

    # Likelihood:
    # L_i = f(Ytilde|X)

    if (some.r) {

      ll.vals <- c()
      for (ii in 1: length(ytilde.r)) {

        # Values for ith subject
        g_i <- g.r[ii]
        Ig_i <- Ig.r[ii]
        k_i <- k.r[ii]
        ytilde_i <- ytilde.r[[ii]]
        gx_i <- gx.r[ii, ]

        # E(Ytilde|X) and V(Ytilde|X)
        Mu_ytilde.x <- matrix(gx_i %*% f.betas, nrow = k_i)
        Sigma_ytilde.x <- g_i * f.sigsq +
          g_i^2 * f.sigsq_p * Ig_i +
          diag(g_i^2 * f.sigsq_m, k_i)

        # Log-likelihood
        ll.vals[ii] <- dmvnorm(x = ytilde_i, log = TRUE,
                               mean = Mu_ytilde.x,
                               sigma = Sigma_ytilde.x)

      }
      ll.r <- sum(ll.vals)

    } else {
      ll.r <- 0
    }

    if (some.s) {

      # E(Ytilde|X) and V(Ytilde|X)
      mu_ytilde.x <- gx %*% f.betas
      sigsq_ytilde.x <- g * f.sigsq + g^2 * f.sigsq_p * Ig + g^2 * f.sigsq_m

      # Log-likelihood
      ll.s <- sum(dnorm(x = ytilde, log = TRUE,
                        mean = mu_ytilde.x, sd = sqrt(sigsq_ytilde.x)))

    } else {
      ll.s <- 0
    }

    # Return negative log-likelihood
    ll <- ll.r + ll.s
    return(-ll)

  }

  # Starting values
  if (is.null(nlminb_list$start)) {
    if (errors == "neither") {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas),
                             start_nonvar_var[2])
    } else if (errors %in% c("measurement", "processing")) {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas),
                             rep(start_nonvar_var[2], 2))
    } else if (errors == "both") {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas),
                             rep(start_nonvar_var[2], 3))
    }
  }
  names(nlminb_list$start) <- theta.labels

  # Lower bounds
  if (is.null(nlminb_list$lower)) {
    if (errors == "neither") {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas),
                             lower_nonvar_var[2])
    } else if (errors %in% c("measurement", "processing")) {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas),
                             rep(lower_nonvar_var[2], 2))
    } else if (errors == "both") {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas),
                             rep(lower_nonvar_var[2], 3))
    }
  }

  # Upper bounds
  if (is.null(nlminb_list$upper)) {
    if (errors == "neither") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas),
                             upper_nonvar_var[2])
    } else if (errors %in% c("measurement", "processing")) {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas),
                             rep(upper_nonvar_var[2], 2))
    } else if (errors == "both") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas),
                             rep(upper_nonvar_var[2], 3))
    }
  }

  # Obtain ML estimates
  ml.max <- do.call(nlminb, c(list(objective = llf), nlminb_list))

  # Print message if nlminb indicates non-convergence
  if (ml.max$convergence == 1) {
    message("Object returned by 'nlminb' function indicates non-convergence. You may want to try different starting values.")
  }

  # Create list to return
  theta.hat <- ml.max$par
  names(theta.hat) <- theta.labels
  ret.list <- list(theta.hat = theta.hat)

  # If requested, add variance-covariance matrix to ret.list
  if (estimate_var) {

    # Estimate Hessian
    hessian.mat <- do.call(numDeriv::hessian,
                           c(list(func = llf, x = theta.hat),
                             hessian_list))

    # Estimate variance-covariance matrix
    theta.variance <- try(solve(hessian.mat), silent = TRUE)
    if (class(theta.variance)[1] == "try-error" | sum(is.na(hessian.mat)) > 0) {
      message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
      ret.list$theta.var <- NULL
    } else {
      colnames(theta.variance) <- rownames(theta.variance) <- theta.labels
      ret.list$theta.var <- theta.variance
    }

  }

  # Add nlminb object and AIC to ret.list
  ret.list$nlminb.object <- ml.max
  ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)

  # Return ret.list
  return(ret.list)

}
