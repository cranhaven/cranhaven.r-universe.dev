#' Normal Discriminant Function Approach for Estimating Odds Ratio with Exposure
#' Measured in Pools and Potentially Subject to Additive Normal Errors
#' (Non-constant Odds Ratio Version)
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
p_ndfa_nonconstant <- function(
  g,
  y,
  xtilde,
  c = NULL,
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

  # Get number of C variables (and assign names)
  if (is.null(c)) {
    c.varnames <- NULL
    n.cvars <- 0
  } else {
    c.varname <- deparse(substitute(c))
    if (! is.matrix(c)) {
      c <- as.matrix(c)
    }
    n.cvars <- ncol(c)
    c.varnames <- colnames(c)
    if (is.null(c.varnames)) {
      if (n.cvars == 1) {
        if (length(grep("$", c.varname, fixed = TRUE)) > 0) {
          c.varname <- substr(c.varname,
                              start = which(unlist(strsplit(c.varname, "")) == "$") + 1,
                              stop = nchar(c.varname))
        }
        c.varnames <- c.varname
      } else {
        c.varnames <- paste("c", 1: n.cvars, sep = "")
      }
    }
  }

  # Sample size
  n <- length(y)

  # Get number of gammas
  n.gammas <- 2 + n.cvars

  # Create vector indicating which observations are pools
  Ig <- ifelse(g > 1, 1, 0)

  # Construct (g, Y, C) matrix
  gyc <- cbind(g, y, c)

  # If no measurement error and xtilde is a list, just use first measurements
  if (errors %in% c("neither", "processing") & class(xtilde) == "list") {
    xtilde <- sapply(xtilde, function(x) x[1])
  }

  # Separate out subjects with replicates
  class.xtilde <- class(xtilde)
  if (class.xtilde == "list") {
    k <- sapply(xtilde, length)
    which.r <- which(k > 1)
    n.r <- length(which.r)
    some.r <- n.r > 0
    if (some.r) {

      # Replicates
      k.r <- k[which.r]
      g.r <- g[which.r]
      Ig.r <- Ig[which.r]
      y.r <- y[which.r]
      gyc.r <- gyc[which.r, , drop = FALSE]
      xtilde.r <- xtilde[which.r]

    }
    n <- n - n.r
    some.s <- n > 0
    if (some.s) {

      # Singles
      g <- g[-which.r]
      Ig <- Ig[-which.r]
      y <- y[-which.r]
      gyc <- gyc[-which.r, , drop = FALSE]
      xtilde <- unlist(xtilde[-which.r])

    }
  } else {
    some.r <- FALSE
    some.s <- TRUE
  }

  # Get indices for parameters being estimated and create labels
  loc.gammas <- 1: n.gammas
  gamma.labels <- paste("gamma", c("0", "y", c.varnames), sep = "_")

  loc.sigsq_1 <- n.gammas + 1
  loc.sigsq_0 <- n.gammas + 2

  theta.labels <- c(gamma.labels, "sigsq_1", "sigsq_0")
  if (errors == "processing") {
    theta.labels <- c(theta.labels, "sigsq_p")
  } else if (errors == "measurement") {
    theta.labels <- c(theta.labels, "sigsq_m")
  } else if (errors == "both") {
    theta.labels <- c(theta.labels, "sigsq_p", "sigsq_m")
  }

  # Log-likelihood function
  llf <- function(f.theta) {

    # Extract parameters
    f.gammas <- matrix(f.theta[loc.gammas], ncol = 1)
    f.sigsq_1 <- f.theta[loc.sigsq_1]
    f.sigsq_0 <- f.theta[loc.sigsq_0]

    if (errors == "neither") {
      f.sigsq_p <- 0
      f.sigsq_m <- 0
    } else if (errors == "measurement") {
      f.sigsq_p <- 0
      f.sigsq_m <- f.theta[loc.sigsq_0 + 1]
    } else if (errors == "processing") {
      f.sigsq_p <- f.theta[loc.sigsq_0 + 1]
      f.sigsq_m <- 0
    } else if (errors == "both") {
      f.sigsq_p <- f.theta[loc.sigsq_0 + 1]
      f.sigsq_m <- f.theta[loc.sigsq_0 + 2]
    }

    # Likelihood:
    # L = f(Xtilde|Y,C)

    if (some.r) {

      # E(Xtilde|Y,C)
      mu_xtilde.yc <- gyc.r %*% f.gammas
      f.sigsq_y <- ifelse(y.r == 1, f.sigsq_1, f.sigsq_0)
      ll.r <- sum(
        mapply(
          FUN = function(g, Ig, k, xtilde, mu_xtilde.yc, f.sigsq_y) {
            dmvnorm(x = xtilde, log = TRUE,
                    mean = rep(mu_xtilde.yc, k),
                    sigma = g * f.sigsq_y +
                      g^2 * f.sigsq_p * Ig + g^2 * diag(f.sigsq_m, k))
          },
          g = g.r,
          Ig = Ig.r,
          k = k.r,
          xtilde = xtilde.r,
          mu_xtilde.yc = mu_xtilde.yc,
          f.sigsq_y = f.sigsq_y
        )
      )

    } else {
      ll.r <- 0
    }

    if (some.s) {

      # E(Xtilde|Y,C) and V(Xtilde|Y,C)
      mu_xtilde.yc <- gyc %*% f.gammas
      sigsq_xtilde.yc <- g * ifelse(y >= 1, f.sigsq_1, f.sigsq_0) +
        g^2 * f.sigsq_p * Ig + g^2 * f.sigsq_m

      # Log-likelihood
      ll.s <- sum(dnorm(x = xtilde, log = TRUE,
                        mean = mu_xtilde.yc,
                        sd = sqrt(sigsq_xtilde.yc)))

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
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.gammas),
                             rep(start_nonvar_var[2], 2))
    } else if (errors %in% c("measurement", "processing")) {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.gammas),
                             rep(start_nonvar_var[2], 3))
    } else if (errors == "both") {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.gammas),
                             rep(start_nonvar_var[2], 4))
    }
  }
  names(nlminb_list$start) <- theta.labels

  # Lower bounds
  if (is.null(nlminb_list$lower)) {
    if (errors == "neither") {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.gammas),
                             rep(lower_nonvar_var[2], 2))
    } else if (errors %in% c("measurement", "processing")) {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.gammas),
                             rep(lower_nonvar_var[2], 3))
    } else if (errors == "both") {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.gammas),
                             rep(lower_nonvar_var[2], 4))
    }
  }

  # Upper bounds
  if (is.null(nlminb_list$upper)) {
    if (errors == "neither") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.gammas),
                             rep(upper_nonvar_var[2], 2))
    } else if (errors %in% c("measurement", "processing")) {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.gammas),
                             rep(upper_nonvar_var[2], 3))
    } else if (errors == "both") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.gammas),
                             rep(upper_nonvar_var[2], 4))
    }
  }

  if (is.null(nlminb_object)) {

    # Obtain ML estimates
    ml.max <- do.call(nlminb, c(list(objective = llf), nlminb_list))

    # If non-convergence, try with jittered starting values if requested
    if (ml.max$convergence == 1) {
      if (! is.null(jitter_start)) {
        message("Trying jittered starting values...")
        nlminb_list$start <- nlminb_list$start +
          rnorm(n = length(nlminb_list$start), sd = jitter_start)
        ml.max2 <- do.call(nlminb, c(list(objective = llf), nlminb_list))
        if (ml.max2$objective < ml.max$objective) ml.max <- ml.max2
      }
      if (ml.max$convergence == 1) {
        message("Object returned by 'nlminb' function indicates non-convergence. You may want to try different starting values.")
      }
    }

  } else {
    ml.max <- nlminb_object
  }
  ml.estimates <- ml.max$par

  # Obtain variance estimates
  hessian.mat <- do.call(numDeriv::hessian,
                         c(list(func = llf, x = ml.estimates),
                           hessian_list))
  theta.variance <- try(solve(hessian.mat), silent = TRUE)
  if (class(theta.variance)[1] == "try-error" | sum(is.na(hessian.mat)) > 0) {

    print(hessian.mat)
    message("The estimated Hessian matrix (printed here) is singular, so variance-covariance matrix could not be obtained. You could try tweaking 'start_nonvar_var' or 'hessian_list' (e.g. increase 'r')")
    theta.variance <- NULL

  } else {

    colnames(theta.variance) <- rownames(theta.variance) <- theta.labels

    if (sum(diag(theta.variance) <= 0) > 0) {
      print(theta.variance)
      message("The estimated variance-covariance matrix (printed here) has some non-positive diagonal elements, so it may not be reliable. You could try tweaking 'start_nonvar_var' or 'hessian_list' (e.g. increase 'r')")
    }

  }

  # Create vector of estimates to return
  estimates <- ml.estimates
  names(estimates) <- theta.labels

  # Create list to return
  ret.list <- list(estimates = estimates,
                   theta.var = theta.variance,
                   nlminb.object = ml.max,
                   aic = 2 * (length(ml.estimates) + ml.max$objective))
  return(ret.list)

}
