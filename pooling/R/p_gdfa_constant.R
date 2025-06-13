#' Gamma Discriminant Function Approach for Estimating Odds Ratio with Exposure
#' Measured in Pools and Potentially Subject to Multiplicative Lognormal Errors
#' (Constant Odds Ratio Version)
#'
#' See \code{\link{p_gdfa}}.
#'
#'
#' @param g Numeric vector with pool sizes, i.e. number of members in each pool.
#' @param y Numeric vector with poolwise Y values, coded 0 if all members are
#' controls and 1 if all members are cases.
#' @param xtilde Numeric vector (or list of numeric vectors, if some pools have
#' replicates) with Xtilde values.
#' @param c List where each element is a numeric matrix containing the
#' \strong{C} values for members of a particular pool (1 row for each member).
#' @param errors Character string specifying the errors that X is subject to.
#' Choices are \code{"neither"}, \code{"processing"} for processing error only,
#' \code{"measurement"} for measurement error only, and \code{"both"}.
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
# data(dat_p_gdfa)
# dat <- dat_p_gdfa$dat
# reps <- dat_p_gdfa$reps
# c.list <- dat_p_gdfa$c.list
#
# g <- dat$g
# y <- dat$y
# xtilde <- reps
# c <- c.list
# errors <- "both"
# estimate_var <- TRUE
p_gdfa_constant <- function(
  g,
  y,
  xtilde,
  c = NULL,
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
  if (! errors %in% c("neither", "processing", "measurement", "both")) {
    stop("The input 'errors' should be set to 'neither', 'processing', 'measurement', or 'both'.")
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
  if (! is.null(jitter_start) & jitter_start <= 0) {
    stop("The input 'jitter_start' should be a non-negative value, if specified.")
  }

  # Get information about covariates C
  if (is.null(c)) {
    c.varnames <- NULL
    n.cvars <- 0
    some.cs <- FALSE
  } else {
    n.cvars <- ncol(c[[1]])
    some.cs <- TRUE
    c.varnames <- colnames(c[[1]])
    if (is.null(c.varnames)) {
      if (n.cvars == 1) {
        c.varnames <- deparse(substitute(c))
      } else {
        c.varnames <- paste("c", 1: n.cvars, sep = "")
      }
    }
  }

  # Sample size
  n <- length(y)

  # Get number of gammas
  n.gammas <- 1 + n.cvars

  # Figure out pool sizes if not specified
  if (is.null(g)) {
    g <- sapply(c, nrow)
  }

  # Create vector indicating which observations are pools
  Ig <- ifelse(g > 1, 1, 0)

  # Construct list of (1, C) matrices
  if (some.cs) {
    onec <- lapply(c, function(x) cbind(1, x))
  } else {
    onec <- NULL
  }

  # If no measurement error and xtilde is a list, just use first measurements.
  # Also, calculate number of replicates per pool.
  class.xtilde <- class(xtilde)
  if (class.xtilde == "list") {
    if (errors %in% c("neither", "processing")) {
      xtilde <- sapply(xtilde, function(x) x[1])
      k <- rep(1, n)
    } else {
      k <- sapply(xtilde, length)
    }
  } else {
    k <- rep(1, n)
  }

  # Separate into subjects with precisely measured X, replicate Xtilde's, and
  # single imprecise Xtilde
  if (errors == "neither") {

    which.p <- 1: n
    which.r <- NULL
    which.i <- NULL

  } else if (errors == "processing") {

    which.p <- which(Ig == 0)
    which.r <- NULL
    which.i <- which(Ig == 1)

  } else if (errors == "measurement") {

    which.p <- NULL
    which.r <- which(k > 1)
    which.i <- which(k == 1)

  } else if (errors == "both") {

    which.p <- NULL
    which.r <- which(k > 1)
    which.i <- which(k == 1)

  }

  n.p <- length(which.p)
  some.p <- n.p > 0
  if (some.p) {
    g.p <- g[which.p]
    Ig.p <- Ig[which.p]
    y.p <- y[which.p]
    onec.p <- onec[which.p]
    x.p <- unlist(xtilde[which.p])
  }

  n.r <- length(which.r)
  some.r <- n.r > 0
  if (some.r) {
    k.r <- k[which.r]
    g.r <- g[which.r]
    Ig.r <- Ig[which.r]
    y.r <- y[which.r]
    onec.r <- onec[which.r]
    xtilde.r <- xtilde[which.r]
  }

  n.i <- length(which.i)
  some.i <- n.i > 0
  if (some.i) {
    g.i <- g[which.i]
    Ig.i <- Ig[which.i]
    y.i <- y[which.i]
    onec.i <- onec[which.i]
    xtilde.i <- unlist(xtilde[which.i])
  }

  # Get indices for parameters being estimated and create labels
  loc.gammas <- 1: n.gammas
  gamma.labels <- paste("gamma", c("0", c.varnames), sep = "_")

  loc.bs <- (n.gammas + 1): (n.gammas + 2)

  theta.labels <- c(gamma.labels, "b1", "b0")
  if (errors == "processing") {
    theta.labels <- c(theta.labels, "sigsq_p")
  } else if (errors == "measurement") {
    theta.labels <- c(theta.labels, "sigsq_m")
  } else if (errors == "both") {
    theta.labels <- c(theta.labels, "sigsq_p", "sigsq_m")
  }

  # Likelihood function for singles and replicates
  if (some.i | some.r) {

    lf <- function(Ig,
                   k,
                   xtilde,
                   x,
                   shape,
                   scale,
                   sigsq_p,
                   sigsq_m) {

      # f(XtildeX|Y,C_1,...,C_g)
      x <- matrix(x, nrow = 1)
      f_xtildex.yc <- apply(x, 2, function(z) {

        # Transformation
        s <- z / (1 - z)

        if (k == 1) {

          # E[log(Xtilde)|X] and V[log(Xtilde|X)]
          mu_logxtilde.x <- log(s) - 1/2 * (sigsq_p * Ig + sigsq_m)
          sigsq_logxtilde.x <- sigsq_p * Ig + sigsq_m

          # Density
          1 / xtilde * dnorm(x = log(xtilde),
                             mean = mu_logxtilde.x,
                             sd = sqrt(sigsq_logxtilde.x)) *
            dgamma(x = s, shape = shape, scale = scale)

        } else {

          # E[log(Xtilde)|X] and V[log(Xtilde|X)]
          Mu_logxtilde.x <- rep(log(s) - 1/2 * (sigsq_p * Ig + sigsq_m), k)
          Sigma_logxtilde.x <- sigsq_p * Ig + diag(sigsq_m, k)

          # Density
          1 / prod(xtilde) * dmvnorm(x = log(xtilde),
                                     mean = Mu_logxtilde.x,
                                     sigma = Sigma_logxtilde.x) *
            dgamma(x = s, shape = shape, scale = scale)

        }

      })

      # Back-transformation
      out <- matrix(f_xtildex.yc / (1 - x)^2, ncol = ncol(x))

    }

  }

  # Log-likelihood function
  llf <- function(f.theta) {

    # Extract parameters
    f.gammas <- matrix(f.theta[loc.gammas], ncol = 1)
    f.b1 <- f.theta[loc.bs[1]]
    f.b0 <- f.theta[loc.bs[2]]

    if (errors == "neither") {
      f.sigsq_p <- 0
      f.sigsq_m <- 0
    } else if (errors == "measurement") {
      f.sigsq_p <- 0
      f.sigsq_m <- f.theta[loc.bs[2] + 1]
    } else if (errors == "processing") {
      f.sigsq_p <- f.theta[loc.bs[2] + 1]
      f.sigsq_m <- 0
    } else if (errors == "both") {
      f.sigsq_p <- f.theta[loc.bs[2] + 1]
      f.sigsq_m <- f.theta[loc.bs[2] + 2]
    }

    if (some.p) {

      # Likelihood for pools with precisely measured X:
      # L = f(X|Y,C_1, ..., C_g)
      if (some.cs) {
        shapes <- sapply(onec.p, function(x) sum(exp(x %*% f.gammas)))
      } else {
        shapes <- g.p * exp(f.gammas[1])
      }
      scales <- ifelse(y.p == 1, f.b1, f.b0)

      ll.p <- sum(dgamma(x = x.p, log = TRUE,
                         shape = shapes,
                         scale = scales))

    } else {
      ll.p <- 0
    }

    # Set skip.rest flag to FALSE
    skip.rest <- FALSE

    if (some.r) {

      # Likelihood for subjects with replicates:
      # L = int_X f(Xtilde|X) f(X|Y,C) dX

      # Shape and scale parameters to feed integral
      if (some.cs) {
        shapes <- sapply(onec.r, function(x) sum(exp(x %*% f.gammas)))
      } else {
        shapes <- g.r * exp(f.gammas[1])
      }
      scales <- ifelse(y.r == 1, f.b1, f.b0)

      int.vals <- c()
      for (ii in 1: length(xtilde.r)) {

        int.ii <- do.call(hcubature,
                          c(list(f = lf,
                                 vectorInterface = TRUE,
                                 lowerLimit = 0,
                                 upperLimit = 1,
                                 Ig = Ig.r[ii],
                                 k = k.r[ii],
                                 xtilde = xtilde.r[[ii]],
                                 shape = shapes[ii],
                                 scale = scales[ii],
                                 sigsq_p = f.sigsq_p,
                                 sigsq_m = f.sigsq_m),
                            hcubature_list))

        # If integral 0, find region with density
        if (is.na(int.ii$integral) | int.ii$integral == 0) {

          limits <- seq(1e-5, 1 - 1e-5, 1e-5)
          fs <- lf(x = limits,
                   Ig = Ig.r[ii],
                   k = k.r[ii],
                   xtilde = xtilde.r[[ii]],
                   shape = shapes[ii],
                   scale = scales[ii],
                   sigsq_p = f.sigsq_p,
                   sigsq_m = f.sigsq_m)
          limits <- limits[fs > 0]
          if (length(limits) > 0) {
            limits <- c(max(0, min(limits) - 1e-5), min(1, max(limits) + 1e-5))
            int.ii <- do.call(hcubature,
                              c(list(f = lf,
                                     vectorInterface = TRUE,
                                     lowerLimit = limits[1],
                                     upperLimit = limits[2],
                                     Ig = Ig.r[ii],
                                     k = k.r[ii],
                                     xtilde = xtilde.r[[ii]],
                                     shape = shapes[ii],
                                     scale = scales[ii],
                                     sigsq_p = f.sigsq_p,
                                     sigsq_m = f.sigsq_m),
                                hcubature_list))
          }

        }

        int.vals[ii] <- int.ii$integral

        # If integral 0, set skip.rest to TRUE to skip further LL calculations
        if (is.na(int.ii$integral) | int.ii$integral == 0) {
          print(paste("Integral is ", int.ii$integral, " for ii = ", ii, sep = ""))
          print(f.theta)
          skip.rest <- TRUE
          break
        }

      }
      ll.r <- sum(log(int.vals))

    } else {
      ll.r <- 0
    }

    if (some.i & ! skip.rest) {

      # Likelihood for pools with single Xtilde:
      # L = int_X f(Xtilde|X) f(X|Y,C_1,...,C_g) dX

      # Shape and scale parameters to feed to integral
      if (some.cs) {
        shapes <- sapply(onec.i, function(x) sum(exp(x %*% f.gammas)))
      } else {
        shapes <- g.i * exp(f.gammas[1])
      }
      scales <- ifelse(y.i == 1, f.b1, f.b0)

      int.vals <- c()
      for (ii in 1: length(xtilde.i)) {

        int.ii <- do.call(hcubature,
                          c(list(f = lf,
                                 vectorInterface = TRUE,
                                 lowerLimit = 0,
                                 upperLimit = 1,
                                 Ig = Ig.i[ii],
                                 k = 1,
                                 xtilde = xtilde.i[ii],
                                 shape = shapes[ii],
                                 scale = scales[ii],
                                 sigsq_p = f.sigsq_p,
                                 sigsq_m = f.sigsq_m),
                          hcubature_list))

        # If integral 0, find region with density
        if (is.na(int.ii$integral) | int.ii$integral == 0) {

          limits <- seq(1e-5, 1 - 1e-5, 1e-5)
          fs <- lf(x = limits,
                   Ig = Ig.i[ii],
                   k = 1,
                   xtilde = xtilde.i[ii],
                   shape = shapes[ii],
                   scale = scales[ii],
                   sigsq_p = f.sigsq_p,
                   sigsq_m = f.sigsq_m)
          limits <- limits[fs > 0]
          if (length(limits) > 0) {
            limits <- c(max(0, min(limits) - 1e-5), min(1, max(limits) + 1e-5))
            int.ii <- do.call(hcubature,
                              c(list(f = lf,
                                     vectorInterface = TRUE,
                                     lowerLimit = limits[1],
                                     upperLimit = limits[2],
                                     Ig = Ig.i[ii],
                                     k = 1,
                                     xtilde = xtilde.i[ii],
                                     shape = shapes[ii],
                                     scale = scales[ii],
                                     sigsq_p = f.sigsq_p,
                                     sigsq_m = f.sigsq_m),
                                hcubature_list))
          }

        }

        int.vals[ii] <- int.ii$integral

        # If integral 0, set skip.rest to TRUE to skip further LL calculations
        if (is.na(int.ii$integral) | int.ii$integral == 0) {
          print(paste("Integral is ", int.ii$integral, " for ii = ", ii, sep = ""))
          print(f.theta)
          skip.rest <- TRUE
          break
        }

      }
      ll.i <- sum(log(int.vals))

    } else {
      ll.i <- 0
    }

    # Return negative log-likelihood
    ll <- ll.p + ll.r + ll.i
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

  # Obtain point estimate for log-odds ratio
  b1.hat <- ml.estimates[loc.bs[1]]
  b0.hat <- ml.estimates[loc.bs[2]]
  logOR.hat <- 1 / b0.hat - 1 / b1.hat

  # Obtain variance estimates
  if (estimate_var) {

    # Estimate Hessian
    hessian.mat <- do.call(numDeriv::hessian,
                           c(list(func = llf, x = ml.estimates),
                             hessian_list))

    # Estimate variance-covariance matrix
    theta.variance <- try(solve(hessian.mat), silent = TRUE)
    if (class(theta.variance)[1] == "try-error" | sum(is.na(hessian.mat)) > 0) {

      print(hessian.mat)
      message("The estimated Hessian matrix (printed here) is singular, so variance-covariance matrix could not be obtained. You could try tweaking 'start_nonvar_var' or 'hessian_list' (e.g. increase 'r')")
      theta.variance <- NULL
      logOR.var <- NA

    } else {

      fprime <- matrix(c(1 / b1.hat^2, -1 / b0.hat^2), nrow = 1)
      colnames(theta.variance) <- rownames(theta.variance) <- theta.labels
      logOR.var <- fprime %*% theta.variance[loc.bs, loc.bs] %*% t(fprime)

      if (sum(diag(theta.variance) <= 0) > 0) {
        print(theta.variance)
        message("The estimated variance-covariance matrix (printed here) has some non-positive diagonal elements, so it may not be reliable. You could try tweaking 'start_nonvar_var' or 'hessian_list' (e.g. increase 'r')")
      }

    }

  } else {

    theta.variance <- NULL
    logOR.var <- NA

  }

  # Create vector of estimates to return
  estimates <- c(ml.estimates, logOR.hat, logOR.var)
  names(estimates) <- c(theta.labels, "logOR.hat", "logOR.var")

  # Create list to return
  ret.list <- list(estimates = estimates,
                   theta.var = theta.variance,
                   nlminb.object = ml.max,
                   aic = 2 * (length(ml.estimates) + ml.max$objective))
  return(ret.list)

}
