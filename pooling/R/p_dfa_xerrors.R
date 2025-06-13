#' Discriminant Function Approach for Estimating Odds Ratio with Normal Exposure
#' Measured in Pools and Potentially Subject to Errors
#'
#' Archived on 7/23/18. Please use \code{\link{p_ndfa}} instead.
#'
#'
#' @param g Numeric vector of pool sizes, i.e. number of members in each pool.
#' @param y Numeric vector of poolwise \code{Y} values (number of cases in each
#' pool).
#' @param xtilde Numeric vector (or list of numeric vectors, if some pools have
#' replicates) with \code{Xtilde} values.
#' @param c Numeric matrix with poolwise \strong{\code{C}} values (if any), with
#' one row for each pool. Can be a vector if there is only 1 covariate.
#' @param constant_or Logical value for whether to assume a constant OR for
#' \code{X}, which means that \code{sigsq_1 = sigsq_0}. If \code{NULL}, model is
#' fit with and without this assumption, and likelihood ratio test is performed
#' to test it.
#' @param errors Character string specifying the errors that \code{X} is subject
#' to. Choices are \code{"neither"}, \code{"processing"} for processing error
#' only, \code{"measurement"} for measurement error only, and \code{"both"}.
#' @param ... Additional arguments to pass to \code{\link[stats]{nlminb}}.
#'
#'
#' @return
#' List of point estimates, variance-covariance matrix, object returned by
#' \code{\link[stats]{nlminb}}, and AIC, for one or two models depending on
#' \code{constant_or}. If \code{constant_or = NULL}, also returns result of a
#' likelihood ratio test for \code{H0: sigsq_1 = sigsq_0}, which is equivalent
#' to \code{H0: log-OR is constant}. If \code{constant_or = NULL}, returned
#' objects with names ending in 1 are for model that does not assume constant
#' log-OR, and those ending in 2 are for model that assumes constant log-OR.
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
#' @examples
#' # Load dataset containing poolwise (Y, Xtilde, C) values for pools of size
#' # 1, 2, and 3. Xtilde values are affected by processing error.
#' data(pdat1)
#'
#' # Estimate log-OR for X and Y adjusted for C, ignoring processing error
#' fit1 <- p_dfa_xerrors(g = pdat1$g, y = pdat1$numcases, xtilde = pdat1$xtilde,
#'                       c = pdat1$c, errors = "neither")
#' fit1$estimates
#'
#' # Repeat, but accounting for processing error. Closer to true log-OR of 0.5.
#' fit2 <- p_dfa_xerrors(g = pdat1$g, y = pdat1$numcases, xtilde = pdat1$xtilde,
#'                       c = pdat1$c, errors = "processing")
#' fit2$estimates
#'
#'
#' @export
p_dfa_xerrors <- function(g, y, xtilde, c = NULL,
                          constant_or = TRUE,
                          errors = "both", ...) {

  # Check that inputs are valid
  if (! is.null(constant_or) && ! is.logical(constant_or)) {
    stop("The input 'contant_or' should be set to TRUE, FALSE, or NULL.")
  }
  if (! errors %in% c("neither", "processing", "measurement", "both")) {
    stop("The input 'errors' should be set to 'neither', 'processing',
         'measurement', or 'both'.")
  }

  # Get name of y input
  y.varname <- deparse(substitute(y))
  if (length(grep("$", y.varname, fixed = TRUE)) > 0) {
    y.varname <- substr(y.varname,
                        start = which(unlist(strsplit(y.varname, "")) == "$") + 1,
                        stop = nchar(y.varname))
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
  gamma.labels <- paste("gamma", c("0", y.varname, c.varnames), sep = "_")

  loc.sigsq <- n.gammas + 1
  loc.sigsq_1 <- n.gammas + 1
  loc.sigsq_0 <- n.gammas + 2

  if (errors == "neither") {
    theta.labels1 <- c(gamma.labels, "sigsq_1", "sigsq_0")
    theta.labels2 <- c(gamma.labels, "sigsq")
  } else if (errors == "processing") {
    theta.labels1 <- c(gamma.labels, "sigsq_1", "sigsq_0", "sigsq_p")
    theta.labels2 <- c(gamma.labels, "sigsq", "sigsq_p")
  } else if (errors == "measurement") {
    theta.labels1 <- c(gamma.labels, "sigsq_1", "sigsq_0", "sigsq_m")
    theta.labels2 <- c(gamma.labels, "sigsq", "sigsq_m")
  } else if (errors == "both") {
    theta.labels1 <- c(gamma.labels, "sigsq_1", "sigsq_0", "sigsq_p", "sigsq_m")
    theta.labels2 <- c(gamma.labels, "sigsq", "sigsq_p", "sigsq_m")
  }

  # Fit model with different residual error variances
  if (is.null(constant_or) || ! constant_or) {

    # Log-likelihood function
    ll.f1 <- function(f.theta) {

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

        ll.vals <- c()
        for (ii in 1: length(xtilde.r)) {

          # Values for ith subject
          g_i <- g.r[ii]
          Ig_i <- Ig.r[ii]
          k_i <- k.r[ii]
          y_i <- y.r[ii]
          gyc_i <- gyc.r[ii, ]
          xtilde_i <- xtilde.r[[ii]]

          # E(Xtilde|Y,C) and V(Xtilde|Y,C)
          Mu_xtilde.yc <- matrix(gyc_i %*% f.gammas, ncol = k_i)
          Sigma_xtilde.yc <-
            matrix(g_i * ifelse(y_i, f.sigsq_1, f.sigsq_0) +
                     g_i^2 * f.sigsq_p * Ig_i, ncol = k_i, nrow = k_i) +
            diag(x = g_i^2 * f.sigsq_m, ncol = k_i, nrow = k_i)

          # Log-likelihood
          ll.vals[ii] <- dmvnorm(x = xtilde_i, log = TRUE,
                                 mean = Mu_xtilde.yc,
                                 sigma = Sigma_xtilde.yc)

        }
        ll.r <- sum(ll.vals)

      } else {
        ll.r <- 0
      }

      if (some.s) {

        # E(Xtilde|Y,C) and V(Xtilde|Y,C)
        mu_xtilde.yc <- gyc %*% f.gammas
        sigsq_xtilde.yc <- g * ifelse(y, f.sigsq_1, f.sigsq_0) +
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

    # Create list of extra arguments, and assign default starting values and
    # lower values if not specified by user
    extra.args <- list(...)
    if (is.null(extra.args$start)) {
      if (errors == "neither") {
        extra.args$start <- c(rep(0.01, n.gammas), rep(1, 2))
      } else if (errors %in% c("measurement", "processing")) {
        extra.args$start <- c(rep(0.01, n.gammas), rep(1, 3))
      } else if (errors == "both") {
        extra.args$start <- c(rep(0.01, n.gammas), rep(1, 4))
      }
    }
    if (is.null(extra.args$lower)) {
      if (errors == "neither") {
        extra.args$lower <- c(rep(-Inf, n.gammas), rep(1e-3, 2))
      } else if (errors %in% c("measurement", "processing")) {
        extra.args$lower <- c(rep(-Inf, n.gammas), rep(1e-3, 3))
      } else if (errors == "both") {
        extra.args$lower <- c(rep(-Inf, n.gammas), rep(1e-3, 4))
      }
    }
    if (is.null(extra.args$control$rel.tol)) {
      extra.args$control$rel.tol <- 1e-6
    }
    if (is.null(extra.args$control$eval.max)) {
      extra.args$control$eval.max <- 1000
    }
    if (is.null(extra.args$control$iter.max)) {
      extra.args$control$iter.max <- 750
    }

    # Obtain ML estimates
    ml.max1 <- do.call(nlminb, c(list(objective = ll.f1), extra.args))
    ml.estimates <- ml.max1$par

    # Variance estimates
    hessian.mat <- numDeriv::hessian(func = ll.f1, x = ml.estimates)
    theta.variance <- try(solve(hessian.mat), silent = TRUE)
    if (class(theta.variance)[1] == "try-error") {
      message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
      theta.variance <- NULL
    } else {
      colnames(theta.variance) <- rownames(theta.variance) <- theta.labels1
    }

    # Create vector of estimates and calculate AIC
    estimates1 <- ml.estimates
    names(estimates1) <- theta.labels1
    theta.var1 <- theta.variance
    aic1 <- 2 * (length(estimates1) + ml.max1$objective)

  }

  # Fit model with same residual error variances
  if (is.null(constant_or) || constant_or) {

    # Log-likelihood function
    ll.f2 <- function(f.theta) {

      # Extract parameters
      f.gammas <- matrix(f.theta[loc.gammas], ncol = 1)
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
      # L = f(Xtilde|Y,C)

      if (some.r) {

        ll.vals <- c()
        for (ii in 1: length(xtilde.r)) {

          # Values for ith subject
          g_i <- g.r[ii]
          Ig_i <- Ig.r[ii]
          k_i <- k.r[ii]
          y_i <- y.r[ii]
          gyc_i <- gyc.r[ii, ]
          xtilde_i <- xtilde.r[[ii]]

          # E(Xtilde|Y,C) and V(Xtilde|Y,C)
          Mu_xtilde.yc <- matrix(gyc_i %*% f.gammas, ncol = k_i)
          Sigma_xtilde.yc <-
            matrix(g_i * f.sigsq +
                     g_i^2 * f.sigsq_p * Ig_i, ncol = k_i, nrow = k_i) +
            diag(x = g_i^2 * f.sigsq_m, ncol = k_i, nrow = k_i)

          # Log-likelihood
          ll.vals[ii] <- dmvnorm(x = xtilde_i, log = TRUE,
                                 mean = Mu_xtilde.yc,
                                 sigma = Sigma_xtilde.yc)

        }
        ll.r <- sum(ll.vals)

      } else {
        ll.r <- 0
      }

      if (some.s) {

        # E(Xtilde|Y,C) and V(Xtilde|Y,C)
        mu_xtilde.yc <- gyc %*% f.gammas
        sigsq_xtilde.yc <- g * f.sigsq +
          g^2 * f.sigsq_p * Ig +
          g^2 * f.sigsq_m

        # Log-likelihood
        ll.s <- sum(dnorm(x = xtilde, log = TRUE,
                          mean = mu_xtilde.yc, sd = sqrt(sigsq_xtilde.yc)))

      } else {
        ll.s <- 0
      }

      # Return negative log-likelihood
      ll <- ll.r + ll.s
      return(-ll)

    }

    # Create list of extra arguments, and assign default starting values and
    # lower values if not specified by user
    extra.args <- list(...)
    if (is.null(extra.args$start)) {
      if (errors == "neither") {
        extra.args$start <- c(rep(0.01, n.gammas), 1)
      } else if (errors %in% c("measurement", "processing")) {
        extra.args$start <- c(rep(0.01, n.gammas), rep(1, 2))
      } else if (errors == "both") {
        extra.args$start <- c(rep(0.01, n.gammas), rep(1, 3))
      }
    }
    if (is.null(extra.args$lower)) {
      if (errors == "neither") {
        extra.args$lower <- c(rep(-Inf, n.gammas), 1e-3)
      } else if (errors %in% c("measurement", "processing")) {
        extra.args$lower <- c(rep(-Inf, n.gammas), rep(1e-3, 2))
      } else if (errors == "both") {
        extra.args$lower <- c(rep(-Inf, n.gammas), rep(1e-3, 3))
      }
    }
    if (is.null(extra.args$control$rel.tol)) {
      extra.args$control$rel.tol <- 1e-6
    }
    if (is.null(extra.args$control$eval.max)) {
      extra.args$control$eval.max <- 1000
    }
    if (is.null(extra.args$control$iter.max)) {
      extra.args$control$iter.max <- 750
    }

    # Obtain ML estimates
    ml.max2 <- do.call(nlminb, c(list(objective = ll.f2), extra.args))
    ml.estimates <- ml.max2$par

    # Obtain point estimate for log-odds ratio
    gamma_y.hat <- ml.estimates[2]
    sigsq.hat <- ml.estimates[loc.sigsq]
    logOR.hat <- gamma_y.hat / sigsq.hat

    # Estimate variance of logOR.hat and perform bias adjustment
    hessian.mat <- numDeriv::hessian(func = ll.f2, x = ml.estimates)
    theta.variance <- try(solve(hessian.mat), silent = TRUE)
    if (class(theta.variance)[1] == "try-error" | sum(is.na(hessian.mat)) > 0) {
      message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained and bias adjustment cannot be applied.")
      theta.variance <- NULL
      logOR.var <- logOR.var <- logOR_adj.hat <- logOR_adj.var <- NA
    } else {
      fprime <- matrix(c(1 / sigsq.hat, -gamma_y.hat / sigsq.hat^2), nrow = 1)
      colnames(theta.variance) <- rownames(theta.variance) <- theta.labels2
      logOR.var <- fprime %*%
        theta.variance[c(2, loc.sigsq), c(2, loc.sigsq)] %*% t(fprime)
      sigsq.var <- theta.variance[loc.sigsq, loc.sigsq]
      logOR_adj.hat <- logOR.hat - gamma_y.hat * sigsq.var / sigsq.hat^3
      logOR_adj.var <- logOR.var * (logOR_adj.hat / logOR.hat)^2
      if (sign(logOR.hat) != sign(logOR_adj.hat)) {
        message("Bias adjustment flipped the sign of the log-OR estimate, so you may want to use the non-bias adjusted version.")
      }
    }

    # Create vector of estimates and calculate AIC
    estimates2 <- c(ml.estimates, logOR.hat, logOR.var, logOR_adj.hat, logOR_adj.var)
    names(estimates2) <- c(theta.labels2, "logOR.hat", "logOR.var", "logOR_adj.hat", "logOR_adj.var")
    theta.var2 <- theta.variance
    aic2 <- 2 * (length(ml.estimates) + ml.max2$objective)

  }

  # Return objects
  if (is.null(constant_or)) {

    # Likelihood ratio test
    d <- 2 * (-ml.max1$objective + ml.max2$objective)
    p <- pchisq(q = d, df = 1, lower.tail = FALSE)
    if (p < 0.05) {
      message <- "H0: Constant log-OR rejected at alpha = 0.05. Recommend using estimates1, theta.var1, etc."
    } else {
      message <- "H0: Constant log-OR not rejected at alpha = 0.05. Recommend using estimates2, theta.var2, etc."
    }
    lrt <- list(d = d, p = p, message = message)

    return(list(lrt = lrt, estimates1 = estimates1,
                estimates2 = estimates2,
                theta.var1 = theta.var1,
                theta.var2 = theta.var2,
                nlminb.object1 = ml.max1,
                nlminb.object2 = ml.max2,
                aic1 = aic1,
                aic2 = aic2))

  } else if (constant_or) {

    return(list(estimates = estimates2,
                theta.var = theta.var2,
                nlminb.object = ml.max2,
                aic = aic2))

  } else {

    return(list(estimates = estimates1,
                theta.var = theta.var1,
                nlminb.object = ml.max1,
                aic = aic1))
  }

}
