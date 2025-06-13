#' Discriminant Function Approach for Estimating Odds Ratio with Gamma Exposure
#' Measured in Pools and Potentially Subject to Errors
#'
#' Archived on 7/23/18. Please use \code{\link{p_gdfa}} instead.
#'
#'
#' @param g Numeric vector with pool sizes, i.e. number of members in each pool.
#' @param y Numeric vector with poolwise \code{Y} values, coded 0 if all members
#' are controls and 1 if all members are cases.
#' @param xtilde Numeric vector (or list of numeric vectors, if some pools have
#' replicates) with \code{Xtilde} values.
#' @param c List where each element is a numeric matrix containing the
#' \strong{\code{C}} values for members of a particular pool (1 row for each
#' member).
#' @param constant_or Logical value for whether to assume a constant OR for
#' \code{X}, which means that \code{gamma_y = 0}. If \code{NULL}, model is
#' fit with and without this assumption, and likelihood ratio test is performed
#' to test it.
#' @param errors Character string specifying the errors that \code{X} is subject
#' to. Choices are \code{"neither"}, \code{"processing"} for processing error
#' only, \code{"measurement"} for measurement error only, and \code{"both"}.
#' @param integrate_tol Numeric value specifying the \code{tol} input to
#' \code{\link{hcubature}}.
#' @param integrate_tol_hessian Same as \code{integrate_tol}, but for use when
#' estimating the Hessian matrix only. Sometimes more precise integration
#' (i.e. smaller tolerance) helps prevent cases where the inverse Hessian is not
#' positive definite.
#' @param estimate_var Logical value for whether to return variance-covariance
#' matrix for parameter estimates.
#' @param fix_posdef Logical value for whether to repeatedly reduce
#' \code{integrate_tol_hessian} by factor of 5 and re-estimate Hessian to try
#' to avoid non-positive definite variance-covariance matrix.
#' @param ... Additional arguments to pass to \code{\link[stats]{nlminb}}.
#'
#'
#' @return
#' List of point estimates, variance-covariance matrix, objects returned by
#' \code{\link[stats]{nlminb}}, and AICs, for one or two models depending on
#' \code{constant_or}. If \code{constant_or = NULL}, also returns result of a
#' likelihood ratio test for \code{H0: gamma_y = 0}, which is equivalent to
#' \code{H0: log-OR is constant}. If \code{constant_or = NULL}, returned objects
#' with names ending in 1 are for model that does not assume constant log-OR,
#' and those ending in 2 are for model that assumes constant log-OR.
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
#' @examples
#' # Load dataset with (g, Y, Xtilde, C) values for 248 pools and list of C
#' # values for members of each pool. Xtilde values are affected by processing
#' # error.
#' data(pdat2)
#' dat <- pdat2$dat
#' c.list <- pdat2$c.list
#'
#' # Estimate log-OR for X and Y adjusted for C, ignoring processing error
#' fit1 <- p_dfa_xerrors2(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = dat$xtilde,
#'   c = c.list,
#'   errors = "neither"
#' )
#' fit1$estimates
#'
#' # Repeat, but accounting for processing error.
#' \dontrun{
#' fit2 <- p_dfa_xerrors2(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = dat$xtilde,
#'   c = c.list,
#'   errors = "processing",
#'   control = list(trace = 1)
#' )
#' fit2$estimates
#' }
#'
#' @export
p_dfa_xerrors2 <- function(g, y, xtilde, c = NULL,
                           constant_or = TRUE,
                           errors = "both",
                           integrate_tol = 1e-8,
                           integrate_tol_hessian = integrate_tol,
                           estimate_var = TRUE,
                           fix_posdef = FALSE,
                           ...) {

  # Check that inputs are valid
  if (! is.null(constant_or) && ! is.logical(constant_or)) {
    stop("The input 'contant_or' should be set to TRUE, FALSE, or NULL.")
  }
  if (! errors %in% c("neither", "processing", "measurement", "both")) {
    stop("The input 'errors' should be set to 'neither', 'processing',
         'measurement', or 'both'.")
  }
  if (! (is.numeric(integrate_tol) & inside(integrate_tol, c(1e-32, Inf)))) {
    stop("The input 'integrate_tol' must be a numeric value greater than 1e-32.")
  }
  if (! (is.numeric(integrate_tol_hessian) & inside(integrate_tol_hessian, c(1e-32, Inf)))) {
    stop("The input 'integrate_tol_hessian' must be a numeric value greater than 1e-32.")
  }
  if (! is.logical(estimate_var)) {
    stop("The input 'estimate_var' should be TRUE or FALSE.")
  }

  # Sample size
  n <- length(y)

  # Get name of y input
  y.varname <- deparse(substitute(y))
  if (length(grep("$", y.varname, fixed = TRUE)) > 0) {
    y.varname <- substr(y.varname,
                        start = which(unlist(strsplit(y.varname, "")) == "$") + 1,
                        stop = nchar(y.varname))
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

  # Get number of gammas
  n.gammas1 <- 2 + n.cvars
  n.gammas2 <- 1 + n.cvars

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

  # If no measurement error and xtilde is a list, just use first measurements
  if (errors %in% c("neither", "processing") & class(xtilde) == "list") {
    xtilde <- sapply(xtilde, function(x) x[1])
  }

  # Separate out pools with precisely measured X
  if (errors == "neither") {
    which.p <- 1: n
  } else if (errors == "processing") {
    which.p <- which(Ig == 0)
  } else {
    which.p <- NULL
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

  # Separate out pools with replicates
  class.xtilde <- class(xtilde)
  if (class.xtilde == "list") {
    k <- sapply(xtilde, length)
    which.r <- which(k > 1)
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
  } else {
    k <- rep(1, n)
    some.r <- FALSE
  }

  # Separate out pools with single Xtilde
  if (errors == "neither") {
    which.i <- NULL
  } else if (errors == "processing") {
    which.i <- which(Ig == 1 & k == 1)
  } else if (errors %in% c("measurement", "both")) {
    which.i <- which(k == 1)
  }
  n.i <- length(which.i)
  some.i <- n.i > 0
  if (some.i) {
    g.i <- g[which.i]
    Ig.i <- Ig[which.i]
    y.i <- y[which.i]
    onec.i <- onec[which.i]
    xtilde.i <- xtilde[which.i]
  }

  # Get indices for parameters being estimated and create labels
  loc.gammas1 <- 1: n.gammas1
  loc.gammas2 <- 1: n.gammas2
  gamma.labels1 <- paste("gamma", c("0", y.varname, c.varnames), sep = "_")
  gamma.labels2 <- paste("gamma", c("0", c.varnames), sep = "_")

  loc.bs1 <- (n.gammas1 + 1): (n.gammas1 + 2)
  loc.bs2 <- (n.gammas2 + 1): (n.gammas2 + 2)

  if (errors == "neither") {
    theta.labels1 <- c(gamma.labels1, "b1", "b0")
    theta.labels2 <- c(gamma.labels2, "b1", "b0")
  } else if (errors == "processing") {
    theta.labels1 <- c(gamma.labels1, "b1", "b0", "sigsq_p")
    theta.labels2 <- c(gamma.labels2, "b1", "b0", "sigsq_p")
  } else if (errors == "measurement") {
    theta.labels1 <- c(gamma.labels1, "b1", "b0", "sigsq_m")
    theta.labels2 <- c(gamma.labels2, "b1", "b0", "sigsq_m")
  } else if (errors == "both") {
    theta.labels1 <- c(gamma.labels1, "b1", "b0", "sigsq_p", "sigsq_m")
    theta.labels2 <- c(gamma.labels2, "b1", "b0", "sigsq_p", "sigsq_m")
  }

  # Fit model with gamma_y
  if (is.null(constant_or) || ! constant_or) {

    # Log-likelihood function
    ll.f1 <- function(f.theta, estimating.hessian = FALSE) {

      # Extract parameters
      f.gammas <- matrix(f.theta[loc.gammas1], ncol = 1)
      f.b1 <- f.theta[loc.bs1[1]]
      f.b0 <- f.theta[loc.bs1[2]]

      if (errors == "neither") {
        f.sigsq_p <- 0
        f.sigsq_m <- 0
      } else if (errors == "measurement") {
        f.sigsq_p <- 0
        f.sigsq_m <- f.theta[loc.bs1[2] + 1]
      } else if (errors == "processing") {
        f.sigsq_p <- f.theta[loc.bs1[2] + 1]
        f.sigsq_m <- 0
      } else if (errors == "both") {
        f.sigsq_p <- f.theta[loc.bs1[2] + 1]
        f.sigsq_m <- f.theta[loc.bs1[2] + 2]
      }

      if (some.p) {

        # Likelihood for pools with precisely measured X:
        # L = f(X|Y,C_1,...,C_g)

        # a_i's in X|Y,C_1, ..., C_g ~ Gamma(a_i, b)
        if (some.cs) {
          alphas <- sapply(onec.p, function(x)
            sum(exp(x %*% f.gammas[-2, , drop = FALSE]))) *
            exp(f.gammas[2] * y.p)
        } else {
          alphas <- g.p * exp(f.gammas[1] + f.gammas[2] * y.p)
        }

        # Log-likelihood
        ll.p <- sum(dgamma(x = x.p,
                           shape = alphas,
                           scale = ifelse(y.p == 1, f.b1, f.b0), log = TRUE))

      } else {
        ll.p <- 0
      }

      # Set skip.rest flag to FALSE
      skip.rest <- FALSE

      if (some.r) {

        # Likelihood for pools with replicates
        # L = \int_X f(Xtilde|X) f(X|Y,C_1,...,C_g) dX

        # a_i's to feed to integral
        if (some.cs) {
          alphas <- sapply(onec.r, function(x)
            sum(exp(x %*% f.gammas[-2, , drop = FALSE]))) *
            exp(f.gammas[2] * y.r)
        } else {
          alphas <- g.r * exp(f.gammas[1] + f.gammas[2] * y.r)
        }

        # Function for integrating out X's
        int.f_i1a <- function(k_i, g_i, Ig_i, y_i, x_i, onec_i, xtilde_i, a_i) {

          # f(XtildeX|Y,C_1,...,C_g)
          x_i <- matrix(x_i, nrow = 1)
          f_xtildex.yc <- apply(x_i, 2, function(z) {

            # Transformation
            s_i <- z / (1 - z)

            # E[log(Xtilde)|X] and V[log(Xtilde|X)]
            Mu_logxtilde.x <-
              matrix(log(s_i) - 1/2 * (f.sigsq_p * Ig_i + f.sigsq_m), nrow = k_i)
            Sigma_logxtilde.x <- matrix(f.sigsq_p * Ig_i, ncol = k_i, nrow = k_i) +
              diag(rep(f.sigsq_m, k_i))

            # Density
            1 / prod(xtilde_i) * dmvnorm(x = log(xtilde_i),
                                         mean = Mu_logxtilde.x,
                                         sigma = Sigma_logxtilde.x) *
              dgamma(x = s_i, shape = a_i, scale = ifelse(y_i, f.b1, f.b0))

          })

          # Back-transformation
          out <- matrix(f_xtildex.yc / (1 - x_i)^2, ncol = ncol(x_i))

        }

        # Get integration tolerance
        if (estimating.hessian) {
          int_tol <- integrate_tol_hessian
        } else {
          int_tol <- integrate_tol
        }

        int.vals <- c()
        for (ii in 1: length(xtilde.r)) {

          # Values for ith subject
          g_i <- g.r[ii]
          Ig_i <- Ig.r[ii]
          k_i <- k.r[ii]
          y_i <- y.r[ii]
          onec_i <- onec.r[[ii]]
          xtilde_i <- xtilde.r[[ii]]
          a_i <- alphas[ii]

          int.ii <-
            hcubature(f = int.f_i1a, tol = int_tol,
                      lowerLimit = 0, upperLimit = 1,
                      vectorInterface = TRUE,
                      g_i = g_i, Ig_i = Ig_i, k_i = k_i, y_i = y_i,
                      onec_i = onec_i, xtilde_i = xtilde_i, a_i = a_i)
          int.vals[ii] <- int.ii$integral

          # If integral 0, set skip.rest to TRUE to skip further LL calculations
          if (int.ii$integral == 0) {
            print(paste("Integral is 0 for ii = ", ii, sep = ""))
            print(f.theta)
            print(int.ii)
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
        # L = \int_X f(Xtilde|X) f(X|Y,C_1,...,C_g)Y|X,C) dX

        # a_i's to feed to integral
        if (some.cs) {
          alphas <- sapply(onec.i, function(x)
            sum(exp(x %*% f.gammas[-2, , drop = FALSE]))) *
            exp(f.gammas[2] * y.i)
        } else {
          alphas <- g.i * exp(f.gammas[1] + f.gammas[2] * y.i)
        }

        # Function for integrating out X's
        int.f_i2a <- function(g_i, Ig_i, y_i, x_i, onec_i, xtilde_i, a_i) {

          # f(XtildeX|Y,C_1,...,C_g)
          x_i <- matrix(x_i, nrow = 1)
          f_xtildex.yc <- apply(x_i, 2, function(z) {

            # Transformation
            s_i <- z / (1 - z)

            # E[log(Xtilde)|X] and V[log(Xtilde|X)]
            mu_logxtilde.x <- log(s_i) - 1/2 * (f.sigsq_p * Ig_i + f.sigsq_m)
            sigsq_logxtilde.x <- f.sigsq_p * Ig_i + f.sigsq_m

            # Density
            1 / xtilde_i * dnorm(x = log(xtilde_i),
                                 mean = mu_logxtilde.x,
                                 sd = sqrt(sigsq_logxtilde.x)) *
              dgamma(x = s_i, shape = a_i, scale = ifelse(y_i, f.b1, f.b0))

          })

          # Back-transformation
          out <- matrix(f_xtildex.yc / (1 - x_i)^2, ncol = ncol(x_i))

        }

        # Get integration tolerance
        if (estimating.hessian) {
          int_tol <- integrate_tol_hessian
        } else {
          int_tol <- integrate_tol
        }

        int.vals <- c()
        for (ii in 1: length(xtilde.i)) {

          # Values for ith subject
          g_i <- g.i[ii]
          Ig_i <- Ig.i[ii]
          y_i <- y.i[ii]
          onec_i <- onec.i[[ii]]
          xtilde_i <- xtilde.i[[ii]]
          a_i <- alphas[ii]

          int.ii <-
            adaptIntegrate(f = int.f_i2a, tol = int_tol,
                           lowerLimit = 0, upperLimit = 1,
                           vectorInterface = TRUE,
                           g_i = g_i, Ig_i = Ig_i, y_i = y_i, onec_i = onec_i,
                           xtilde_i = xtilde_i, a_i = a_i)
          int.vals[ii] <- int.ii$integral

          # If integral 0, set skip.rest to TRUE to skip further LL calculations
          if (int.ii$integral == 0) {
            print(paste("Integral is 0 for ii = ", ii, sep = ""))
            print(f.theta)
            print(int.ii)
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

    # Create list of extra arguments, and assign default starting values and lower
    # values if not specified by user
    extra.args <- list(...)
    if (is.null(extra.args$start)) {
      if (errors == "neither") {
        extra.args$start <- c(rep(0.01, n.gammas1), rep(1, 2))
      } else if (errors %in% c("processing", "measurement")) {
        extra.args$start <- c(rep(0.01, n.gammas1), rep(1, 3))
      } else if (errors == "both") {
        extra.args$start <- c(rep(0.01, n.gammas1), rep(1, 4))
      }
    }
    if (is.null(extra.args$lower)) {
      if (errors == "neither") {
        extra.args$lower <- c(rep(-Inf, n.gammas1), rep(1e-3, 2))
      } else if (errors %in% c("processing", "measurement")) {
        extra.args$lower <- c(rep(-Inf, n.gammas1), rep(1e-3, 3))
      } else if (errors == "both") {
        extra.args$lower <- c(rep(-Inf, n.gammas1), rep(1e-3, 4))
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

    # Create list to return
    theta.hat <- ml.max1$par
    names(theta.hat) <- theta.labels1

    # If requested, add variance-covariance matrix to ret.list
    if (estimate_var) {

      # Estimate Hessian
      hessian.mat <- numDeriv::hessian(func = ll.f1, estimating.hessian = TRUE,
                                       x = ml.estimates)
      theta.variance <- try(solve(hessian.mat), silent = TRUE)
      if (class(theta.variance)[1] == "try-error" ||
          ! all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {

        # Repeatedly divide integrate_tol_hessian by 5 and re-try
        while (integrate_tol_hessian > 1e-15 & fix_posdef) {
          integrate_tol_hessian <- integrate_tol_hessian / 5
          hessian.mat <- numDeriv::hessian(func = ll.f1, estimating.hessian = TRUE,
                                           x = ml.estimates)
          theta.variance <- try(solve(hessian.mat), silent = TRUE)
          if (class(theta.variance)[1] != "try-error" &&
              all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {
            break
          }

        }
      }

      if (class(theta.variance)[1] == "try-error" ||
          ! all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {

        message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
        theta.variance <- NULL

      } else {
        colnames(theta.variance) <- rownames(theta.variance) <- theta.labels1
      }

    } else {
      theta.variance <- NULL
    }

    # Create vector of estimates and calculate AIC
    estimates1 <- ml.estimates
    names(estimates1) <- theta.labels1
    theta.var1 <- theta.variance
    aic1 <- 2 * (length(estimates1) + ml.max1$objective)

  }

  # Fit model without gamma_y
  if (is.null(constant_or) || constant_or) {

    # Log-likelihood function
    ll.f2 <- function(f.theta, estimating.hessian = FALSE) {

      # Extract parameters
      f.gammas <- matrix(f.theta[loc.gammas2], ncol = 1)
      f.b1 <- f.theta[loc.bs2[1]]
      f.b0 <- f.theta[loc.bs2[2]]

      if (errors == "neither") {
        f.sigsq_p <- 0
        f.sigsq_m <- 0
      } else if (errors == "measurement") {
        f.sigsq_p <- 0
        f.sigsq_m <- f.theta[loc.bs2[2] + 1]
      } else if (errors == "processing") {
        f.sigsq_p <- f.theta[loc.bs2[2] + 1]
        f.sigsq_m <- 0
      } else if (errors == "both") {
        f.sigsq_p <- f.theta[loc.bs2[2] + 1]
        f.sigsq_m <- f.theta[loc.bs2[2] + 2]
      }

      if (some.p) {

        # Likelihood for pools with precisely measured X:
        # L = f(X|Y,C_1,...,C_g)

        # a_i's in X|Y,C_1, ..., C_g ~ Gamma(a_i, b)
        if (some.cs) {
          alphas <- sapply(onec.p, function(x) sum(exp(x %*% f.gammas)))
        } else {
          alphas <- g.p * exp(f.gammas[1])
        }

        # Log-likelihood
        ll.p <- sum(dgamma(x = x.p,
                           shape = alphas,
                           scale = ifelse(y.p == 1, f.b1, f.b0), log = TRUE))

      } else {
        ll.p <- 0
      }

      # Set skip.rest flag to FALSE
      skip.rest <- FALSE

      if (some.r) {

        # Likelihood for pools with replicates
        # L = \int_X f(Xtilde|X) f(X|Y,C_1,...,C_g) dX

        # a_i's to feed to integral
        if (some.cs) {
          alphas <- sapply(onec.r, function(x) sum(exp(x %*% f.gammas)))
        } else {
          alphas <- g.r * exp(f.gammas[1])
        }

        # Function for integrating out X's
        int.f_i2a <- function(k_i, g_i, Ig_i, y_i, x_i, onec_i, xtilde_i, a_i) {

          # f(XtildeX|Y,C_1,...,C_g)
          x_i <- matrix(x_i, nrow = 1)
          f_xtildex.yc <- apply(x_i, 2, function(z) {

            # Transformation
            s_i <- z / (1 - z)

            # E[log(Xtilde)|X] and V[log(Xtilde|X)]
            Mu_logxtilde.x <-
              matrix(log(s_i) - 1/2 * (f.sigsq_p * Ig_i + f.sigsq_m), nrow = k_i)
            Sigma_logxtilde.x <- matrix(f.sigsq_p * Ig_i, ncol = k_i, nrow = k_i) +
              diag(rep(f.sigsq_m, k_i))

            # Density
            1 / prod(xtilde_i) * dmvnorm(x = log(xtilde_i),
                                         mean = Mu_logxtilde.x,
                                         sigma = Sigma_logxtilde.x) *
              dgamma(x = s_i, shape = a_i, scale = ifelse(y_i, f.b1, f.b0))

          })

          # Back-transformation
          out <- matrix(f_xtildex.yc / (1 - x_i)^2, ncol = ncol(x_i))

        }

        int.vals <- c()
        for (ii in 1: length(xtilde.r)) {

          # Values for ith subject
          g_i <- g.r[ii]
          Ig_i <- Ig.r[ii]
          k_i <- k.r[ii]
          y_i <- y.r[ii]
          onec_i <- onec.r[[ii]]
          xtilde_i <- xtilde.r[[ii]]
          a_i <- alphas[ii]

          int.ii <-
            adaptIntegrate(f = int.f_i2a, tol = integrate_tol,
                           lowerLimit = 0, upperLimit = 1,
                           vectorInterface = TRUE,
                           g_i = g_i, Ig_i = Ig_i, k_i = k_i, y_i = y_i,
                           onec_i = onec_i, xtilde_i = xtilde_i, a_i = a_i)
          int.vals[ii] <- int.ii$integral

          # If integral 0, set skip.rest to TRUE to skip further LL calculations
          if (int.ii$integral == 0) {
            print(paste("Integral is 0 for ii = ", ii, sep = ""))
            print(f.theta)
            print(int.ii)
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
        # L = \int_X f(Xtilde|X) f(X|Y,C_1,...,C_g) dX

        # a_i's to feed to integral
        if (some.cs) {
          alphas <- sapply(onec.i, function(x) sum(exp(x %*% f.gammas)))
        } else {
          alphas <- g.i * exp(f.gammas[1])
        }

        # Function for integrating out X's
        int.f_i2b <- function(g_i, Ig_i, y_i, x_i, onec_i, xtilde_i, a_i) {

          # f(XtildeX|Y,C_1,...,C_g)
          x_i <- matrix(x_i, nrow = 1)
          f_xtildex.yc <- apply(x_i, 2, function(z) {

            # Transformation
            s_i <- z / (1 - z)

            # E[log(Xtilde)|X] and V[log(Xtilde|X)]
            mu_logxtilde.x <- log(s_i) - 1/2 * (f.sigsq_p * Ig_i + f.sigsq_m)
            sigsq_logxtilde.x <- f.sigsq_p * Ig_i + f.sigsq_m

            # Density
            1 / xtilde_i * dnorm(x = log(xtilde_i),
                                 mean = mu_logxtilde.x,
                                 sd = sqrt(sigsq_logxtilde.x)) *
              dgamma(x = s_i, shape = a_i, scale = ifelse(y_i, f.b1, f.b0))

          })

          # Back-transformation
          out <- matrix(f_xtildex.yc / (1 - x_i)^2, ncol = ncol(x_i))

        }

        int.vals <- c()
        for (ii in 1: length(xtilde.i)) {

          # Values for ith subject
          g_i <- g.i[ii]
          Ig_i <- Ig.i[ii]
          y_i <- y.i[ii]
          onec_i <- onec.i[[ii]]
          xtilde_i <- xtilde.i[[ii]]
          a_i <- alphas[ii]

          int.ii <-
            adaptIntegrate(f = int.f_i2b, tol = integrate_tol,
                           lowerLimit = 0, upperLimit = 1,
                           vectorInterface = TRUE,
                           g_i = g_i, Ig_i = Ig_i, y_i = y_i, onec_i = onec_i,
                           xtilde_i = xtilde_i, a_i = a_i)
          int.vals[ii] <- int.ii$integral

          # If integral 0, set skip.rest to TRUE to skip further LL calculations
          if (int.ii$integral == 0) {
            print(paste("Integral is 0 for ii = ", ii, sep = ""))
            print(f.theta)
            print(int.ii)
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

    # Create list of extra arguments, and assign default starting values and lower
    # values if not specified by user
    extra.args <- list(...)
    if (is.null(extra.args$start)) {
      if (errors == "neither") {
        extra.args$start <- c(rep(0.01, n.gammas2), rep(1, 2))
      } else if (errors %in% c("processing", "measurement")) {
        extra.args$start <- c(rep(0.01, n.gammas2), rep(1, 3))
      } else if (errors == "both") {
        extra.args$start <- c(rep(0.01, n.gammas2), rep(1, 4))
      }
    }
    if (is.null(extra.args$lower)) {
      if (errors == "neither") {
        extra.args$lower <- c(rep(-Inf, n.gammas2), rep(1e-3, 2))
      } else if (errors %in% c("processing", "measurement")) {
        extra.args$lower <- c(rep(-Inf, n.gammas2), rep(1e-3, 3))
      } else if (errors == "both") {
        extra.args$lower <- c(rep(-Inf, n.gammas2), rep(1e-3, 4))
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
    b1.hat <- ml.estimates[loc.bs2[1]]
    b0.hat <- ml.estimates[loc.bs2[2]]
    logOR.hat <- 1 / b0.hat - 1 / b1.hat

    # Estimate variance of logOR.hat
    if (estimate_var) {

      # Estimate Hessian
      hessian.mat <- numDeriv::hessian(func = ll.f2, estimating.hessian = TRUE,
                                       x = ml.estimates)
      theta.variance <- try(solve(hessian.mat), silent = TRUE)
      if (class(theta.variance)[1] == "try-error" ||
          ! all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {

        # Repeatedly divide integrate_tol_hessian by 5 and re-try
        while (integrate_tol_hessian > 1e-15 & fix_posdef) {
          integrate_tol_hessian <- integrate_tol_hessian / 5
          hessian.mat <- numDeriv::hessian(func = ll.f2, estimating.hessian = TRUE,
                                           x = ml.estimates)
          theta.variance <- try(solve(hessian.mat), silent = TRUE)
          if (class(theta.variance)[1] != "try-error" &&
              all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {
            break
          }

        }
      }

      if (class(theta.variance)[1] == "try-error" ||
          ! all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {

        message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
        theta.variance <- NULL
        logOR.var <- NA

      } else {

        fprime <- matrix(c(1 / b1.hat^2, -1 / b0.hat^2), nrow = 1)
        colnames(theta.variance) <- rownames(theta.variance) <- theta.labels2
        logOR.var <- fprime %*% theta.variance[loc.bs2, loc.bs2] %*% t(fprime)

      }

    } else {

      theta.variance <- NULL
      logOR.var <- NA

    }

    # Create vector of estimates and calculate AIC
    estimates2 <- c(ml.estimates, logOR.hat, logOR.var)
    names(estimates2) <- c(theta.labels2, "logOR.hat", "logOR.var")
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

    return(list(lrt = lrt,
                estimates1 = estimates1,
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
