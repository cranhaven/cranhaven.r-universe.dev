#' Poolwise Logistic Regression with Normal Exposure Subject to Errors
#'
#' Assumes normal linear model for exposure given covariates, and additive
#' normal processing errors and measurement errors acting on the poolwise mean
#' exposure. Manuscript fully describing the approach is under review.
#'
#'
#' @param g Numeric vector with pool sizes, i.e. number of members in each pool.
#' @param y Numeric vector with poolwise Y values, coded 0 if all members are
#' controls and 1 if all members are cases.
#' @param xtilde Numeric vector (or list of numeric vectors, if some pools have
#' replicates) with Xtilde values.
#' @param c Numeric matrix with poolwise \strong{C} values (if any), with one
#' row for each pool. Can be a vector if there is only 1 covariate.
#' @param errors Character string specifying the errors that X is subject to.
#' Choices are \code{"neither"}, \code{"processing"} for processing error
#' only, \code{"measurement"} for measurement error only, and \code{"both"}.
#' @param nondiff_pe Logical value for whether to assume the processing error
#' variance is non-differential, i.e. the same in case pools and control pools.
#' @param nondiff_me Logical value for whether to assume the measurement error
#' variance is non-differential, i.e. the same in case pools and control pools.
#' @param constant_pe Logical value for whether to assume the processing error
#' variance is constant with pool size. If \code{FALSE}, assumption is that
#' processing error variance increase with pool size such that, for example, the
#' processing error affecting a pool 2x as large as another has 2x the variance.
#' @param prev Numeric value specifying disease prevalence, allowing
#' for valid estimation of the intercept with case-control sampling. Can specify
#' \code{samp_y1y0} instead if sampling rates are known.
#' @param samp_y1y0 Numeric vector of length 2 specifying sampling probabilities
#' for cases and controls, allowing for valid estimation of the intercept with
#' case-control sampling. Can specify \code{prev} instead if it's easier.
#' @param approx_integral Logical value for whether to use the probit
#' approximation for the logistic-normal integral, to avoid numerically
#' integrating X's out of the likelihood function.
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
#' \code{\link[cubature]{hcubature}} for numerical integration. Only used if
#' \code{approx_integral = FALSE}.
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
#' Weinberg, C.R. and Umbach, D.M. (1999) "Using pooled exposure assessment to
#' improve efficiency in case-control studies." \emph{Biometrics} \strong{55}:
#' 718--726.
#'
#' Weinberg, C.R. and Umbach, D.M. (2014) "Correction to 'Using pooled exposure
#' assessment to improve efficiency in case-control studies' by Clarice R.
#' Weinberg and David M. Umbach; 55, 718--726, September 1999."
#' \emph{Biometrics} \strong{70}: 1061.
#'
#'
#' @examples
#' # Load dataset containing (Y, Xtilde, C) values for pools of size 1, 2, and
#' # 3. Xtilde values are affected by processing error.
#' data(pdat1)
#'
#' # Estimate log-OR for X and Y adjusted for C, ignoring processing error
#' fit1 <- p_logreg_xerrors(
#'   g = pdat1$g,
#'   y = pdat1$allcases,
#'   xtilde = pdat1$xtilde,
#'   c = pdat1$c,
#'   errors = "neither"
#' )
#' fit1$theta.hat
#'
#' # Repeat, but accounting for processing error. Closer to true log-OR of 0.5.
#' fit2 <- p_logreg_xerrors(
#'   g = pdat1$g,
#'   y = pdat1$allcases,
#'   xtilde = pdat1$xtilde,
#'   c = pdat1$c,
#'   errors = "processing"
#' )
#' fit2$theta.hat
#'
#'
#' @export
p_logreg_xerrors <- function(
  g,
  y,
  xtilde,
  c = NULL,
  errors = "processing",
  nondiff_pe = TRUE,
  nondiff_me = TRUE,
  constant_pe = TRUE,
  prev = NULL,
  samp_y1y0 = NULL,
  approx_integral = TRUE,
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
    stop("The input 'errors' should be set to 'neither', 'processing',
         'measurement', or 'both'.")
  }
  if (! is.logical(nondiff_pe)) {
    stop("The input 'nondiff_pe' should be TRUE if you want to assume non-differential processing error and FALSE otherwise.")
  }
  if (! is.logical(nondiff_me)) {
    stop("The input 'nondiff_me' should be TRUE if you want to assume non-differential measurement error and FALSE otherwise.")
  }
  if (! is.logical(constant_pe)) {
    stop("The input 'constant_pe' should be TRUE if you want to assume that processing error variance is constant with pool size and FALSE otherwise.")
  }
  if (! is.null(prev)) {
    if (prev < 0 | prev > 1) {
      stop("The input 'prev' is the disease prevalence, and must be between 0 and 1.")
    }
  }
  if (! is.null(samp_y1y0)) {
    if (! (length(samp_y1y0) == 2 &
           min(samp_y1y0) > 0 & max(samp_y1y0) < 1)) {
      stop("The input 'samp_y1y0' is the sampling probabilities for cases and controls, and should be a numeric vector of two probabilities.")
    }
  }
  if (! is.logical(approx_integral)) {
    stop("The input 'approx_integral' should be TRUE or FALSE.")
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

  # Get name of xtilde input
  x.varname <- deparse(substitute(xtilde))
  if (length(grep("$", x.varname, fixed = TRUE)) > 0) {
    x.varname <- substr(x.varname,
                        start = which(unlist(strsplit(x.varname, "")) == "$") + 1,
                        stop = nchar(x.varname))
  }

  # Get information about covariates C
  if (is.null(c)) {
    c.varnames <- NULL
    n.cvars <- 0
    some.cs <- FALSE
  } else {
    c.varname <- deparse(substitute(c))
    if (! is.matrix(c)) {
      c <- as.matrix(c)
    }
    n.cvars <- ncol(c)
    some.cs <- TRUE
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

  # Get number of betas and alphas
  n.betas <- 2 + n.cvars
  n.alphas <- 1 + n.cvars

  # Create indicator vector I(g > 1)
  Ig <- ifelse(g > 1, 1, 0)

  # Calculate offsets according to Weinberg and Umbach formula, incorporating
  # disease prevalence or sampling probabilities if known
  n <- length(y)
  locs.cases <- which(y == 1)
  n_1 <- sum(g[locs.cases])
  n_0 <- sum(g[-locs.cases])
  g.vals <- unique(g)
  qg <- rep(NA, n)

  if (! is.null(prev)) {

    for (jj in 1: length(g.vals)) {
      g.jj <- g.vals[jj]
      locs.g <- which(g == g.jj)
      n.casepools <- sum(g == g.jj & y == 1)
      n.controlpools <- sum(g == g.jj & y == 0)
      qg[locs.g] <- log(n.casepools / n.controlpools) -
        g.jj * log(prev / (1 - prev))
    }

  } else if (! is.null(samp_y1y0)) {

    for (jj in 1: length(g.vals)) {
      g.jj <- g.vals[jj]
      locs.g <- which(g == g.jj)
      n.casepools <- sum(g == g.jj & y == 1)
      n.controlpools <- sum(g == g.jj & y == 0)
      qg[locs.g] <- log(n.casepools / n.controlpools) -
        g.jj * log(n_1 / n_0) - g.jj * log(samp_y1y0[2] / samp_y1y0[1])
    }

  } else {

    for (jj in 1: length(g.vals)) {
      g.jj <- g.vals[jj]
      locs.g <- which(g == g.jj)
      n.casepools <- sum(g == g.jj & y == 1)
      n.controlpools <- sum(g == g.jj & y == 0)
      qg[locs.g] <- log(n.casepools / n.controlpools) - g.jj * log(n_1 / n_0)
    }

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
    y.p <- y[which.p]
    x.p <- unlist(xtilde[which.p])
    c.p <- c[which.p, , drop = FALSE]
    qg.p <- qg[which.p]
    gxc.p <- cbind(g.p, x.p, c.p)
    gc.p <- gxc.p[, -2, drop = FALSE]
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
      c.r <- c[which.r, , drop = FALSE]
      qg.r <- qg[which.r]
      gc.r <- cbind(g.r, c.r)
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
    c.i <- c[which.i, , drop = FALSE]
    qg.i <- qg[which.i]
    gc.i <- cbind(g.i, c.i)
    xtilde.i <- unlist(xtilde[which.i])
  }

  # Estimate (alpha, sigsq_x.c, sigsq_p, sigsq_m) if pseudo-lik...

  # Get indices for parameters being estimated and create labels
  loc.betas <- 1: n.betas
  beta.labels <- paste("beta", c("0", x.varname, c.varnames), sep = "_")

  loc.alphas <- (n.betas + 1): (n.betas + n.alphas)
  alpha.labels <- paste("alpha", c("0", c.varnames), sep = "_")

  loc.sigsq_x.c <- n.betas + n.alphas + 1

  if (errors == "neither") {
    theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c")
  } else if (errors == "processing") {
    if (! nondiff_pe) {
      loc.sigsq_p1 <- loc.sigsq_x.c + 1
      loc.sigsq_p0 <- loc.sigsq_x.c + 2
      theta.labels <- c(beta.labels, alpha.labels,
                        "sigsq_x.c", "sigsq_p1", "sigsq_p0")
    } else {
      loc.sigsq_p1 <- loc.sigsq_p0 <- loc.sigsq_x.c + 1
      theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_p")
    }
  } else if (errors == "measurement") {
    if (! nondiff_me) {
      loc.sigsq_m1 <- loc.sigsq_x.c + 1
      loc.sigsq_m0 <- loc.sigsq_x.c + 2
      theta.labels <- c(beta.labels, alpha.labels,
                        "sigsq_x.c", "sigsq_m1", "sigsq_m0")
    } else {
      loc.sigsq_m1 <- loc.sigsq_m0 <- loc.sigsq_x.c + 1
      theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_m")
    }
  } else if (errors == "both") {
    if (! nondiff_pe & ! nondiff_me) {
      loc.sigsq_p1 <- loc.sigsq_x.c + 1
      loc.sigsq_p0 <- loc.sigsq_x.c + 2
      loc.sigsq_m1 <- loc.sigsq_x.c + 3
      loc.sigsq_m0 <- loc.sigsq_x.c + 4
      theta.labels <- c(beta.labels, alpha.labels,
                        "sigsq_x.c", "sigsq_p1", "sigsq_p0", "sigsq_m1",
                        "sigsq_m0")
    } else if (! nondiff_pe & nondiff_me) {
      loc.sigsq_p1 <- loc.sigsq_x.c + 1
      loc.sigsq_p0 <- loc.sigsq_x.c + 2
      loc.sigsq_m1 <- loc.sigsq_m0 <- loc.sigsq_x.c + 3
      theta.labels <- c(beta.labels, alpha.labels,
                        "sigsq_x.c", "sigsq_p1", "sigsq_p0", "sigsq_m")
    } else if (nondiff_pe & ! nondiff_me) {
      loc.sigsq_p1 <- loc.sigsq_p0 <- loc.sigsq_x.c + 1
      loc.sigsq_m1 <- loc.sigsq_x.c + 2
      loc.sigsq_m0 <- loc.sigsq_x.c + 3
      theta.labels <- c(beta.labels, alpha.labels,
                        "sigsq_x.c", "sigsq_p", "sigsq_m1", "sigsq_m0")
    } else if (nondiff_pe & nondiff_me) {
      loc.sigsq_p1 <- loc.sigsq_p0 <- loc.sigsq_x.c + 1
      loc.sigsq_m1 <- loc.sigsq_m0 <- loc.sigsq_x.c + 2
      theta.labels <- c(beta.labels, alpha.labels,
                        "sigsq_x.c", "sigsq_p", "sigsq_m")
    }
  }

  # Log-likelihood function
  llf <- function(f.theta) {

    # Extract parameters
    f.betas <- matrix(f.theta[loc.betas], ncol = 1)
    f.beta_0 <- f.betas[1]
    f.beta_x <- f.betas[2]
    f.beta_c <- matrix(f.betas[-c(1: 2)], ncol = 1)

    f.alphas <- matrix(f.theta[loc.alphas], ncol = 1)
    f.alpha_0 <- f.alphas[1]
    f.alpha_c <- matrix(f.alphas[-1], ncol = 1)

    f.sigsq_x.c <- f.theta[loc.sigsq_x.c]

    if (errors == "neither") {
      f.sigsq_p1 <- f.sigsq_p0 <- f.sigsq_m1 <- f.sigsq_m0 <- 0
    }
    if (errors %in% c("processing", "both")) {
      f.sigsq_p1 <- f.theta[loc.sigsq_p1]
      f.sigsq_p0 <- f.theta[loc.sigsq_p0]
    } else {
      f.sigsq_p1 <- f.sigsq_p0 <- 0
    }
    if (errors %in% c("measurement", "both")) {
      f.sigsq_m1 <- f.theta[loc.sigsq_m1]
      f.sigsq_m0 <- f.theta[loc.sigsq_m0]
    } else {
      f.sigsq_m1 <- f.sigsq_m0 <- 0
    }

    if (some.p) {

      # Likelihood for pools with precisely measured X:
      # L = f(Y|X,C) f(X|C)

      # P(Y|X,C)
      eta <- gxc.p %*% f.betas + qg.p
      p_y.xc <- (1 + exp(-eta))^(-1)

      # E(X|C) and V(X|C)
      mu_x.c <- gc.p %*% f.alphas
      sigsq_x.c <- g.p * f.sigsq_x.c

      # Log-likelihood
      ll.p <-
        sum(dbinom(x = y.p, size = 1, prob = p_y.xc, log = TRUE) +
            dnorm(x = x.p, mean = mu_x.c, sd = sqrt(sigsq_x.c), log = TRUE))

    } else {
      ll.p <- 0
    }

    # Set skip.rest flag to FALSE
    skip.rest <- FALSE

    if (some.r) {

      # Likelihood for pools with replicates
      # L = f(Y, Xtilde|C)
      #   = [\int_X f(Y|X,C) f(X|Xtilde,C) dX] f(Xtilde|C)
      #   = int_X f(Y|X,C) f(Xtilde|X) f(X|C) dX

      # Create error vectors
      sigsq_p <- ifelse(y.r, f.sigsq_p1, f.sigsq_p0) * Ig.r
      sigsq_m <- ifelse(y.r, f.sigsq_m1, f.sigsq_m0)

      # Calculate E(X|C) and V(X|C)
      mu_x.c <- gc.r %*% f.alphas
      sigsq_x.c <- g.r * f.sigsq_x.c

      if (approx_integral) {

        # Probit approximation for logistic-normal integral

        ll.vals <- c()
        for (ii in 1: length(xtilde.r)) {

          # Values for ith subject
          k_i <- k.r[ii]
          g_i <- g.r[ii]
          y_i <- y.r[ii]
          c_i <- c.r[ii, ]
          qg_i <- qg.r[ii]
          mu_x.c_i <- mu_x.c[ii]
          sigsq_x.c_i <- sigsq_x.c[ii]
          xtilde_i <- xtilde.r[[ii]]
          sigsq_p_i <- sigsq_p[ii]
          sigsq_m_i <- sigsq_m[ii]

          # E(X|Xtilde,C) and V(X|Xtilde,C)
          Mu_xxtilde.c <- matrix(mu_x.c_i, nrow = k_i + 1)
          Sigma_xxtilde.c_11 <- sigsq_x.c_i
          Sigma_xxtilde.c_12 <- matrix(sigsq_x.c_i, ncol = k_i)
          Sigma_xxtilde.c_21 <- t(Sigma_xxtilde.c_12)
          Sigma_xxtilde.c_22 <- g_i * f.sigsq_x.c +
            g_i^2 * ifelse(constant_pe, 1, g_i) * sigsq_p_i +
            diag(x = g_i^2 * sigsq_m_i, ncol = k_i, nrow = k_i)

          mu_x.xtildec <- Mu_xxtilde.c[1] + Sigma_xxtilde.c_12 %*%
            solve(Sigma_xxtilde.c_22) %*% (xtilde_i - Mu_xxtilde.c[-1])
          sigsq_x.xtildec <- Sigma_xxtilde.c_11 - Sigma_xxtilde.c_12 %*%
            solve(Sigma_xxtilde.c_22) %*% Sigma_xxtilde.c_21

          # Approximation of \int_X f(Y|X,C) f(X|Xtilde,C) dX
          if (some.cs) {
            t <- (g_i * f.beta_0 + f.beta_x * mu_x.xtildec +
                    c_i %*% f.beta_c + qg_i) /
              sqrt(1 + sigsq_x.xtildec * f.beta_x^2 / 1.7^2)
          } else {
            t <- (g_i * f.beta_0 + f.beta_x * mu_x.xtildec + qg_i) /
              sqrt(1 + sigsq_x.xtildec * f.beta_x^2 / 1.7^2)
          }
          p <- exp(t) / (1 + exp(t))
          part1 <- dbinom(x = y_i, size = 1, prob = p, log = TRUE)

          # log[f(Xtilde|C)]
          if (k_i == 2) {

            mu_xtilde1.xtilde2c <- mu_x.c_i + Sigma_xxtilde.c_22[1, 2] /
              Sigma_xxtilde.c_22[2, 2] * (xtilde_i[2] - mu_x.c_i)
            sigsq_xtilde1.xtilde2c <- Sigma_xxtilde.c_22[1, 1] -
              Sigma_xxtilde.c_22[1, 2]^2 / Sigma_xxtilde.c_22[2, 2]
            part2 <- sum(dnorm(x = xtilde_i, log = TRUE,
                               mean = c(mu_xtilde1.xtilde2c, mu_x.c_i),
                               sd = sqrt(c(sigsq_xtilde1.xtilde2c,
                                           Sigma_xxtilde.c_22[2, 2]))))

          } else {

            part2 <- dmvnorm(x = xtilde_i, log = TRUE,
                             mean = Mu_xxtilde.c[-1],
                             sigma = Sigma_xxtilde.c_22)

          }

          # Log-likelihood
          ll.vals[ii] <- part1 + part2

        }
        ll.r <- sum(ll.vals)

      } else {

        # Full likelihood

        # Function for integrating out X's
        int.f_i1 <- function(k_i, g_i,
                             y_i, x_i, gc_i, qg_i,
                             mu_x.c_i, sigsq_x.c_i, xtilde_i,
                             sigsq_p_i, sigsq_m_i) {

          x_i <- matrix(x_i, nrow = 1)
          f_yxtildex.c <- apply(x_i, 2, function(z) {

            # Transformation
            s_i <- z / (1 - z^2)

            # P(Y_i|X_i^*,C_i^*)
            p_y.xc <-
              (1 + exp(-as.numeric(gc_i %*% f.betas[-2, , drop = FALSE]) -
                         s_i * f.beta_x - qg_i))^(-1)

            if (g_i == 1) {

              # f(Y,X,Xtilde|C) = f(Y|X,C) f(Xtilde1|Xtilde2,X) f(Xtilde2|X) f(X|C)
              dbinom(x = y_i, size = 1, prob = p_y.xc) *
                prod(dnorm(x = xtilde_i, mean = s_i, sd = sqrt(sigsq_m_i))) *
                dnorm(x = s_i, mean = mu_x.c_i, sd = sqrt(sigsq_x.c_i))

            } else {

              # E(Xtilde|X) and V(Xtilde|X)
              Mu_xtilde.x <- rep(s_i, k_i)
              # Sigma_xtilde.x <- g_i^2 * sigsq_p_i +
              #   diag(x = g_i^2 * sigsq_m_i, ncol = k_i, nrow = k_i)
              Sigma_xtilde.x <-
                g_i^2 * ifelse(constant_pe, 1, g_i) * sigsq_p_i +
                diag(x = g_i^2 * sigsq_m_i, ncol = k_i, nrow = k_i)

              # f(Y,X,Xtilde|C) = f(Y|X,C) f(Xtilde|X) f(X|C)
              dbinom(x = y_i, size = 1, prob = p_y.xc) *
                dmvnorm(x = xtilde_i, mean = Mu_xtilde.x,
                        sigma = Sigma_xtilde.x) *
                dnorm(x = s_i, mean = mu_x.c_i, sd = sqrt(sigsq_x.c_i))

            }

          })

          # Back-transformation
          out <- matrix(f_yxtildex.c * (1 + x_i^2) / (1 - x_i^2)^2,
                        ncol = ncol(x_i))
          return(out)

        }

        int.vals <- c()
        for (ii in 1: length(xtilde.r)) {

          # Get values for ith participant
          k_i <- k.r[ii]
          g_i <- g.r[ii]
          y_i <- y.r[ii]
          gc_i <- gc.r[ii, ]
          qg_i <- qg.r[ii]
          mu_x.c_i <- mu_x.c[ii]
          sigsq_x.c_i <- sigsq_x.c[ii]
          xtilde_i <- xtilde.r[[ii]]
          sigsq_p_i <- sigsq_p[ii]
          sigsq_m_i <- sigsq_m[ii]

          # Try integrating out X with default settings
          int.ii <- do.call(hcubature,
                            c(list(f = int.f_i1,
                                   lowerLimit = -1,
                                   upperLimit = 1,
                                   vectorInterface = TRUE,
                                   k_i = k_i,
                                   g_i = g_i,
                                   y_i = y_i,
                                   gc_i = gc_i,
                                   qg_i = qg_i,
                                   mu_x.c_i = mu_x.c_i,
                                   sigsq_x.c_i = sigsq_x.c_i,
                                   xtilde_i = xtilde_i,
                                   sigsq_p_i = sigsq_p_i,
                                   sigsq_m_i = sigsq_m_i),
                              hcubature_list))

          # If integral 0, find region with density
          if (is.na(int.ii$integral) | int.ii$integral == 0) {

            limits <- seq(-1 + 1e-5, 1 - 1e-5, 1e-5)
            fs <- int.f_i1(x_i = limits,
                           k_i = k_i,
                           g_i = g_i,
                           y_i = y_i,
                           gc_i = gc_i,
                           qg_i = qg_i,
                           mu_x.c_i = mu_x.c_i,
                           sigsq_x.c_i = sigsq_x.c_i,
                           xtilde_i = xtilde_i,
                           sigsq_p_i = sigsq_p_i,
                           sigsq_m_i = sigsq_m_i)
            limits <- limits[fs > 0]
            if (length(limits) > 0) {
              limits <- c(max(-1, min(limits) - 1e-5), min(1, max(limits) + 1e-5))
              int.ii <- do.call(hcubature,
                                c(list(f = int.f_i1,
                                       lowerLimit = limits[1],
                                       upperLimit = limits[2],
                                       vectorInterface = TRUE,
                                       k_i = k_i,
                                       g_i = g_i,
                                       y_i = y_i,
                                       gc_i = gc_i,
                                       qg_i = qg_i,
                                       mu_x.c_i = mu_x.c_i,
                                       sigsq_x.c_i = sigsq_x.c_i,
                                       xtilde_i = xtilde_i,
                                       sigsq_p_i = sigsq_p_i,
                                       sigsq_m_i = sigsq_m_i),
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

      }

    } else {
      ll.r <- 0
    }

    if (some.i & ! skip.rest) {

      # Likelihood for pools with single Xtilde:
      # L = f(Y,Xtilde|C) = [\int_X f(Y|X,C) f(X|Xtilde,C) dX] f(Xtilde|C)
      #                    = \int_X f(Y|X,C) f(Xtilde|X) f(X|C) dX

      # Create error vectors
      sigsq_p <- ifelse(y.i, f.sigsq_p1, f.sigsq_p0) * Ig.i
      sigsq_m <- ifelse(y.i, f.sigsq_m1, f.sigsq_m0)

      # Calculate E(X|C) and V(X|C)
      mu_x.c <- gc.i %*% f.alphas
      sigsq_x.c <- g.i * f.sigsq_x.c

      if (approx_integral) {

        # Probit approximation for logistic-normal integral

        # E(X,Xtilde|C) and V(X,Xtilde|C)
        Mu_xxtilde.c_1 <- mu_x.c
        Mu_xxtilde.c_2 <- mu_x.c
        Sigma_xxtilde.c_11 <- sigsq_x.c
        Sigma_xxtilde.c_12 <- sigsq_x.c
        if (constant_pe) {
          Sigma_xxtilde.c_22 <-
            g.i * f.sigsq_x.c + g.i^2 * sigsq_p + g.i^2 * sigsq_m
        } else {
          Sigma_xxtilde.c_22 <-
            g.i * f.sigsq_x.c + g.i^2 * g.i * sigsq_p + g.i^2 * sigsq_m
        }

        # E(X|Xtilde,C) and V(X|Xtilde,C)
        mu_x.xtildec <- Mu_xxtilde.c_1 + Sigma_xxtilde.c_12 /
          Sigma_xxtilde.c_22 * (xtilde.i - Mu_xxtilde.c_2)
        sigsq_x.xtildec <- Sigma_xxtilde.c_11 - Sigma_xxtilde.c_12^2 /
          Sigma_xxtilde.c_22

        # Approximation of \int_x f(Y|X,C) f(X|Xtilde,C) dx
        if (some.cs) {
          t <- as.numeric(g.i * f.beta_0 + f.beta_x * mu_x.xtildec +
                            c.i %*% f.beta_c + qg.i) /
            sqrt(1 + sigsq_x.xtildec * f.beta_x^2 / 1.7^2)
        } else {
          t <- (g.i * f.beta_0 + f.beta_x * mu_x.xtildec + qg.i) /
            sqrt(1 + sigsq_x.xtildec * f.beta_x^2 / 1.7^2)
        }
        p <- exp(t) / (1 + exp(t))
        part1 <- dbinom(x = y.i, size = 1, prob = p, log = TRUE)

        # log[f(Xtilde|C)]
        part2 <- dnorm(x = xtilde.i, log = TRUE,
                       mean = Mu_xxtilde.c_2, sd = sqrt(Sigma_xxtilde.c_22))

        # Log-likelihood
        ll.vals <- part1 + part2
        ll.i <- sum(ll.vals)

      } else {

        # Full likelihood

        # Function for integrating out X
        int.f_i2 <- function(g_i,
                             y_i, x_i, gc_i, qg_i,
                             mu_x.c_i, sigsq_x.c_i, xtilde_i,
                             sigsq_p_i, sigsq_m_i) {

          # Transformation
          s_i <- x_i / (1 - x_i^2)

          # P(Y|X,C)
          p_y.xc <-
            (1 + exp(as.numeric(-gc_i %*% f.betas[-2, , drop = FALSE]) -
                       s_i * f.beta_x - qg_i))^(-1)

          # E(Xtilde|X) and V(Xtilde|X)
          mu_xtilde.x <- s_i
          sigsq_xtilde.x <- g_i^2 * sigsq_p_i + g_i^2 * sigsq_m_i

          # f(Y,X,Xtilde|C) = f(Y|X,C) f(Xtilde|X) f(X|C)
          f_yx.xtildec <-
            dbinom(x = y_i, size = 1, prob = p_y.xc) *
            dnorm(x = xtilde_i, mean = mu_xtilde.x, sd = sqrt(sigsq_xtilde.x)) *
            dnorm(x = s_i, mean = mu_x.c_i, sd = sqrt(sigsq_x.c_i))

          # Back-transformation
          out <- f_yx.xtildec * (1 + x_i^2) / (1 - x_i^2)^2
          return(out)

        }

        int.vals <- c()
        for (ii in 1: length(xtilde.i)) {

          # Get values for ith participant
          g_i <- g.i[ii]
          y_i <- y.i[ii]
          gc_i <- gc.i[ii, ]
          qg_i <- qg.i[ii]
          mu_x.c_i <- mu_x.c[ii]
          sigsq_x.c_i <- sigsq_x.c[ii]
          xtilde_i <- xtilde.i[ii]
          sigsq_p_i <- sigsq_p[ii]
          sigsq_m_i <- sigsq_m[ii]

          # Try integrating out X_i with default settings
          int.ii <- do.call(hcubature,
                            c(list(f = int.f_i2,
                                   lowerLimit = -1,
                                   upperLimit = 1,
                                   vectorInterface = TRUE,
                                   g_i = g_i,
                                   y_i = y_i,
                                   gc_i = gc_i,
                                   qg_i = qg_i,
                                   mu_x.c_i = mu_x.c_i,
                                   sigsq_x.c_i = sigsq_x.c_i,
                                   xtilde_i = xtilde_i,
                                   sigsq_p_i = sigsq_p_i,
                                   sigsq_m_i = sigsq_m_i),
                              hcubature_list))

          # If integral 0, find region with density
          if (is.na(int.ii$integral) | int.ii$integral == 0) {

            limits <- seq(-1 + 1e-5, 1 - 1e-5, 1e-5)
            fs <- int.f_i2(x_i = limits,
                           g_i = g_i,
                           y_i = y_i,
                           gc_i = gc_i,
                           qg_i = qg_i,
                           mu_x.c_i = mu_x.c_i,
                           sigsq_x.c_i = sigsq_x.c_i,
                           xtilde_i = xtilde_i,
                           sigsq_p_i = sigsq_p_i,
                           sigsq_m_i = sigsq_m_i)
            limits <- limits[fs > 0]
            if (length(limits) > 0) {
              limits <- c(max(-1, min(limits) - 1e-5), min(1, max(limits) + 1e-5))
              int.ii <- do.call(hcubature,
                                c(list(f = int.f_i2,
                                       lowerLimit = -1,
                                       upperLimit = 1,
                                       vectorInterface = TRUE,
                                       g_i = g_i,
                                       y_i = y_i,
                                       gc_i = gc_i,
                                       qg_i = qg_i,
                                       mu_x.c_i = mu_x.c_i,
                                       sigsq_x.c_i = sigsq_x.c_i,
                                       xtilde_i = xtilde_i,
                                       sigsq_p_i = sigsq_p_i,
                                       sigsq_m_i = sigsq_m_i),
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

      }

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
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas + n.alphas),
                             start_nonvar_var[2])
    } else if (errors == "processing") {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas + n.alphas),
                             rep(start_nonvar_var[2], loc.sigsq_p0 - loc.sigsq_x.c + 1))
    } else if (errors %in% c("measurement", "both")) {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas + n.alphas),
                             rep(start_nonvar_var[2], loc.sigsq_m0 - loc.sigsq_x.c + 1))
    }
  }
  names(nlminb_list$start) <- theta.labels

  # Lower bounds
  if (is.null(nlminb_list$lower)) {
    if (errors == "neither") {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas + n.alphas),
                             lower_nonvar_var[2])
    } else if (errors == "processing") {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas + n.alphas),
                             rep(lower_nonvar_var[2], loc.sigsq_p0 - loc.sigsq_x.c + 1))
    } else if (errors %in% c("measurement", "both")) {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas + n.alphas),
                             rep(lower_nonvar_var[2], loc.sigsq_m0 - loc.sigsq_x.c + 1))
    }
  }

  # Upper bounds
  if (is.null(nlminb_list$upper)) {
    if (errors == "neither") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             upper_nonvar_var[2])
    } else if (errors == "processing") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             rep(upper_nonvar_var[2], loc.sigsq_p0 - loc.sigsq_x.c + 1))
    } else if (errors %in% c("measurement", "both")) {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             rep(upper_nonvar_var[2], loc.sigsq_m0 - loc.sigsq_x.c + 1))
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

      print(hessian.mat)
      message("The estimated Hessian matrix (printed here) is singular, so variance-covariance matrix could not be obtained. You could try tweaking 'start_nonvar_var' or 'hessian_list' (e.g. increase 'r')")
      ret.list$theta.var <- NULL

    } else {

      colnames(theta.variance) <- rownames(theta.variance) <- theta.labels
      ret.list$theta.var <- theta.variance

      if (sum(diag(theta.variance) <= 0) > 0) {
        print(theta.variance)
        message("The estimated variance-covariance matrix (printed here) has some non-positive diagonal elements, so it may not be reliable. You could try tweaking 'start_nonvar_var' or 'hessian_list' (e.g. increase 'r')")
      }

    }

  }

  # Add nlminb object and AIC to ret.list
  ret.list$nlminb.object <- ml.max
  ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)

  # Return ret.list
  return(ret.list)

}
