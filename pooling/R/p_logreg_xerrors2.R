#' Poolwise Logistic Regression with Gamma Exposure Subject to Errors
#'
#' Assumes constant-scale Gamma model for exposure given covariates, and
#' multiplicative lognormal processing errors and measurement errors acting on
#' the poolwise mean exposure. Manuscript fully describing the approach is
#' under review.
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
#' Mitchell, E.M, Lyles, R.H., and Schisterman, E.F. (2015) "Positing, fitting,
#' and selecting regression models for pooled biomarker data." \emph{Stat. Med}
#' \strong{34}(17): 2544--2558.
#'
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
#' fit1 <- p_logreg_xerrors2(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = dat$xtilde,
#'   c = c.list,
#'   errors = "neither"
#' )
#' fit1$theta.hat
#'
#' # Repeat, but accounting for processing error.
#' \dontrun{
#' fit2 <- p_logreg_xerrors2(
#'   g = dat$g,
#'   y = dat$y,
#'   xtilde = dat$xtilde,
#'   c = c.list,
#'   errors = "processing"
#' )
#' fit2$theta.hat
#' }
#'
#'
#' @export
p_logreg_xerrors2 <- function(
  g = NULL,
  y,
  xtilde,
  c = NULL,
  errors = "processing",
  nondiff_pe = TRUE,
  nondiff_me = TRUE,
  constant_pe = TRUE,
  prev = NULL,
  samp_y1y0 = NULL,
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
    if (! (length(samp_y1y0) == 2 & sum(samp_y1y0) == 1 &
           min(samp_y1y0) > 0 & max(samp_y1y0) < 1)) {
      stop("The input 'samp_y1y0' is the sampling probabilities for cases and controls, and should be a numeric vector of two probabilities adding to 1.")
    }
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
    n.cvars <- ncol(c[[1]])
    some.cs <- TRUE
    c.varnames <- colnames(c[[1]])
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

  # Figure out pool sizes if not specified
  if (is.null(g)) {
    g <- sapply(c, nrow)
  }

  # Create vector indicating which observations are pools
  Ig <- ifelse(g > 1, 1, 0)

  # Construct X|C design matrix list
  if (some.cs) {
    onec <- lapply(c, function(x) cbind(1, x))
  } else {
    onec <- NULL
  }

  # Calculate poolwise C's
  if (some.cs) {
    cstar <- matrix(sapply(c, colSums), byrow = TRUE, ncol = n.cvars)
  } else {
    cstar <- NULL
  }

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
    cstar.p <- cstar[which.p, , drop = FALSE]
    gxc.p <- cbind(g.p, x.p, cstar.p)
    qg.p <- qg[which.p]
    onec.p <- onec[which.p]
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
      xtilde.r <- xtilde[which.r]
      onec.r <- onec[which.r]
      cstar.r <- cstar[which.r, , drop = FALSE]
      qg.r <- qg[which.r]
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
    cstar.i <- cstar[which.i, , drop = FALSE]
    qg.i <- qg[which.i]
    onec.i <- onec[which.i]
    xtilde.i <- unlist(xtilde[which.i])
  }

  # Get indices for parameters being estimated and create labels
  loc.betas <- 1: n.betas
  beta.labels <- paste("beta", c("0", x.varname, c.varnames), sep = "_")

  loc.alphas <- (n.betas + 1): (n.betas + n.alphas)
  alpha.labels <- paste("alpha", c("0", c.varnames), sep = "_")

  loc.b <- n.betas + n.alphas + 1

  if (errors == "neither") {
    theta.labels <- c(beta.labels, alpha.labels, "b")
  } else if (errors == "processing") {
    if (! nondiff_pe) {
      loc.sigsq_p1 <- loc.b + 1
      loc.sigsq_p0 <- loc.b + 2
      theta.labels <- c(beta.labels, alpha.labels,
                        "b", "sigsq_p1", "sigsq_p0")
    } else {
      loc.sigsq_p1 <- loc.sigsq_p0 <- loc.b + 1
      theta.labels <- c(beta.labels, alpha.labels, "b", "sigsq_p")
    }
  } else if (errors == "measurement") {
    if (! nondiff_me) {
      loc.sigsq_m1 <- loc.b + 1
      loc.sigsq_m0 <- loc.b + 2
      theta.labels <- c(beta.labels, alpha.labels,
                        "b", "sigsq_m1", "sigsq_m0")
    } else {
      loc.sigsq_m1 <- loc.sigsq_m0 <- loc.b + 1
      theta.labels <- c(beta.labels, alpha.labels, "b", "sigsq_m")
    }
  } else if (errors == "both") {
    if (! nondiff_pe & ! nondiff_me) {
      loc.sigsq_p1 <- loc.b + 1
      loc.sigsq_p0 <- loc.b + 2
      loc.sigsq_m1 <- loc.b + 3
      loc.sigsq_m0 <- loc.b + 4
      theta.labels <- c(beta.labels, alpha.labels,
                        "b", "sigsq_p1", "sigsq_p0", "sigsq_m1",
                        "sigsq_m0")
    } else if (! nondiff_pe & nondiff_me) {
      loc.sigsq_p1 <- loc.b + 1
      loc.sigsq_p0 <- loc.b + 2
      loc.sigsq_m1 <- loc.sigsq_m0 <- loc.b + 3
      theta.labels <- c(beta.labels, alpha.labels,
                        "b", "sigsq_p1", "sigsq_p0", "sigsq_m")
    } else if (nondiff_pe & ! nondiff_me) {
      loc.sigsq_p1 <- loc.sigsq_p0 <- loc.b + 1
      loc.sigsq_m1 <- loc.b + 2
      loc.sigsq_m0 <- loc.b + 3
      theta.labels <- c(beta.labels, alpha.labels,
                        "b", "sigsq_p", "sigsq_m1", "sigsq_m0")
    } else if (nondiff_pe & nondiff_me) {
      loc.sigsq_p1 <- loc.sigsq_p0 <- loc.b + 1
      loc.sigsq_m1 <- loc.sigsq_m0 <- loc.b + 2
      theta.labels <- c(beta.labels, alpha.labels,
                        "b", "sigsq_p", "sigsq_m")
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

    f.b <- f.theta[loc.b]

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
      # L = f(Y|X,C) f(X|C_1,...,C_g)

      # P(Y|X,C)
      eta <- gxc.p %*% f.betas + qg.p
      p_y.xc <- (1 + exp(-eta))^(-1)

      # a_i's in X|C_1, ..., C_g ~ Gamma(a_i, b)
      if (some.cs) {
        alphas <- sapply(onec.p, function(x) sum(exp(x %*% f.alphas)))
      } else {
        alphas <- g.p * exp(f.alpha_0)
      }

      # Log-likelihood (non-positive X that generate NAs get excluded!)
      ll.p <- sum(dbinom(x = y.p, size = 1, prob = p_y.xc, log = TRUE) +
                    dgamma(x = x.p, shape = alphas, scale = f.b, log = TRUE))

    } else {
      ll.p <- 0
    }

    # Set skip.rest flag to FALSE
    skip.rest <- FALSE

    if (some.r) {

      # Likelihood for pools with replicates
      # L = \int_X f(Y|X,C) f(Xtilde|X) f(X|C_1,...,C_g) dX

      # Create error vectors
      sigsq_p <- ifelse(y.r > 0, f.sigsq_p1, f.sigsq_p0) * Ig.r
      sigsq_m <- ifelse(y.r > 0, f.sigsq_m1, f.sigsq_m0)

      # a_i's to feed to integral
      if (some.cs) {
        alphas <- sapply(onec.r, function(x) sum(exp(x %*% f.alphas)))
      } else {
        alphas <- g.r * exp(f.alpha_0)
      }

      # Function for integrating out X's
      int.f_i1 <- function(k_i, g_i, Ig_i, y_i, x_i, cstar_i, qg_i, xtilde_i,
                           a_i, sigsq_p_i, sigsq_m_i) {

        # f(Y,Xtilde,X|C_1,...,C_g)
        x_i <- matrix(x_i, nrow = 1)
        f_yxtildex.c <- apply(x_i, 2, function(z) {

          # Transformation
          s_i <- z / (1 - z)

          # P(Y|X,C)
          if (some.cs) {
            p_y.xc <- (1 + exp(-g_i * f.beta_0 - f.beta_x * s_i -
                                 as.vector(t(f.beta_c) %*% cstar_i) - qg_i))^(-1)
          } else {
            p_y.xc <- (1 + exp(-g_i * f.beta_0 - f.beta_x * s_i - qg_i))^(-1)
          }

          # E[log(Xtilde)|X] and V[log(Xtilde|X)]
          Mu_logxtilde.x <- matrix(log(s_i) - 1/2 * (sigsq_p_i + sigsq_m_i), nrow = k_i)
          Sigma_logxtilde.x <- matrix(sigsq_p_i, ncol = k_i, nrow = k_i) +
            diag(rep(sigsq_m_i, k_i))

          # Density
          dbinom(x = y_i, size = 1, prob = p_y.xc) *
            1 / prod(xtilde_i) * dmvnorm(x = log(xtilde_i),
                                         mean = Mu_logxtilde.x,
                                         sigma = Sigma_logxtilde.x) *
            dgamma(x = s_i, shape = a_i, scale = f.b)

        })

        # Back-transformation
        out <- matrix(f_yxtildex.c / (1 - x_i)^2, ncol = ncol(x_i))

      }

      int.vals <- c()
      for (ii in 1: length(xtilde.r)) {

        # Get values for ith participant
        g_i <- g.r[ii]
        Ig_i <- Ig.r[ii]
        k_i <- k.r[ii]
        y_i <- y.r[ii]
        cstar_i <- cstar.r[ii, ]
        qg_i <- qg.r[ii]
        xtilde_i <- xtilde.r[[ii]]
        a_i <- alphas[ii]
        sigsq_p_i <- sigsq_p[ii]
        sigsq_m_i <- sigsq_m[ii]

        # Try integrating out X with default settings
        int.ii <- do.call(hcubature,
                          c(list(f = int.f_i1,
                                 lowerLimit = 0,
                                 upperLimit = 1,
                                 vectorInterface = TRUE,
                                 g_i = g_i,
                                 Ig_i = Ig_i,
                                 k_i = k_i,
                                 y_i = y_i,
                                 cstar_i = cstar_i,
                                 qg_i = qg_i,
                                 xtilde_i = xtilde_i,
                                 a_i = a_i,
                                 sigsq_p_i = sigsq_p_i,
                                 sigsq_m_i = sigsq_m_i),
                            hcubature_list))

        # If integral 0, find region with density
        if (is.na(int.ii$integral) | int.ii$integral == 0) {

          limits <- seq(1e-5, 1 - 1e-5, 1e-5)
          fs <- int.f_i1(x_i = limits,
                         g_i = g_i,
                         Ig_i = Ig_i,
                         k_i = k_i,
                         y_i = y_i,
                         cstar_i = cstar_i,
                         qg_i = qg_i,
                         xtilde_i = xtilde_i,
                         a_i = a_i,
                         sigsq_p_i = sigsq_p_i,
                         sigsq_m_i = sigsq_m_i)
          limits <- limits[fs > 0]
          if (length(limits) > 0) {
            limits <- c(max(0, min(limits) - 1e-5), min(1, max(limits) + 1e-5))
            int.ii <- do.call(hcubature,
                              c(list(f = int.f_i1,
                                     lowerLimit = limits[1],
                                     upperLimit = limits[2],
                                     vectorInterface = TRUE,
                                     g_i = g_i,
                                     Ig_i = Ig_i,
                                     k_i = k_i,
                                     y_i = y_i,
                                     cstar_i = cstar_i,
                                     qg_i = qg_i,
                                     xtilde_i = xtilde_i,
                                     a_i = a_i,
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

    } else {
      ll.r <- 0
    }

    if (some.i & ! skip.rest) {

      # Likelihood for pools with single Xtilde:
      # L = \int_X f(Y|X,C) f(Xtilde|X) f(X|C_1,...,C_g) dX

      # Create error vectors
      sigsq_p <- ifelse(y.i > 0, f.sigsq_p1, f.sigsq_p0) * Ig.i
      sigsq_m <- ifelse(y.i > 0, f.sigsq_m1, f.sigsq_m0)

      # a_i's to feed to integral
      if (some.cs) {
        alphas <- sapply(onec.i, function(x) sum(exp(x %*% f.alphas)))
      } else {
        alphas <- g.i * exp(f.alpha_0)
      }

      # Function for integrating out X's
      int.f_i2 <- function(g_i, Ig_i, y_i, x_i, cstar_i, qg_i, xtilde_i, a_i,
                           sigsq_p_i, sigsq_m_i) {

        # Transformation
        s_i <- x_i / (1 - x_i)

        # P(Y|X,C)
        if (some.cs) {
          p_y.xc <- (1 + exp(-g_i * f.beta_0 - f.beta_x * s_i -
                               as.vector(t(f.beta_c) %*% cstar_i) - qg_i))^(-1)
        } else {
          p_y.xc <- (1 + exp(-g_i * f.beta_0 - f.beta_x * s_i - qg_i))^(-1)
        }

        # E[log(Xtilde)|X] and V[log(Xtilde|X)]
        mu_logxtilde.x <- log(s_i) - 1/2 * (sigsq_p_i + sigsq_m_i)
        sigsq_logxtilde.x <- sigsq_p_i + sigsq_m_i

        # f(Y,Xtilde,X|C_1,...,C_g) = f(Y|X,C) f(Xtilde|X) f(X|C_1,...,C_g)
        f_yxtildex.c <- dbinom(x = y_i, size = 1, prob = p_y.xc) *
          1 / xtilde_i * dnorm(x = log(xtilde_i),
                               mean = mu_logxtilde.x,
                               sd = sqrt(sigsq_logxtilde.x)) *
          dgamma(x = s_i, shape = a_i, scale = f.b)

        # Back-transformation
        out <- f_yxtildex.c / (1 - x_i)^2
        return(out)

      }

      int.vals <- c()
      for (ii in 1: length(xtilde.i)) {

        # Get values for ith participant
        g_i <- g.i[ii]
        Ig_i <- Ig.i[ii]
        y_i <- y.i[ii]
        cstar_i <- cstar.i[ii, ]
        qg_i <- qg.i[ii]
        xtilde_i <- xtilde.i[ii]
        a_i <- alphas[ii]
        sigsq_p_i <- sigsq_p[ii]
        sigsq_m_i <- sigsq_m[ii]

        # Try integrating out X_i with default settings
        int.ii <- do.call(hcubature,
                          c(list(f = int.f_i2,
                                 lowerLimit = 0,
                                 upperLimit = 1,
                                 vectorInterface = TRUE,
                                 g_i = g_i,
                                 Ig_i = Ig_i,
                                 y_i = y_i,
                                 cstar_i = cstar_i,
                                 qg_i = qg_i,
                                 xtilde_i = xtilde_i,
                                 a_i = a_i,
                                 sigsq_p_i = sigsq_p_i,
                                 sigsq_m_i = sigsq_m_i),
                            hcubature_list))

        # If integral 0, find region with density
        if (is.na(int.ii$integral) | int.ii$integral == 0) {

          limits <- seq(1e-5, 1 - 1e-5, 1e-5)
          fs <- int.f_i2(x_i = limits,
                         g_i = g_i,
                         Ig_i = Ig_i,
                         y_i = y_i,
                         cstar_i = cstar_i,
                         qg_i = qg_i,
                         xtilde_i = xtilde_i,
                         a_i = a_i,
                         sigsq_p_i = sigsq_p_i,
                         sigsq_m_i = sigsq_m_i)
          limits <- limits[fs > 0]
          if (length(limits) > 0) {
            limits <- c(max(0, min(limits) - 1e-5), min(1, max(limits) + 1e-5))
            int.ii <- do.call(hcubature,
                              c(list(f = int.f_i2,
                                     lowerLimit = limits[1],
                                     upperLimit = limits[2],
                                     vectorInterface = TRUE,
                                     g_i = g_i,
                                     Ig_i = Ig_i,
                                     y_i = y_i,
                                     cstar_i = cstar_i,
                                     qg_i = qg_i,
                                     xtilde_i = xtilde_i,
                                     a_i = a_i,
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
                             rep(start_nonvar_var[2], loc.sigsq_p0 - loc.b + 1))
    } else if (errors %in% c("measurement", "both")) {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas + n.alphas),
                             rep(start_nonvar_var[2], loc.sigsq_m0 - loc.b + 1))
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
                             rep(lower_nonvar_var[2], loc.sigsq_p0 - loc.b + 1))
    } else if (errors %in% c("measurement", "both")) {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas + n.alphas),
                             rep(lower_nonvar_var[2], loc.sigsq_m0 - loc.b + 1))
    }
  }

  # Upper bounds
  if (is.null(nlminb_list$upper)) {
    if (errors == "neither") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             upper_nonvar_var[2])
    } else if (errors == "processing") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             rep(upper_nonvar_var[2], loc.sigsq_p0 - loc.b + 1))
    } else if (errors %in% c("measurement", "both")) {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             rep(upper_nonvar_var[2], loc.sigsq_m0 - loc.b + 1))
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
      message("Estimated Hessian matrix (printed here) is singular, so variance-covariance matrix could not be obtained. You could try tweaking 'start_nonvar_var' or 'hessian_list' (e.g. increase 'r')")
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

  # return ret.list
  return(ret.list)

}
