#' Conditional Logistic Regression with Measurement Error in One Covariate
#'
#' Compatible with individual or pooled measurements. Assumes a normal linear
#' model for exposure given other covariates, and additive normal errors.
#'
#'
#' @param g Numeric vector with pool sizes, i.e. number of members in each pool.
#' @param xtilde1 Numeric vector (or list of numeric vectors, if some
#' observations have replicates) with Xtilde values for cases.
#' @param xtilde0 Numeric vector (or list of numeric vectors, if some
#' observations have replicates) with Xtilde values for controls.
#' @param c1 Numeric matrix with precisely measured covariates for cases.
#' @param c0 Numeric matrix with precisely measured covariates for controls.
#' @param errors Character string specifying the errors that X is subject to.
#' Choices are \code{"none"}, \code{"measurement"} for measurement error,
#' \code{"processing"} for processing error (only relevant for pooled data), and
#' \code{"both"}.
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
#' Saha-Chaudhuri, P., Umbach, D.M. and Weinberg, C.R. (2011) "Pooled exposure
#' assessment for matched case-control studies." \emph{Epidemiology}
#' \strong{22}(5): 704--712.
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
#'
#' @examples
#' # Load simulated data for 150 case pools and 150 control pools
#' data(dat_cond_logreg)
#' dat <- dat_cond_logreg$dat
#' xtilde1 <- dat_cond_logreg$xtilde1
#' xtilde0 <- dat_cond_logreg$xtilde0
#'
#' # Fit conditional logistic regression to estimate log-odds ratio for X and Y
#' # adjusted for C, using the precise poolwise summed exposure X. True log-OR
#' # for X is 0.5.
#' truth <- cond_logreg(
#'   g = dat$g,
#'   xtilde1 = dat$x1,
#'   xtilde0 = dat$x0,
#'   c1 = dat$c1.model,
#'   c0 = dat$c0.model,
#'   errors = "neither"
#' )
#' truth$theta.hat
#'
#' # Suppose X is subject to additive measurement error and processing error,
#' # and we observe Xtilde1 and Xtilde0 rather than X1 and X0. Fit model with
#' # Xtilde's, accounting for errors (numerical integration avoided by using
#' # probit approximation).
#' \dontrun{
#' corrected <- cond_logreg(
#'   g = dat$g,
#'   xtilde1 = xtilde1,
#'   xtilde0 = xtilde0,
#'   c1 = dat$c1.model,
#'   c0 = dat$c0.model,
#'   errors = "both",
#'   approx_integral = TRUE
#' )
#' corrected$theta.hat
#' }
#'
#'
#' @export
cond_logreg <- function(
  g = rep(1, length(xtilde1)),
  xtilde1,
  xtilde0,
  c1 = NULL,
  c0 = NULL,
  errors = "processing",
  approx_integral = TRUE,
  estimate_var = FALSE,
  start_nonvar_var = c(0.01, 1),
  lower_nonvar_var = c(-Inf, 1e-4),
  upper_nonvar_var = c(Inf, Inf),
  jitter_start = 0.01,
  hcubature_list = list(tol = 1e-8),
  nlminb_list = list(control = list(trace = 1, eval.max = 500, iter.max = 500)),
  hessian_list = list(method.args = list(r = 4)),
  nlminb_object = NULL
) {

  # Get number of pools
  n <- length(g)

  # Figure out number of replicates for each pool
  k1 <- sapply(xtilde1, length)
  k0 <- sapply(xtilde0, length)

  # Get name of xtilde
  x.varname <- deparse(substitute(xtilde1))
  if (length(grep("$", x.varname, fixed = TRUE)) > 0) {
    x.varname <- substr(x.varname,
                        start = which(unlist(strsplit(x.varname, "")) == "$") + 1,
                        stop = nchar(x.varname))
  }

  # Get information about covariates
  if (is.null(c1)) {
    some.cs <- FALSE
    n.cvars <- 0
    c.varnames <- NULL
  } else {
    if (! is.matrix(c1)) {
      c1 <- as.matrix(c1)
      c0 <- as.matrix(c0)
    }
    n.cvars <- ncol(c1)
    some.cs <- TRUE
    c.varnames <- colnames(c1)
    if (is.null(c.varnames)) {
      c.varnames <- paste("c", 1: n.cvars, sep = "")
    }
  }

  # Get number of betas and alphas
  n.betas <- n.alphas <- 1 + n.cvars

  # Create indicator vector I(g > 1)
  Ig <- ifelse(g > 1, 1, 0)

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

    x1.p <- unlist(xtilde1[which.p])
    x0.p <- unlist(xtilde0[which.p])

    c1.p <- c1[which.p, , drop = FALSE]
    c0.p <- c0[which.p, , drop = FALSE]
    xc_diff.p <- cbind(x1.p - x0.p, c1.p - c0.p)

    gc1.p <- cbind(g.p, c1.p)
    gc0.p <- cbind(g.p, c0.p)

  }

  # Separate out pools with imprecisely measured X
  which.i <- setdiff(1: n, which.p)
  n.i <- n - n.p
  some.i <- n.i > 0
  if (some.i) {

    g.i <- g[which.i]
    Ig.i <- Ig[which.i]

    k1.i <- k1[which.i]
    k0.i <- k0[which.i]

    xtilde1.i <- xtilde1[which.i]
    xtilde0.i <- xtilde0[which.i]

    gc1.i <- cbind(g.i, c1[which.i, , drop = FALSE])
    gc0.i <- cbind(g.i, c0[which.i, , drop = FALSE])

    if (some.cs) {
      c_diff.i <- c1[which.i, , drop = FALSE] - c0[which.i, , drop = FALSE]
    }

  }

  # Get indices for parameters being estimated and create labels
  loc.betas <- 1: n.betas
  beta.labels <- paste("beta", c(x.varname, c.varnames), sep = "_")

  loc.alphas <- (n.betas + 1): (n.betas + n.alphas)
  alpha.labels <- paste("alpha", c("0", c.varnames), sep = "_")

  loc.sigsq_x.c <- n.betas + n.alphas + 1

  if (errors == "neither") {
    theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c")
  } else if (errors == "processing") {
    theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_p")
  } else if (errors == "measurement") {
    theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_m")
  } else if (errors == "both") {
    theta.labels <- c(beta.labels, alpha.labels,
                      "sigsq_x.c", "sigsq_p", "sigsq_m")
  }

  # Log-likelihood function for approximate ML
  llf.approx <- function(g, Ig,
                         k1, k0,
                         xtilde1, xtilde0,
                         mu_x1.c1, mu_x0.c0,
                         sigsq_x.c,
                         f.sigsq_p, f.sigsq_m,
                         f.beta_x,
                         cterm) {

    # Extract xtilde1 and xtilde0
    xtilde1 <- unlist(xtilde1)
    xtilde0 <- unlist(xtilde0)

    # Constants
    onek1 <- matrix(1, nrow = k1)
    onek0 <- matrix(1, nrow = k0)
    jk1 <- matrix(1, nrow = k1, ncol = k1)
    jk0 <- matrix(1, nrow = k0, ncol = k0)
    ik1 <- diag(k1)
    ik0 <- diag(k0)

    # E(X|Xtilde,C) and V(X|Xtilde,C)
    V1 <- jk1 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik1
    V0 <- jk0 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik0
    V1inv <- solve(V1)
    V0inv <- solve(V0)

    Mu_1 <- mu_x1.c1 + sigsq_x.c * t(onek1) %*% V1inv %*%
      (xtilde1 - onek1 * mu_x1.c1)
    Mu_2 <- mu_x0.c0 + sigsq_x.c * t(onek0) %*% V0inv %*%
      (xtilde0 - onek0 * mu_x0.c0)

    Sigma_11 <- sigsq_x.c - sigsq_x.c^2 * t(onek1) %*% V1inv %*% onek1
    Sigma_22 <- sigsq_x.c - sigsq_x.c^2 * t(onek0) %*% V0inv %*% onek0

    # Approximation
    t <- (f.beta_x * (Mu_1 - Mu_2) + cterm) /
      sqrt(1 + f.beta_x^2 * (Sigma_11 + Sigma_22) / 1.7^2)

    p <- exp(t) / (1 + exp(t))
    part1 <- log(p)

    # Part outside integral
    part2 <-
      dmvnorm(x = xtilde1, log = TRUE, mean = rep(mu_x1.c1, k1), sigma = V1) +
      dmvnorm(x = xtilde0, log = TRUE, mean = rep(mu_x0.c0, k0), sigma = V0)

    return(part1 + part2)

  }

  # Likelihood function for full ML
  lf.full <- function(g, Ig,
                      k1, k0,
                      xtilde1, xtilde0,
                      mu_x1.c1,
                      mu_x0.c0,
                      sigsq_x.c,
                      f.sigsq_p, f.sigsq_m,
                      f.beta_x,
                      cterm,
                      x10) {

    x10 <- matrix(x10, nrow = 2)
    dens <- apply(x10, 2, function(z) {

      # Transformation
      x1 <- z[1]
      x0 <- z[2]
      s1 <- x1 / (1 - x1^2)
      s0 <- x0 / (1 - x0^2)

      # Constants
      onek1 <- matrix(1, nrow = k1)
      onek0 <- matrix(1, nrow = k0)
      zerok1 <- matrix(0, nrow = k1)
      zerok0 <- matrix(0, nrow = k0)
      jk1 <- matrix(1, nrow = k1, ncol = k1)
      jk0 <- matrix(1, nrow = k0, ncol = k0)
      ik1 <- diag(k1)
      ik0 <- diag(k0)

      # E(X1*,X2*,Xtilde1*,Xtilde2*) and V()
      Mu <- matrix(c(mu_x1.c1,
                     mu_x0.c0,
                     rep(mu_x1.c1, k1),
                     rep(mu_x0.c0, k0)),
                   ncol = 1)

      V1 <- jk1 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik1
      V0 <- jk0 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik0

      Sigma <- cbind(
        matrix(c(sigsq_x.c, 0, onek1 * sigsq_x.c, zerok0,
                 0, sigsq_x.c, zerok1, onek0 * sigsq_x.c),
               ncol = 2),
        rbind(c(sigsq_x.c * onek1, zerok0),
              c(zerok1, sigsq_x.c * onek0),
              cbind(V1, zerok1 %*% t(zerok0)),
              cbind(zerok0 %*% t(zerok1), V0))
      )

      # f(Y1,Y2,X1*,X2*,Xtilde1*,Xtilde2*)
      (1 + exp(-f.beta_x * (s1 - s0) - cterm))^(-1) *
        dmvnorm(x = c(s1, s0, xtilde1, xtilde0),
                mean = Mu,
                sigma = Sigma)

    })

    # Back-transformation
    out <- matrix(
      dens * (1 + x10[1, ]^2) / (1 - x10[1, ]^2)^2 *
        (1 + x10[2, ]^2) / (1 - x10[2, ]^2)^2,
      ncol = ncol(x10)
    )
    return(out)

  }

  # Log-likelihood function
  llf <- function(f.theta) {

    # Extract parameters
    f.betas <- matrix(f.theta[loc.betas], ncol = 1)
    f.beta_x <- f.betas[1]
    f.beta_c <- matrix(f.betas[-1], ncol = 1)

    f.alphas <- matrix(f.theta[loc.alphas], ncol = 1)
    f.alpha_0 <- f.alphas[1]
    f.alpha_c <- matrix(f.alphas[-1], ncol = 1)

    f.sigsq_x.c <- f.theta[loc.sigsq_x.c]

    if (errors == "neither") {
      f.sigsq_p <- 0
      f.sigsq_m <- 0
    } else if (errors == "processing") {
      f.sigsq_p <- f.theta[loc.sigsq_x.c + 1]
      f.sigsq_m <- 0
    } else if (errors == "measurement") {
      f.sigsq_p <- 0
      f.sigsq_m <- f.theta[loc.sigsq_x.c + 1]
    } else if (errors == "both") {
      f.sigsq_p <- f.theta[loc.sigsq_x.c + 1]
      f.sigsq_m <- f.theta[loc.sigsq_x.c + 2]
    }

    if (some.p) {

      # Likelihood for pools with precisely measured X:
      # L = f(Y|X) f(X)
      mu_x1.c1 <- gc1.p %*% f.alphas
      mu_x0.c0 <- gc0.p %*% f.alphas

      ll.p <- sum(
        -log(1 + exp(-xc_diff.p %*% f.betas)) +
          dnorm(x = x1.p, log = TRUE,
                mean = mu_x1.c1,
                sd = sqrt(g.p * f.sigsq_x.c)) +
          dnorm(x = x0.p, log = TRUE,
                mean = mu_x0.c0,
                sd = sqrt(g.p * f.sigsq_x.c))
      )

    } else {
      ll.p <- 0
    }

    if (some.i) {

      # Likelihood for pools with single Xtilde:
      # L = f(Y,Xtilde|C) = [\int_X f(Y|X,C) f(X|Xtilde,C) dX] f(Xtilde|C)
      #                    = \int_X f(Y|X,C) f(Xtilde|X) f(X|C) dX

      # beta_c^T (C_i1* - C_i0*) term
      if (some.cs) {
        cterms <- c_diff.i %*% f.beta_c
      } else {
        cterms <- rep(0, n.i)
      }

      # E(X|C) and V(X|C) terms
      mu_x1.c1s <- gc1.i %*% f.alphas
      mu_x0.c0s <- gc0.i %*% f.alphas
      sigsq_x.cs <- g.i * f.sigsq_x.c

      if (approx_integral) {

        ll.i <- mapply(
          FUN = llf.approx,
          g = g.i,
          Ig = Ig.i,
          k1 = k1.i,
          k0 = k0.i,
          xtilde1 = xtilde1.i,
          xtilde0 = xtilde0.i,
          mu_x1.c1 = mu_x1.c1s,
          mu_x0.c0 = mu_x0.c0s,
          sigsq_x.c = sigsq_x.cs,
          f.sigsq_p = f.sigsq_p,
          f.sigsq_m = f.sigsq_m,
          f.beta_x = f.beta_x,
          cterm = cterms
        )
        ll.i <- sum(ll.i)

      } else {

        int.vals <- c()
        for (ii in 1: n.i) {

          # Perform integration
          int.ii <- do.call(hcubature,
                            c(list(f = lf.full,
                                   lowerLimit = rep(-1, 2),
                                   upperLimit = rep(1, 2),
                                   vectorInterface = TRUE,
                                   g = g.i[ii],
                                   Ig = Ig.i[ii],
                                   k1 = k1.i[ii],
                                   k0 = k0.i[ii],
                                   xtilde1 = unlist(xtilde1.i[ii]),
                                   xtilde0 = unlist(xtilde0.i[ii]),
                                   mu_x1.c1 = mu_x1.c1s[ii],
                                   mu_x0.c0 = mu_x0.c0s[ii],
                                   sigsq_x.c = sigsq_x.cs[ii],
                                   f.sigsq_p = f.sigsq_p,
                                   f.sigsq_m = f.sigsq_m,
                                   f.beta_x = f.beta_x,
                                   cterm = cterms[ii]),
                              hcubature_list))
          int.vals[ii] <- int.ii$integral
          if (int.ii$integral == 0) {
            print(paste("Integral is 0 for ii = ", ii, sep = ""))
            print(f.theta)
            break
          }

        }

        ll.i <- sum(log(int.vals))

      }

    } else {

      ll.i <- 0

    }

    # Return negative log-likelihood
    ll <- ll.p + ll.i
    return(-ll)

  }

  # Starting values
  if (is.null(nlminb_list$start)) {
    if (errors == "neither") {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas + n.alphas),
                             start_nonvar_var[2])
    } else if (errors %in% c("processing", "measurement")) {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas + n.alphas),
                             rep(start_nonvar_var[2], 2))
    } else if (errors == "both") {
      nlminb_list$start <- c(rep(start_nonvar_var[1], n.betas + n.alphas),
                             rep(start_nonvar_var[2], 3))
    }
  }
  names(nlminb_list$start) <- theta.labels

  # Lower bounds
  if (is.null(nlminb_list$lower)) {
    if (errors == "neither") {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas + n.alphas),
                             lower_nonvar_var[2])
    } else if (errors %in% c("processing", "measurement")) {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas + n.alphas),
                             rep(lower_nonvar_var[2], 2))
    } else if (errors == "both") {
      nlminb_list$lower <- c(rep(lower_nonvar_var[1], n.betas + n.alphas),
                             rep(lower_nonvar_var[2], 3))
    }
  }

  # Upper bounds
  if (is.null(nlminb_list$upper)) {
    if (errors == "neither") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             upper_nonvar_var[2])
    } else if (errors %in% c("processing", "measurement")) {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             rep(upper_nonvar_var[2], 2))
    } else if (errors == "both") {
      nlminb_list$upper <- c(rep(upper_nonvar_var[1], n.betas + n.alphas),
                             rep(upper_nonvar_var[2], 3))
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

  # Return ret.list
  return(ret.list)

}

# Archived 5/6/2018

# # Generate big pool of data
# n <- 1000
# n.sample <- 100
# sigsq_p <- 0.5
# c <- rnorm(n)
# c1 <- rnorm(n)
# x <- 0.25 + 0.2 * c + 0.3 * c1 + rnorm(n = n, sd = sqrt(1))
# y <- rbinom(n = n, size = 1, prob = (1 + exp(-0.5 - 0.5 * x - 0.1 * c - 0.2 * c1))^(-1))
# dat <- data.frame(y = y, x = x, c = c)
#
# # Match cases and controls on C
# match <- Match(Tr = y,
#                X = matrix(c),
#                replace = FALSE,
#                M = 1)
# dat.matched <- data.frame(
#   y1 = rep(1, n.sample),
#   y0 = rep(0, n.sample),
#   x1 = x[match$index.treated[1: n.sample]],
#   x0 = x[match$index.control[1: n.sample]],
#   c1 = c1[match$index.treated[1: n.sample]],
#   c0 = c1[match$index.control[1: n.sample]]
# )
#
# # Create pools of size 1 and 2
# dat.pooled <- p.neighbors(dat = dat.matched,
#                           pool.sizes = c(1, 2),
#                           n.pools = c(n.sample / 2, n.sample / 4))
# dat.pooled$y1 <- 1
#
# # PE only
# np <- nrow(dat.pooled)
# xtilde1 <- (dat.pooled$x1 / dat.pooled$g +
#   rnorm(n = np, sd = sqrt(sigsq_p)) * ifelse(dat.pooled$g > 1, 1, 0)) *
#   dat.pooled$g
# xtilde0 <- (dat.pooled$x0 / dat.pooled$g +
#               rnorm(n = np, sd = sqrt(sigsq_p)) * ifelse(dat.pooled$g > 1, 1, 0)) *
#   dat.pooled$g
#
#
# # PE and ME with replicates
# np <- nrow(dat.pooled)
# k <- rep(1, np)
# k[1: 30] <- 2
# xtilde1 <- list()
# xtilde0 <- list()
# for (ii in 1: nrow(dat.pooled)) {
#   xtilde1[[ii]] <- (dat.pooled$x1[ii] / dat.pooled$g[ii] +
#                       rnorm(1, sd = sqrt(sigsq_p)) * ifelse(dat.pooled$g[ii] > 1, 1, 0) +
#                       rnorm(n = k[ii], sd = sqrt(sigsq_m))) * dat.pooled$g[ii]
#   xtilde0[[ii]] <- (dat.pooled$x0[ii] / dat.pooled$g[ii] +
#                       rnorm(1, sd = sqrt(sigsq_p)) * ifelse(dat.pooled$g[ii] > 1, 1, 0) +
#                       rnorm(n = k[ii], sd = sqrt(sigsq_m))) * dat.pooled$g[ii]
# }
#
# c1 <- matrix(dat.pooled$c1, dimnames = list(NULL, "c1"))
# c0 <- matrix(dat.pooled$c0, dimnames = list(NULL, "c1"))
# g <- dat.pooled$g
#
# # Unobservable truth
# abc <- cond_logreg(g = dat.pooled$g,
#                    xtilde1 = dat.pooled$x1,
#                    xtilde0 = dat.pooled$x0,
#                    c1 = c1,
#                    c0 = c0,
#                    errors = "neither",
#                    control = list(trace = 1))
# print(abc$theta.hat)
#
# # # Naive
# # abc <- cond_logreg(g = dat.pooled$g,
# #                    xtilde1 = dat.pooled$xtilde1,
# #                    xtilde2 = dat.pooled$xtilde2,
# #                    errors = "neither",
# #                    control = list(trace = 1))
#
# # Corrected - approx ML
# abc <- cond_logreg(g = dat.pooled$g,
#                    xtilde1 = xtilde1,
#                    xtilde0 = xtilde0,
#                    c1 = c1,
#                    c0 = c0,
#                    errors = "processing",
#                    control = list(trace = 1))
# print(abc$theta.hat)
#
# # Corrected - full ML
# abc <- cond_logreg(g = dat.pooled$g,
#                    xtilde1 = xtilde1,
#                    xtilde0 = xtilde0,
#                    errors = "processing",
#                    approx_integral = FALSE,
#                    control = list(trace = 1))
# print(abc$theta.hat)
#
#
# # xtilde1 and xtilde2 are vectors or lists
# # c1 and c2 are matrices
# # g is a vector
# # Don't need y1 and y0 because all y1's are 1. Maybe switch to 1/0 notation
# # to make it clear that 1 = case and 0 = control!
#
# cond_logreg <- function(g = rep(1, length(xtilde1)),
#                         xtilde1,
#                         xtilde0,
#                         c1 = NULL,
#                         c0 = NULL,
#                         errors = "both",
#                         approx_integral = TRUE,
#                         integrate_tol = 1e-8,
#                         integrate_tol_hessian = integrate_tol,
#                         estimate_var = TRUE,
#                         ...) {
#
#   # Get number of pools
#   n <- length(g)
#
#   # Figure out number of replicates for each pool
#   k1 <- sapply(xtilde1, length)
#   k0 <- sapply(xtilde0, length)
#
#   # Get name of xtilde
#   x.varname <- deparse(substitute(xtilde1))
#   if (length(grep("$", x.varname, fixed = TRUE)) > 0) {
#     x.varname <- substr(x.varname,
#                         start = which(unlist(strsplit(x.varname, "")) == "$") + 1,
#                         stop = nchar(x.varname))
#   }
#
#   # Get information about covariates
#   if (is.null(c1)) {
#     some.cs <- FALSE
#     n.cvars <- 0
#     c.varnames <- NULL
#   } else {
#     if (class(c1) != "matrix") {
#       c1 <- as.matrix(c)
#       c0 <- as.matrix(c)
#     }
#     n.cvars <- ncol(c1)
#     some.cs <- TRUE
#     c.varnames <- colnames(c1)
#     if (is.null(c.varnames)) {
#       c.varnames <- paste("c", 1: n.cvars, sep = "")
#     }
#   }
#
#   # Get number of betas and alphas
#   n.betas <- n.alphas <- 1 + n.cvars
#
#   # Create indicator vector I(g > 1)
#   Ig <- ifelse(g > 1, 1, 0)
#
#   # Separate out pools with precisely measured X
#   if (errors == "neither") {
#     which.p <- 1: n
#   } else if (errors == "processing") {
#     which.p <- which(Ig == 0)
#   } else {
#     which.p <- NULL
#   }
#   n.p <- length(which.p)
#   some.p <- n.p > 0
#   if (some.p) {
#
#     g.p <- g[which.p]
#     Ig.p <- Ig[which.p]
#
#     x1.p <- unlist(xtilde1[which.p])
#     x0.p <- unlist(xtilde0[which.p])
#
#     c1.p <- c1[which.p, , drop = FALSE]
#     c0.p <- c0[which.p, , drop = FALSE]
#     xc_diff.p <- cbind(x1.p - x0.p, c1.p - c0.p)
#
#     gc1.p <- cbind(g.p, c1.p)
#     gc0.p <- cbind(g.p, c0.p)
#
#   }
#
#   # Separate out pools with imprecisely measured X
#   which.i <- setdiff(1: n, which.p)
#   n.i <- n - n.p
#   some.i <- n.i > 0
#   if (some.i) {
#
#     g.i <- g[which.i]
#     Ig.i <- Ig[which.i]
#
#     k1.i <- k1[which.i]
#     k0.i <- k0[which.i]
#
#     xtilde1.i <- xtilde1[which.i]
#     xtilde0.i <- xtilde0[which.i]
#
#     gc1.i <- cbind(g.i, c1[which.i, , drop = FALSE])
#     gc0.i <- cbind(g.i, c0[which.i, , drop = FALSE])
#
#     if (some.cs) {
#       c_diff.i <- c1[which.i, , drop = FALSE] - c0[which.i, , drop = FALSE]
#     }
#
#   }
#
#   # Get indices for parameters being estimated and create labels
#   loc.betas <- 1: n.betas
#   beta.labels <- paste("beta", c(x.varname, c.varnames), sep = "_")
#
#   loc.alphas <- (n.betas + 1): (n.betas + n.alphas)
#   alpha.labels <- paste("alpha", c("0", c.varnames), sep = "_")
#
#   loc.sigsq_x.c <- n.betas + n.alphas + 1
#
#   if (errors == "neither") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c")
#   } else if (errors == "processing") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_p")
#   } else if (errors == "measurement") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_m")
#   } else if (errors == "both") {
#     theta.labels <- c(beta.labels, alpha.labels,
#                       "sigsq_x.c", "sigsq_p", "sigsq_m")
#   }
#
#   # Log-likelihood function for approximate ML
#   llf.approx <- function(g, Ig,
#                          k1, k0,
#                          xtilde1, xtilde0,
#                          mu_x1.c1, mu_x0.c0,
#                          sigsq_x.c,
#                          f.sigsq_p, f.sigsq_m,
#                          f.beta_x,
#                          cterm) {
#
#     # Extract xtilde1 and xtilde0
#     xtilde1 <- unlist(xtilde1)
#     xtilde0 <- unlist(xtilde0)
#
#     # Constants
#     onek1 <- matrix(1, nrow = k1)
#     onek0 <- matrix(1, nrow = k0)
#     jk1 <- matrix(1, nrow = k1, ncol = k1)
#     jk0 <- matrix(1, nrow = k0, ncol = k0)
#     ik1 <- diag(k1)
#     ik0 <- diag(k0)
#
#     # E(X|Xtilde,C) and V(X|Xtilde,C)
#     V1 <- jk1 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik1
#     V0 <- jk0 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik0
#     V1inv <- solve(V1)
#     V0inv <- solve(V0)
#
#     Mu_1 <- mu_x1.c1 + sigsq_x.c * t(onek1) %*% V1inv %*%
#       (xtilde1 - onek1 * mu_x1.c1)
#     Mu_2 <- mu_x0.c0 + sigsq_x.c * t(onek0) %*% V0inv %*%
#       (xtilde0 - onek0 * mu_x0.c0)
#
#     Sigma_11 <- sigsq_x.c - sigsq_x.c^2 * t(onek1) %*% V1inv %*% onek1
#     Sigma_22 <- sigsq_x.c - sigsq_x.c^2 * t(onek0) %*% V0inv %*% onek0
#
#     # Approximation
#     t <- (f.beta_x * (Mu_1 - Mu_2) + cterm) /
#       sqrt(1 + f.beta_x^2 * (Sigma_11 + Sigma_22) / 1.7^2)
#
#     p <- exp(t) / (1 + exp(t))
#     part1 <- log(p)
#
#     # Part outside integral
#     part2 <-
#       dmvnorm(x = xtilde1, log = TRUE, mean = rep(mu_x1.c1, k1), sigma = V1) +
#       dmvnorm(x = xtilde0, log = TRUE, mean = rep(mu_x0.c0, k0), sigma = V0)
#
#     return(part1 + part2)
#
#   }
#
#   # Likelihood function for full ML
#   lf.full <- function(g, Ig,
#                       k1, k0,
#                       xtilde1, xtilde0,
#                       mu_x1.c1,
#                       mu_x0.c0,
#                       sigsq_x.c,
#                       f.sigsq_p, f.sigsq_m,
#                       f.beta_x,
#                       cterm,
#                       x10) {
#
#     x10 <- matrix(x10, nrow = 2)
#     dens <- apply(x10, 2, function(z) {
#
#       # Transformation
#       x1 <- z[1]
#       x0 <- z[2]
#       s1 <- x1 / (1 - x1^2)
#       s0 <- x0 / (1 - x0^2)
#
#       # Constants
#       onek1 <- matrix(1, nrow = k1)
#       onek0 <- matrix(1, nrow = k0)
#       zerok1 <- matrix(0, nrow = k1)
#       zerok0 <- matrix(0, nrow = k0)
#       jk1 <- matrix(1, nrow = k1, ncol = k1)
#       jk0 <- matrix(1, nrow = k0, ncol = k0)
#       ik1 <- diag(k1)
#       ik0 <- diag(k0)
#
#       # E(X1*,X2*,Xtilde1*,Xtilde2*) and V()
#       Mu <- matrix(c(mu_x1.c1,
#                      mu_x0.c0,
#                      rep(mu_x1.c1, k1),
#                      rep(mu_x0.c0, k0)),
#                    ncol = 1)
#
#       V1 <- jk1 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik1
#       V0 <- jk0 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik0
#
#       Sigma <- cbind(
#         matrix(c(sigsq_x.c, 0, onek1 * sigsq_x.c, zerok0,
#                  0, sigsq_x.c, zerok1, onek0 * sigsq_x.c),
#                ncol = 2),
#         rbind(c(sigsq_x.c * onek1, zerok0),
#               c(zerok1, sigsq_x.c * onek0),
#               cbind(V1, zerok1 %*% t(zerok0)),
#               cbind(zerok0 %*% t(zerok1), V0))
#       )
#
#       # f(Y1,Y2,X1*,X2*,Xtilde1*,Xtilde2*)
#       (1 + exp(-f.beta_x * (s1 - s0) - cterm))^(-1) *
#         dmvnorm(x = c(s1, s0, xtilde1, xtilde0),
#                 mean = Mu,
#                 sigma = Sigma)
#
#     })
#
#     # Back-transformation
#     out <- matrix(
#       dens * (1 + x10[1, ]^2) / (1 - x10[1, ]^2)^2 *
#         (1 + x10[2, ]^2) / (1 - x10[2, ]^2)^2,
#       ncol = ncol(x10)
#     )
#     return(out)
#
#   }
#
#   # Log-likelihood function
#   llf <- function(f.theta, estimating.hessian = FALSE) {
#
#     # Extract parameters
#     f.betas <- matrix(f.theta[loc.betas], ncol = 1)
#     f.beta_x <- f.betas[1]
#     f.beta_c <- matrix(f.betas[-1], ncol = 1)
#
#     f.alphas <- matrix(f.theta[loc.alphas], ncol = 1)
#     f.alpha_0 <- f.alphas[1]
#     f.alpha_c <- matrix(f.alphas[-1], ncol = 1)
#
#     f.sigsq_x.c <- f.theta[loc.sigsq_x.c]
#
#     if (errors == "neither") {
#       f.sigsq_p <- 0
#       f.sigsq_m <- 0
#     } else if (errors == "processing") {
#       f.sigsq_p <- f.theta[loc.sigsq_x.c + 1]
#       f.sigsq_m <- 0
#     } else if (errors == "measurement") {
#       f.sigsq_p <- 0
#       f.sigsq_m <- f.theta[loc.sigsq_x.c + 1]
#     } else if (errors == "both") {
#       f.sigsq_p <- f.theta[loc.sigsq_x.c + 1]
#       f.sigsq_m <- f.theta[loc.sigsq_x.c + 2]
#     }
#
#     if (some.p) {
#
#       # Likelihood for pools with precisely measured X:
#       # L = f(Y|X) f(X)
#       mu_x1.c1 <- gc1.p %*% f.alphas
#       mu_x0.c0 <- gc0.p %*% f.alphas
#
#       ll.p <- sum(
#         -log(1 + exp(-xc_diff.p %*% f.betas)) +
#           dnorm(x = x1.p, log = TRUE,
#                 mean = mu_x1.c1,
#                 sd = sqrt(g.p * f.sigsq_x.c)) +
#           dnorm(x = x0.p, log = TRUE,
#                 mean = mu_x0.c0,
#                 sd = sqrt(g.p * f.sigsq_x.c))
#       )
#
#     } else {
#       ll.p <- 0
#     }
#
#     if (some.i) {
#
#       # Likelihood for pools with single Xtilde:
#       # L = f(Y,Xtilde|C) = [\int_X f(Y|X,C) f(X|Xtilde,C) dX] f(Xtilde|C)
#       #                    = \int_X f(Y|X,C) f(Xtilde|X) f(X|C) dX
#
#       # beta_c^T (C_i1* - C_i0*) term
#       if (some.cs) {
#         cterms <- c_diff.i %*% f.beta_c
#       } else {
#         cterms <- rep(0, n.i)
#       }
#
#       # E(X|C) and V(X|C) terms
#       mu_x1.c1s <- gc1.i %*% f.alphas
#       mu_x0.c0s <- gc0.i %*% f.alphas
#       sigsq_x.cs <- g.i * f.sigsq_x.c
#
#       if (approx_integral) {
#
#         ll.i <- mapply(
#           FUN = llf.approx,
#           g = g.i,
#           Ig = Ig.i,
#           k1 = k1.i,
#           k0 = k0.i,
#           xtilde1 = xtilde1.i,
#           xtilde0 = xtilde0.i,
#           mu_x1.c1 = mu_x1.c1s,
#           mu_x0.c0 = mu_x0.c0s,
#           sigsq_x.c = sigsq_x.cs,
#           f.sigsq_p = f.sigsq_p,
#           f.sigsq_m = f.sigsq_m,
#           f.beta_x = f.beta_x,
#           cterm = cterms
#         )
#         ll.i <- sum(ll.i)
#
#       } else {
#
#         int.vals <- c()
#         for (ii in 1: n.i) {
#
#           # Perform integration
#           int.ii <-
#             adaptIntegrate(f = lf.full,
#                            tol = integrate_tol,
#                            lowerLimit = rep(-1, 2),
#                            upperLimit = rep(1, 2),
#                            vectorInterface = TRUE,
#                            g = g.i[ii],
#                            Ig = Ig.i[ii],
#                            k1 = k1.i[ii],
#                            k0 = k0.i[ii],
#                            xtilde1 = unlist(xtilde1.i[ii]),
#                            xtilde0 = unlist(xtilde0.i[ii]),
#                            mu_x1.c1 = mu_x1.c1s[ii],
#                            mu_x0.c0 = mu_x0.c0s[ii],
#                            sigsq_x.c = sigsq_x.cs[ii],
#                            f.sigsq_p = f.sigsq_p,
#                            f.sigsq_m = f.sigsq_m,
#                            f.beta_x = f.beta_x,
#                            cterm = cterms[ii])
#           int.vals[ii] <- int.ii$integral
#
#         }
#
#         ll.i <- sum(log(int.vals))
#
#       }
#
#     } else {
#
#       ll.i <- 0
#
#     }
#
#     # Return negative log-likelihood
#     ll <- ll.p + ll.i
#     return(-ll)
#
#   }
#
#   # Create list of extra arguments, and assign default starting values and
#   # lower values if not specified by user
#   extra.args <- list(...)
#   if (is.null(extra.args$start)) {
#     if (errors == "neither") {
#       extra.args$start <- c(rep(0.01, n.betas + n.alphas), 1)
#     } else if (errors %in% c("processing", "measurement")) {
#       extra.args$start <- c(rep(0.01, n.betas + n.alphas), rep(1, 2))
#     } else if (errors == "both") {
#       extra.args$start <- c(rep(0.01, n.betas + n.alphas), rep(1, 3))
#     }
#   }
#   if (is.null(extra.args$lower)) {
#     if (errors == "neither") {
#       extra.args$lower <- c(rep(-Inf, 2), 1e-3)
#     } else if (errors %in% c("processing", "measurement")) {
#       extra.args$lower <- c(rep(-Inf, 2), rep(1e-3, 2))
#     } else if (errors == "both") {
#       extra.args$lower <- c(rep(-Inf, 2), rep(1e-3, 3))
#     }
#   }
#   if (is.null(extra.args$control$rel.tol)) {
#     extra.args$control$rel.tol <- 1e-6
#   }
#   if (is.null(extra.args$control$eval.max)) {
#     extra.args$control$eval.max <- 1000
#   }
#   if (is.null(extra.args$control$iter.max)) {
#     extra.args$control$iter.max <- 750
#   }
#
#   # Obtain ML estimates
#   ml.max <- do.call(nlminb, c(list(objective = llf), extra.args))
#
#   # Create list to return
#   theta.hat <- ml.max$par
#   names(theta.hat) <- theta.labels
#   ret.list <- list(theta.hat = theta.hat)
#
#   # If requested, add variance-covariance matrix to ret.list
#   if (estimate_var) {
#     hessian.mat <- hessian(f = llf, estimating.hessian = TRUE,
#                                    x0 = theta.hat)
#     theta.variance <- try(solve(hessian.mat), silent = TRUE)
#     if (class(theta.variance) == "try-error") {
#       print(hessian.mat)
#       message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
#       ret.list$theta.var <- NULL
#     } else {
#       colnames(theta.variance) <- rownames(theta.variance) <- theta.labels
#       ret.list$theta.var <- theta.variance
#     }
#   }
#
#   # Add nlminb object and AIC to ret.list
#   ret.list$nlminb.object <- ml.max
#   ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)
#
#   # Return ret.list
#   return(ret.list)
#
# }
#
# # Archived 5/6/2018
# cond_logreg <- function(g,
#                         xtilde1,
#                         xtilde0,
#                         c1 = NULL,
#                         c0 = NULL,
#                         errors = "both",
#                         approx_integral = TRUE,
#                         integrate_tol = 1e-8,
#                         estimate_var = TRUE,
#                         ...) {
#
#   # Get number of pools
#   n <- length(g)
#
#   # Figure out number of replicates for each pool
#   k1 <- sapply(xtilde1, length)
#   k0 <- sapply(xtilde0, length)
#
#   # Get name of xtilde
#   x.varname <- deparse(substitute(xtilde1))
#   if (length(grep("$", x.varname, fixed = TRUE)) > 0) {
#     x.varname <- substr(x.varname,
#                         start = which(unlist(strsplit(x.varname, "")) == "$") + 1,
#                         stop = nchar(x.varname))
#   }
#
#   # Get information about covariates C
#   if (is.null(c1)) {
#     some.cs <- FALSE
#     n.cvars <- 0
#     c.varnames <- NULL
#   } else {
#     if (class(c1) != "matrix") {
#       c1 <- as.matrix(c)
#       c0 <- as.matrix(c)
#     }
#     n.cvars <- ncol(c1)
#     some.cs <- TRUE
#     c.varnames <- colnames(c1)
#     if (is.null(c.varnames)) {
#       c.varnames <- paste("c", 1: n.cvars, sep = "")
#     }
#   }
#
#   # Get number of betas and alphas
#   n.betas <- n.alphas <- 1 + n.cvars
#
#   # Create indicator vector I(g > 1)
#   Ig <- ifelse(g > 1, 1, 0)
#
#   # Separate out pools with precisely measured X
#   if (errors == "neither") {
#     which.p <- 1: n
#   } else if (errors == "processing") {
#     which.p <- which(Ig == 0)
#   } else {
#     which.p <- NULL
#   }
#   n.p <- length(which.p)
#   some.p <- n.p > 0
#   if (some.p) {
#
#     g.p <- g[which.p]
#     Ig.p <- Ig[which.p]
#
#     x1.p <- unlist(xtilde1[which.p])
#     x0.p <- unlist(xtilde0[which.p])
#     xc_diff.p <- cbind(x1.p - x0.p, c1.p - c0.p)
#
#     if (some.cs) {
#
#       gc1.p <- cbind(rep(1, n.p), c1[which.p, ])
#       gc0.p <- cbind(rep(1, n.p), c0[which.p, ])
#
#     }
#
#     # c1.p <- c1[which.p, , drop = FALSE]
#     # c0.p <- c0[which.p, , drop = FALSE]
#
#   }
#
#   # Separate out pools with imprecisely measured X
#   which.i <- setdiff(1: n, which.p)
#   n.i <- n - n.p
#   some.i <- n.i > 0
#   if (some.i) {
#
#     g.i <- g[which.i]
#     Ig.i <- Ig[which.i]
#
#     k1.i <- k1[which.i]
#     k0.i <- k0[which.i]
#
#     xtilde1.i <- xtilde1[which.i]
#     xtilde0.i <- xtilde0[which.i]
#
#     if (some.cs) {
#
#       gc1.i <- cbind(g.i, c1[which.i, ])
#       gc0.i <- cbind(g.i, c0[which.i, ])
#       c_diff.i <- c1[which.i, , drop = FALSE] - c0[which.i, , drop = FALSE]
#
#     }
#
#   }
#
#   # Get indices for parameters being estimated and create labels
#   loc.betas <- 1: n.betas
#   beta.labels <- paste("beta", c(x.varname, c.varnames), sep = "_")
#
#   loc.alphas <- (n.betas + 1): (n.betas + n.alphas)
#   alpha.labels <- paste("alpha", c("0", c.varnames), sep = "_")
#
#   loc.sigsq_x.c <- n.betas + n.alphas + 1
#
#   if (errors == "neither") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c")
#   } else if (errors == "processing") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_p")
#   } else if (errors == "measurement") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_m")
#   } else if (errors == "both") {
#     theta.labels <- c(beta.labels, alpha.labels,
#                       "sigsq_x.c", "sigsq_p", "sigsq_m")
#   }
#
#   # Log-likelihood function for approximate ML
#   llf.approx <- function(g, Ig,
#                          k1, k0,
#                          xtilde1, xtilde0,
#                          mu_x1.c1, mu_x0.c0,
#                          sigsq_x.c,
#                          f.sigsq_p, f.sigsq_m,
#                          f.beta_x,
#                          cterm) {
#
#     # Extract xtilde1 and xtilde0
#     xtilde1 <- unlist(xtilde1)
#     xtilde0 <- unlist(xtilde0)
#
#     # Constants
#     onek1 <- matrix(1, nrow = k1)
#     onek0 <- matrix(1, nrow = k0)
#     jk1 <- matrix(1, nrow = k1, ncol = k1)
#     jk0 <- matrix(1, nrow = k0, ncol = k0)
#     ik1 <- diag(k1)
#     ik0 <- diag(k0)
#
#     # E(X|Xtilde,C) and V(X|Xtilde,C)
#     V1 <- jk1 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik1
#     V0 <- jk0 * (sigsq_x.c + g^2 * f.sigsq_p * Ig) + g^2 * f.sigsq_m * ik0
#     V1inv <- solve(V1)
#     V0inv <- solve(V0)
#
#     Mu_1 <- mu_x1.c1 + sigsq_x.c * t(onek1) %*% V1inv %*%
#       (xtilde1 - onek1 * mu_x1.c1)
#     Mu_2 <- mu_x0.c0 + sigsq_x.c * t(onek0) %*% V0inv %*%
#       (xtilde0 - onek0 * mu_x0.c0)
#
#     Sigma_11 <- sigsq_x.c - sigsq_x.c^2 * t(onek1) %*% V1inv %*% onek1
#     Sigma_22 <- sigsq_x.c - sigsq_x.c^2 * t(onek0) %*% V0inv %*% onek0
#
#     # Approximation
#     onemat <- matrix(c(1, -1), nrow = 1)
#     t <- (f.beta_x * (Mu_1 - Mu_2) + cterm) /
#       sqrt(1 + f.beta_x^2 * (Sigma_11 + Sigma_22) / 1.7^2)
#
#     p <- exp(t) / (1 + exp(t))
#     part1 <- log(p)
#
#     # Part outside integral
#     part2 <-
#       sum(dmvnorm(x = xtilde1, log = TRUE, mean = rep(Mu_1, k1), sigma = V1)) +
#       sum(dmvnorm(x = xtilde0, log = TRUE, mean = rep(Mu_2, k0), sigma = V0))
#
#     return(part1 + part2)
#
#   }
#
#   # Likelihood function for full ML
#   lf.full <- function(g, Ig,
#                       k1, k0,
#                       xtilde1, xtilde0,
#                       c1, c0,
#                       f.sigsq_x.c, f.sigsq_p, f.sigsq_m,
#                       f.beta_x,
#                       cterm,
#                       f.alpha_0, f.alpha_c,
#                       x10) {
#
#     x10 <- matrix(x10, nrow = 2)
#     dens <- apply(x10, 2, function(z) {
#
#       # Transformation
#       x1 <- z[1]
#       x0 <- z[2]
#       s1 <- x1 / (1 - x1^2)
#       s0 <- x0 / (1 - x0^2)
#
#       # Constants
#       onek1 <- matrix(1, nrow = k1)
#       onek0 <- matrix(1, nrow = k0)
#       zerok1 <- matrix(0, nrow = k1)
#       zerok0 <- matrix(0, nrow = k0)
#       jk1 <- matrix(1, nrow = k1, ncol = k1)
#       jk0 <- matrix(1, nrow = k0, ncol = k0)
#       ik1 <- diag(k1)
#       ik0 <- diag(k0)
#
#       # E(X1*,X2*,Xtilde1*,Xtilde2*) and V()
#       mu_x.xtildec_1 <- g * f.alpha_0 + sum(f.alpha_c * c1)
#       mu_x.xtildec_2 <- g * f.alpha_0 + sum(f.alpha_c * c0)
#       Mu <- matrix(c(mu_x.xtildec_1,
#                      mu_x.xtildec_2,
#                      rep(mu_x.xtildec_1, k1),
#                      rep(mu_x.xtildec_2, k0)),
#                    ncol = 1)
#
#       V1 <- jk1 * (g * f.sigsq_x.c + g^2 * f.sigsq_p * Ig) +
#         g^2 * f.sigsq_m * ik1
#       V0 <- jk0 * (g * f.sigsq_x.c + g^2 * f.sigsq_p * Ig) +
#         g^2 * f.sigsq_m * ik0
#
#       Sigma <- cbind(
#         matrix(c(g * f.sigsq_x, 0, onek1 * g * f.sigsq_x, zerok0,
#                  0, g * f.sigsq_x, zerok1, onek0 * g * f.sigsq_x),
#                ncol = 2),
#         rbind(c(g * f.sigsq_x * onek1, zerok0),
#               c(zerok1, g * f.sigsq_x * onek0),
#               cbind(V1, zerok1 %*% t(zerok0)),
#               cbind(zerok0 %*% t(zerok1), V0))
#       )
#
#       # f(Y1,Y2,X1*,X2*,Xtilde1*,Xtilde2*)
#       (1 + exp(-f.beta_x * (s1 - s0) - cterm))^(-1) *
#         dmvnorm(x = c(s1, s0, xtilde1, xtilde0),
#                 mean = Mu,
#                 sigma = Sigma)
#
#     })
#
#     # Back-transformation
#     out <- matrix(
#       dens * (1 + x10[1]^2) / (1 - x10[1]^2)^2 *
#         (1 + x10[2]^2) / (1 - x10[2]^2)^2,
#       ncol = ncol(x10)
#     )
#
#     return(out)
#
#   }
#
#   # Log-likelihood function
#   llf <- function(f.theta, estimating.hessian = FALSE) {
#
#     # Extract parameters
#     f.betas <- matrix(f.theta[loc.betas], ncol = 1)
#     f.beta_x <- f.betas[1]
#     f.beta_c <- matrix(f.betas[-1], ncol = 1)
#
#     f.alphas <- matrix(f.theta[loc.alphas], ncol = 1)
#     f.alpha_0 <- f.alphas[1]
#     f.alpha_c <- matrix(f.alphas[-1], ncol = 1)
#
#     f.sigsq_x.c <- f.theta[loc.sigsq_x.c]
#
#     if (errors == "neither") {
#       f.sigsq_p <- 0
#       f.sigsq_m <- 0
#     } else if (errors == "processing") {
#       f.sigsq_p <- f.theta[loc.sigsq_x.c + 1]
#       f.sigsq_m <- 0
#     } else if (errors == "measurement") {
#       f.sigsq_p <- 0
#       f.sigsq_m <- f.theta[loc.sigsq_x.c + 1]
#     } else if (errors == "both") {
#       f.sigsq_p <- f.theta[loc.sigsq_x.c + 1]
#       f.sigsq_m <- f.theta[loc.sigsq_x.c + 2]
#     }
#
#     if (some.p) {
#
#       # Likelihood for pools with precisely measured X:
#       # L = f(Y|X) f(X)
#       if (some.cs) {
#         mu_x1.c1 <- gc1.p %*% f.alphas
#         mu_x0.c0 <- gc0.p %*% f.alphas
#       } else {
#         mu_x1.c1 <- mu_x0.c0 <- g.p * f.alpha_0
#       }
#
#       ll.p <- sum(
#         -log(1 + exp(-xc_diff.p %*% f.betas)) +
#           dnorm(x = x1.p, log = TRUE,
#                 mean = mu_x1.c1,
#                 sd = sqrt(g.p * f.sigsq_x.c)) +
#           dnorm(x = x0.p, log = TRUE,
#                 mean = mu_x0.c0,
#                 sd = sqrt(g.p * f.sigsq_x.c))
#       )
#
#     } else {
#       ll.p <- 0
#     }
#
#     if (some.i) {
#
#       # Likelihood for pools with single Xtilde:
#       # L = f(Y,Xtilde|C) = [\int_X f(Y|X,C) f(X|Xtilde,C) dX] f(Xtilde|C)
#       #                    = \int_X f(Y|X,C) f(Xtilde|X) f(X|C) dX
#
#       if (approx_integral) {
#
#         # beta_c^T (C_i1* - C_i0*) term
#         if (some.cs) {
#           cterms <- c_diff.i %*% f.beta_c
#         } else {
#           cterms <- rep(0, n.i)
#         }
#
#         # E(X|C) and V(X|C) terms
#         if (some.cs) {
#           mu_x1.c1s <- gc1.i %*% f.alphas
#           mu_x0.c0s <- gc0.i %*% f.alphas
#         } else {
#           mu_x1.c1s <- mu_x0.c0s <- g.i * f.alpha_0
#         }
#         sigsq_x.cs <- g.i * f.sigsq_x.c
#
#         ll.i <- sum(mapply(
#           FUN = llf.approx,
#           g = g.i,
#           Ig = Ig.i,
#           k1 = k1.i,
#           k0 = k0.i,
#           xtilde1 = xtilde1.i,
#           xtilde0 = xtilde0.i,
#           mu_x1.c1 = mu_x1.c1s,
#           mu_x0.c0 = mu_x0.c0s,
#           sigsq_x.c = sigsq_x.cs,
#           f.sigsq_p = f.sigsq_p,
#           f.sigsq_m = f.sigsq_m,
#           f.beta_x = f.beta_x,
#           cterm = cterms
#         ))
#
#       } else {
#
#         int.vals <- c()
#         for (ii in 1: length(xtilde1.i)) {
#
#           # Perform integration
#           int.ii <-
#             adaptIntegrate(f = lf.full,
#                            tol = 1e-3,
#                            lowerLimit = rep(-1, 2),
#                            upperLimit = rep(1, 2),
#                            vectorInterface = TRUE,
#                            g = g.i[ii],
#                            Ig = Ig.i[ii],
#                            k1 = k1.i[ii],
#                            k0 = k0.i[ii],
#                            xtilde1 = unlist(xtilde1.i[ii]),
#                            xtilde0 = unlist(xtilde0.i[ii]),
#                            c1 = c1.i[ii, , drop = FALSE],
#                            c0 = c0.i[ii, , drop = FALSE],
#                            f.sigsq_x.c = f.sigsq_x.c,
#                            f.sigsq_p = f.sigsq_p,
#                            f.sigsq_m = f.sigsq_m,
#                            f.beta_x = f.beta_x,
#                            cterm = ifelse(some.cs, sum(f.beta_c * c_diff.i), 0),
#                            f.alpha_0 = f.alpha_0,
#                            f.alpha_c = f.alpha_c)
#
#           int.vals[ii] <- int.ii$integral
#
#         }
#
#         ll.i <- sum(log(int.vals))
#
#       }
#
#     } else {
#
#       ll.i <- 0
#
#     }
#
#     # Return negative log-likelihood
#     ll <- ll.p + ll.i
#     return(-ll)
#
#   }
#
#   # Create list of extra arguments, and assign default starting values and
#   # lower values if not specified by user
#   extra.args <- list(...)
#   if (is.null(extra.args$start)) {
#     if (errors == "neither") {
#       extra.args$start <- c(rep(0.01, 2), 1)
#     } else if (errors %in% c("processing", "measurement")) {
#       extra.args$start <- c(rep(0.01, 2), rep(1, 2))
#     } else if (errors == "both") {
#       extra.args$start <- c(rep(0.01, 2), rep(1, 3))
#     }
#   }
#   if (is.null(extra.args$lower)) {
#     if (errors == "neither") {
#       extra.args$lower <- c(rep(-Inf, 2), 1e-3)
#     } else if (errors %in% c("processing", "measurement")) {
#       extra.args$lower <- c(rep(-Inf, 2), rep(1e-3, 2))
#     } else if (errors == "both") {
#       extra.args$lower <- c(rep(-Inf, 2), rep(1e-3, 3))
#     }
#   }
#   if (is.null(extra.args$control$rel.tol)) {
#     extra.args$control$rel.tol <- 1e-6
#   }
#   if (is.null(extra.args$control$eval.max)) {
#     extra.args$control$eval.max <- 1000
#   }
#   if (is.null(extra.args$control$iter.max)) {
#     extra.args$control$iter.max <- 750
#   }
#
#   # Obtain ML estimates
#   ml.max <- do.call(nlminb, c(list(objective = llf), extra.args))
#
#   # Create list to return
#   theta.hat <- ml.max$par
#   names(theta.hat) <- theta.labels
#   ret.list <- list(theta.hat = theta.hat)
#
#   # If requested, add variance-covariance matrix to ret.list
#   if (estimate_var) {
#     hessian.mat <- hessian(f = llf, estimating.hessian = TRUE,
#                                    x0 = theta.hat)
#     theta.variance <- try(solve(hessian.mat), silent = TRUE)
#     if (class(theta.variance) == "try-error") {
#       print(hessian.mat)
#       message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
#       ret.list$theta.var <- NULL
#     } else {
#       colnames(theta.variance) <- rownames(theta.variance) <- theta.labels
#       ret.list$theta.var <- theta.variance
#     }
#   }
#
#   # Add nlminb object and AIC to ret.list
#   ret.list$nlminb.object <- ml.max
#   ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)
#
#   # Return ret.list
#   return(ret.list)
#
# }

# # Also archived later on 5/5/2018
# cond_logreg <- function(g,
#                         xtilde1,
#                         xtilde0,
#                         c1 = NULL,
#                         c0 = NULL,
#                         errors = "both",
#                         approx_integral = TRUE,
#                         integrate_tol = 1e-8,
#                         estimate_var = TRUE,
#                         ...) {
#
#   # Get number of pools
#   n <- length(g)
#
#   # Figure out number of replicates for each pool
#   k1 <- sapply(xtilde1, length)
#   k0 <- sapply(xtilde0, length)
#
#   # Get name of xtilde
#   x.varname <- deparse(substitute(xtilde1))
#   if (length(grep("$", x.varname, fixed = TRUE)) > 0) {
#     x.varname <- substr(x.varname,
#                         start = which(unlist(strsplit(x.varname, "")) == "$") + 1,
#                         stop = nchar(x.varname))
#   }
#
#   # Get information about covariates C
#   if (is.null(c1)) {
#     some.cs <- FALSE
#     n.cvars <- 0
#   } else {
#     if (class(c1) != "matrix") {
#       c1 <- as.matrix(c)
#       c0 <- as.matrix(c)
#     }
#     n.cvars <- ncol(c1)
#     some.cs <- TRUE
#     c.varnames <- colnames(c1)
#     if (is.null(c.varnames)) {
#       c.varnames <- paste("c", 1: n.cvars, sep = "")
#     }
#   }
#
#   # Get number of betas and alphas
#   n.betas <- n.alphas <- 1 + n.cvars
#
#   # Create indicator vector I(g > 1)
#   Ig <- ifelse(g > 1, 1, 0)
#
#   # Separate out pools with precisely measured X
#   if (errors == "neither") {
#     which.p <- 1: n
#   } else if (errors == "processing") {
#     which.p <- which(Ig == 0)
#   } else {
#     which.p <- NULL
#   }
#   n.p <- length(which.p)
#   some.p <- n.p > 0
#   if (some.p) {
#
#     g.p <- g[which.p]
#     Ig.p <- Ig[which.p]
#
#     x1.p <- unlist(xtilde1[which.p])
#     x0.p <- unlist(xtilde0[which.p])
#
#     c1.p <- c1[which.p, , drop = FALSE]
#     c0.p <- c0[which.p, , drop = FALSE]
#
#   }
#
#   # Separate out pools with imprecisely measured X
#   which.i <- setdiff(1: n, n.p)
#   n.i <- n - n.p
#   some.i <- n.i > 0
#   if (some.i) {
#
#     g.i <- g[which.i]
#     Ig.i <- Ig[which.i]
#
#     k1.i <- k1[which.i]
#     k0.i <- k0[which.i]
#
#     xtilde1.i <- xtilde1[which.i]
#     xtilde0.i <- xtilde0[which.i]
#
#     c1.i <- c1[which.i, , drop = FALSE]
#     c0.i <- c0[which.i, , drop = FALSE]
#
#   }
#
#   # Get indices for parameters being estimated and create labels
#   loc.betas <- 1: n.betas
#   beta.labels <- paste("beta", c(x.varname, c.varnames), sep = "_")
#
#   loc.alphas <- (n.betas + 1): (n.betas + n.alphas)
#   alpha.labels <- paste("alpha", c("0", c.varnames), sep = "_")
#
#   loc.sigsq_x.c <- n.betas + n.alphas + 1
#
#   if (errors == "neither") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c")
#   } else if (errors == "processing") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_p")
#   } else if (errors == "measurement") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_m")
#   } else if (errors == "both") {
#     theta.labels <- c(beta.labels, alpha.labels,
#                       "sigsq_x.c", "sigsq_p", "sigsq_m")
#   }
#
#   # Log-likelihood function for approximate ML
#   llf.approx <- function(g, Ig, k1, k0, xtilde1, xtilde0, c1, c0,
#                          f.sigsq_x.c, f.sigsq_p, f.sigsq_m,
#                          f.beta_x, f.beta_c, f.alpha_0, f.alpha_c) {
#
#     # Extract xtilde1 and xtilde0
#     xtilde1 <- unlist(xtilde1)
#     xtilde0 <- unlist(xtilde0)
#
#     # Constants
#     onek1 <- matrix(1, nrow = k1)
#     onek0 <- matrix(1, nrow = k0)
#     jk1 <- matrix(1, nrow = k1, ncol = k1)
#     jk0 <- matrix(1, nrow = k0, ncol = k0)
#     ik1 <- diag(k1)
#     ik0 <- diag(k0)
#
#     # E(X|Xtilde,C) and V(X|Xtilde,C)
#     V1 <- jk1 * (g * f.sigsq_x.c + g^2 * f.sigsq_p * Ig) +
#       g^2 * f.sigsq_m * ik1
#     V0 <- jk0 * (g * f.sigsq_x.c + g^2 * f.sigsq_p * Ig) +
#       g^2 * f.sigsq_m * ik0
#     V1inv <- solve(V1)
#     V0inv <- solve(V0)
#
#     Mu_1 <- g * f.alpha_0 + sum(c1 * f.alpha_c) +
#       g * f.sigsq_x * t(onek1) %*% V1inv %*%
#       (xtilde1 - onek1 * (g * f.alpha_0 + sum(c1 * f.alpha_c)))
#     Mu_2 <- g * f.alpha_0 + sum(c0 * f.alpha_c) +
#       g * f.sigsq_x * t(onek0) %*% V0inv %*%
#       (xtilde0 - onek0 * (g * f.alpha_0 + sum(c0 * f.alpha_c)))
#
#     Sigma_11 <- g * f.sigsq_x - g^2 * f.sigsq_x^2 * t(onek1) %*%
#       V1inv %*% onek1
#     Sigma_22 <- g * f.sigsq_x - g^2 * f.sigsq_x^2 * t(onek0) %*%
#       V0inv %*% onek0
#
#     # Approximation
#     onemat <- matrix(c(1, -1), nrow = 1)
#     t <- (f.beta_x * (Mu_1 - Mu_2) + sum(f.beta_c * (c1 - c0))) /
#       sqrt(1 + f.beta_x^2 * (Sigma_11 + Sigma_22) / 1.7^2)
#
#     p <- exp(t) / (1 + exp(t))
#     part1 <- log(p)
#
#     # Part outside integral
#     part2 <-
#       sum(dmvnorm(x = xtilde1, log = TRUE, mean = Mu_1, sigma = V1)) +
#       sum(dmvnorm(x = xtilde0, log = TRUE, mean = Mu_2, sigma = V0))
#
#     return(part1 + part2)
#
#   }
#
#   # Likelihood function for full ML
#   lf.full <- function(g, Ig, k1, k0, xtilde1, xtilde0, c1, c0,
#                       f.sigsq_x.c, f.sigsq_p, f.sigsq_m,
#                       f.beta_x, f.beta_c, f.alpha_0, f.alpha_c,
#                       x12) {
#
#     x12 <- matrix(x12, nrow = 2)
#     dens <- apply(x12, 2, function(z) {
#
#       # Transformation
#       x1 <- z[1]
#       x2 <- z[2]
#       s1 <- x1 / (1 - x1^2)
#       s2 <- x2 / (1 - x2^2)
#
#       # Extract xtilde1 and xtilde0
#       xtilde1 <- unlist(xtilde1)
#       xtilde0 <- unlist(xtilde0)
#
#       # Constants
#       onek1 <- matrix(1, nrow = k1)
#       onek0 <- matrix(1, nrow = k0)
#       zerok1 <- matrix(0, nrow = k1)
#       zerok0 <- matrix(0, nrow = k0)
#       jk1 <- matrix(1, nrow = k1, ncol = k1)
#       jk0 <- matrix(1, nrow = k0, ncol = k0)
#       ik1 <- diag(k1)
#       ik0 <- diag(k0)
#
#       # E(X1*,X2*,Xtilde1*,Xtilde2*) and V()
#       mu_x.xtildec_1 <- g * f.alpha_0 + sum(f.alpha_c * c1)
#       mu_x.xtildec_2 <- g * f.alpha_0 + sum(f.alpha_c * c0)
#       Mu <- matrix(c(mu_x.xtildec_1, mu_x.xtildec_2,
#                      rep(mu_x.xtildec_1, k1), rep(mu_x.xtildec_2, k0)),
#                    ncol = 1)
#
#       V1 <- jk1 * (g * f.sigsq_x.c + g^2 * f.sigsq_p * Ig) +
#         g^2 * f.sigsq_m * ik1
#       V0 <- jk0 * (g * f.sigsq_x.c + g^2 * f.sigsq_p * Ig) +
#         g^2 * f.sigsq_m * ik0
#
#       Sigma <- cbind(
#         matrix(c(g * f.sigsq_x, 0, onek1 * g * f.sigsq_x, zerok0,
#                  0, g * f.sigsq_x, zerok1, onek0 * g * f.sigsq_x),
#                ncol = 2),
#         rbind(c(g * f.sigsq_x * onek1, zerok0),
#               c(zerok1, g * f.sigsq_x * onek0),
#               cbind(V1, zerok1 %*% t(zerok0)),
#               cbind(zerok0 %*% t(zerok1), V0))
#       )
#
#       # f(Y1,Y2,X1*,X2*,Xtilde1*,Xtilde2*)
#       (1 + exp(-f.beta_x * (s1 - s2)))^(-1) *
#         dmvnorm(x = c(s1, s2, xtilde1, xtilde2),
#                 mean = Mu,
#                 sigma = Sigma)
#
#     })
#
#     # Back-transformation
#     out <- matrix(
#       dens * (1 + x1^2) / (1 - x1^2)^2 *
#         (1 + x0^2) / (1 - x0^2)^2,
#       ncol = ncol(x12)
#     )
#
#     return(out)
#
#   }
#
#   # Log-likelihood function
#   llf <- function(f.theta, estimating.hessian = FALSE) {
#
#     # Extract parameters
#     f.betas <- matrix(f.theta[loc.betas], ncol = 1)
#     f.beta_x <- f.betas[1]
#     f.beta_c <- matrix(f.betas[-1], ncol = 1)
#
#     f.alphas <- matrix(f.theta[loc.alphas], ncol = 1)
#     f.alpha_0 <- f.alphas[1]
#     f.alpha_c <- matrix(f.alphas[-1], ncol = 1)
#
#     f.sigsq_x.c <- f.theta[loc.sigsq_x.c]
#
#     if (errors == "neither") {
#       f.sigsq_p <- 0
#       f.sigsq_m <- 0
#     } else if (errors == "processing") {
#       f.sigsq_p <- f.theta[loc.sigsq_x.c + 1]
#       f.sigsq_m <- 0
#     } else if (errors == "measurement") {
#       f.sigsq_p <- 0
#       f.sigsq_m <- f.theta[loc.sigsq_x.c + 1]
#     } else if (errors == "both") {
#       f.sigsq_p <- f.theta[loc.sigsq_x.c + 1]
#       f.sigsq_m <- f.theta[loc.sigsq_x.c + 2]
#     }
#
#     if (some.p) {
#
#       # Likelihood for pools with precisely measured X:
#       # L = f(Y|X) f(X)
#       ll.p <- sum(-log(1 + exp(f.beta_x * (x2.p - x1.p))) +
#                     dnorm(x = c(x1.p, x2.p), log = TRUE,
#                           mean = g.p * f.mu_x,
#                           sd = sqrt(g.p * f.sigsq_x)))
#
#     } else {
#       ll.p <- 0
#     }
#
#     if (some.i) {
#
#       # Likelihood for pools with single Xtilde:
#       # L = f(Y,Xtilde|C) = [\int_X f(Y|X,C) f(X|Xtilde,C) dX] f(Xtilde|C)
#       #                    = \int_X f(Y|X,C) f(Xtilde|X) f(X|C) dX
#
#       if (approx_integral) {
#
#         ll.i <- mapply(
#           FUN = f.i,
#           g = g.i,
#           Ig = Ig.i,
#           k1 = k1.i,
#           k0 = k0.i,
#           xtilde1 = xtilde1.i,
#           xtilde0 = xtilde0.i,
#           c1 = c1.i,
#           c0 = c0.i,
#           f.sigsq_x.c = f.sigsq_x.c,
#           f.sigsq_p = f.sigsq_p,
#           f.sigsq_m = f.sigsq_m,
#           f.beta_x = f.beta_x,
#           f.beta_c = f.beta_c,
#           f.alpha_0 = f.alpha_0,
#           f.alpha_c = f.alpha_c
#         )
#
#       } else {
#
#         int.vals <- c()
#         for (ii in 1: length(xtilde1.i)) {
#
#           # Get values for ith participant
#           g = g.i[ii]
#           Ig = Ig.i[ii]
#           k1 = k1.i[ii]
#           k0 = k0.i[ii]
#           xtilde1 = unlist(xtilde1.i[ii])
#           xtilde0 = unlist(xtilde2.i[ii])
#           c1 = c1.i
#           c0 = c0.i
#
#           # Perform integration
#           int.ii <-
#             adaptIntegrate(f = lf.full,
#                            tol = 1e-3,
#                            lowerLimit = rep(-1, 2),
#                            upperLimit = rep(1, 2),
#                            vectorInterface = TRUE,
#                            g = g.i[ii],
#                            Ig = Ig.i[ii],
#                            k1 = k1.i[ii],
#                            k0 = k0.i[ii],
#                            xtilde1 = unlist(xtilde1.i),
#                            xtilde0 = unlist(xtilde0.i),
#                            c1 = c1.i[ii, , drop = FALSE],
#                            c0 = c0.i[ii, , drop = FALSE],
#                            f.sigsq_x.c = f.sigsq_x.c,
#                            f.sigsq_p = f.sigsq_p,
#                            f.sigsq_m = f.sigsq_m,
#                            f.beta_x = f.beta_x,
#                            f.beta_c = f.beta_c,
#                            f.alpha_0 = f.alpha_0,
#                            f.alpha_c = f.alpha_c)
#
#           int.vals[ii] <- int.ii$integral
#
#         }
#
#
#         # Full likelihood
#         mu_x1 <- g.i * f.mu_x
#         sigsq_x1 <- g.i * f.sigsq_x
#         sigsq_xtilde1 <- g.i * f.sigsq_x + g.i^2 * (f.sigsq_p * Ig.i + f.sigsq_m)
#
#         # Function for integrating out X
#         int.f_i1 <- function(x12_i,
#                              xtilde1_i, xtilde2_i,
#                              mu_x1_i,
#                              sigsq_x1_i, sigsq_xtilde1_i) {
#
#           # Transformation
#           x1_i <- x12_i[1]
#           x2_i <- x12_i[2]
#           s1_i <- x1_i / (1 - x1_i^2)
#           s2_i <- x2_i / (1 - x2_i^2)
#
#           # P(Y|X,C)
#           #p_y1.x <- (1 + exp(-f.beta_x * (s1_i - s2_i)))^(-1)
#
#           # E(X1*,X2*,Xtilde1*,Xtilde2*) and V()
#           Mu <- matrix(mu_x1_i, ncol = 1, nrow = 4)
#           Sigma <- matrix(c(sigsq_x1_i, 0, sigsq_x1_i, 0,
#                             0, sigsq_x1_i, 0, sigsq_x1_i,
#                             sigsq_x1_i, 0, sigsq_xtilde1_i, 0,
#                             0, sigsq_x1_i, 0, sigsq_xtilde1_i),
#                           ncol = 4)
#
#           # f(Y1,Y2,X1*,X2*,Xtilde1*,Xtilde2*)
#           f_stuff <-
#             (1 + exp(-f.beta_x * (s1_i - s2_i)))^(-1) *
#             dmvnorm(x = c(s1_i, s2_i, xtilde1_i, xtilde2_i),
#                     mean = Mu,
#                     sigma = Sigma)
#
#           # Back-transformation
#           out <- f_stuff *
#             (1 + x1_i^2) / (1 - x1_i^2)^2 *
#             (1 + x2_i^2) / (1 - x2_i^2)^2
#           return(out)
#
#         }
#
#         # Function for integrating out X
#         int.f_i1 <- function(x12_i,
#                              xtilde1_i, xtilde2_i,
#                              mu_x1_i,
#                              sigsq_x1_i, sigsq_xtilde1_i) {
#
#           x12_i <- matrix(x12_i, nrow = 2)
#           dens <- apply(x12_i, 2, function(z) {
#
#             # Transformation
#             x1_i <- z[1]
#             x2_i <- z[2]
#             s1_i <- x1_i / (1 - x1_i^2)
#             s2_i <- x2_i / (1 - x2_i^2)
#
#             # E(X1*,X2*,Xtilde1*,Xtilde2*) and V()
#             Mu <- matrix(mu_x1_i, ncol = 1, nrow = 4)
#             Sigma <- matrix(c(sigsq_x1_i, 0, sigsq_x1_i, 0,
#                               0, sigsq_x1_i, 0, sigsq_x1_i,
#                               sigsq_x1_i, 0, sigsq_xtilde1_i, 0,
#                               0, sigsq_x1_i, 0, sigsq_xtilde1_i),
#                             ncol = 4)
#
#             # f(Y1,Y2,X1*,X2*,Xtilde1*,Xtilde2*)
#             (1 + exp(-f.beta_x * (s1_i - s2_i)))^(-1) *
#               dmvnorm(x = c(s1_i, s2_i, xtilde1_i, xtilde2_i),
#                       mean = Mu,
#                       sigma = Sigma)
#
#           })
#
#           # Back-transformation
#           out <- matrix(
#             dens * (1 + x1_i^2) / (1 - x1_i^2)^2 *
#               (1 + x2_i^2) / (1 - x2_i^2)^2,
#             ncol = ncol(x12_i)
#           )
#
#           return(out)
#
#         }
#
#         int.vals <- c()
#         for (ii in 1: length(xtilde1.i)) {
#
#           # Get values for ith participant
#           xtilde1_i <- xtilde1.i[ii]
#           xtilde2_i <- xtilde2.i[ii]
#           mu_x1_i <- mu_x1[ii]
#           sigsq_x1_i <- sigsq_x1[ii]
#           sigsq_xtilde1_i <- sigsq_xtilde1[ii]
#
#           # Try integrating out X_i with default settings
#           int.ii <-
#             adaptIntegrate(f = int.f_i1,
#                            tol = 1e-3,
#                            lowerLimit = rep(-1, 2),
#                            upperLimit = rep(1, 2),
#                            vectorInterface = TRUE,
#                            xtilde1_i = xtilde1_i,
#                            xtilde2_i = xtilde2_i,
#                            mu_x1_i = mu_x1_i,
#                            sigsq_x1_i = sigsq_x1_i,
#                            sigsq_xtilde1_i = sigsq_xtilde1_i)
#
#           int.vals[ii] <- int.ii$integral
#
#         }
#
#
#         ll.i <- sum(log(int.vals))
#
#       }
#
#     } else {
#
#       ll.i <- 0
#
#     }
#
#     # Return negative log-likelihood
#     ll <- ll.p + ll.i
#     return(-ll)
#
#   }
#
#   # Create list of extra arguments, and assign default starting values and
#   # lower values if not specified by user
#   extra.args <- list(...)
#   if (is.null(extra.args$start)) {
#     if (errors == "neither") {
#       extra.args$start <- c(rep(0.01, 2), 1)
#     } else if (errors %in% c("processing", "measurement")) {
#       extra.args$start <- c(rep(0.01, 2), rep(1, 2))
#     } else if (errors == "both") {
#       extra.args$start <- c(rep(0.01, 2), rep(1, 3))
#     }
#   }
#   if (is.null(extra.args$lower)) {
#     if (errors == "neither") {
#       extra.args$lower <- c(rep(-Inf, 2), 1e-3)
#     } else if (errors %in% c("processing", "measurement")) {
#       extra.args$lower <- c(rep(-Inf, 2), rep(1e-3, 2))
#     } else if (errors == "both") {
#       extra.args$lower <- c(rep(-Inf, 2), rep(1e-3, 3))
#     }
#   }
#   if (is.null(extra.args$control$rel.tol)) {
#     extra.args$control$rel.tol <- 1e-6
#   }
#   if (is.null(extra.args$control$eval.max)) {
#     extra.args$control$eval.max <- 1000
#   }
#   if (is.null(extra.args$control$iter.max)) {
#     extra.args$control$iter.max <- 750
#   }
#
#   # Obtain ML estimates
#   ml.max <- do.call(nlminb, c(list(objective = ll.f), extra.args))
#
#   # Create list to return
#   theta.hat <- ml.max$par
#   names(theta.hat) <- theta.labels
#   ret.list <- list(theta.hat = theta.hat)
#
#   # If requested, add variance-covariance matrix to ret.list
#   if (estimate_var) {
#     hessian.mat <- hessian(f = ll.f, estimating.hessian = TRUE,
#                                    x0 = theta.hat)
#     theta.variance <- try(solve(hessian.mat), silent = TRUE)
#     if (class(theta.variance) == "try-error") {
#       print(hessian.mat)
#       message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
#       ret.list$theta.var <- NULL
#     } else {
#       colnames(theta.variance) <- rownames(theta.variance) <- theta.labels
#       ret.list$theta.var <- theta.variance
#     }
#   }
#
#   # Add nlminb object and AIC to ret.list
#   ret.list$nlminb.object <- ml.max
#   ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)
#
#   # Return ret.list
#   return(ret.list)
#
# }


# Archived 5/5/2018
# cond_logreg <- function(g,
#                         xtilde1,
#                         xtilde2,
#                         errors = "both",
#                         approx_integral = TRUE,
#                         integrate_tol = 1e-8,
#                         estimate_var = TRUE,
#                              ...) {
#
#   # Create indicator vector I(g > 1)
#   Ig <- ifelse(g > 1, 1, 0)
#
#   # Number of pools
#   n <- length(g)
#
#   # Create y1 and y2 vectors
#   y1 <- rep(1, n)
#   y2 <- rep(0, n)
#
#   # Separate out pools with precisely measured X
#   if (errors == "neither") {
#     which.p <- 1: n
#   } else if (errors == "processing") {
#     which.p <- which(Ig == 0)
#   } else {
#     which.p <- NULL
#   }
#   n.p <- length(which.p)
#   some.p <- n.p > 0
#   if (some.p) {
#
#     g.p <- g[which.p]
#
#     y1.p <- y1[which.p]
#     y2.p <- y2[which.p]
#
#     x1.p <- xtilde1[which.p]
#     x2.p <- xtilde2[which.p]
#
#     Ig.p <- Ig[which.p]
#
#   }
#
#   # Separate out pools with single Xtilde
#   if (errors == "neither") {
#     which.i <- NULL
#   } else if (errors == "processing") {
#     which.i <- which(Ig == 1)
#   } else if (errors %in% c("measurement", "both")) {
#     which.i <- 1: n
#   }
#   n.i <- length(which.i)
#   some.i <- n.i > 0
#   if (some.i) {
#
#     g.i <- g[which.i]
#
#     y1.i <- y1[which.i]
#     y2.i <- y2[which.i]
#
#     xtilde1.i <- xtilde1[which.i]
#     xtilde2.i <- xtilde2[which.i]
#
#     Ig.i <- Ig[which.i]
#
#   }
#
#   # Create labels
#   beta.labels <- c("beta_x")
#   alpha.labels <- c("mu_x")
#   if (errors == "neither") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x")
#   } else if (errors == "processing") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x", "sigsq_p")
#   } else if (errors == "measurement") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x", "sigsq_m")
#   } else if (errors == "both") {
#     theta.labels <- c(beta.labels, alpha.labels, "sigsq_x", "sigsq_p", "sigsq_m")
#   }
#
#   # Log-likelihood function
#   ll.f <- function(f.theta, estimating.hessian = FALSE) {
#
#     # Extract parameters
#     f.beta_x <- f.theta[1]
#     f.mu_x <- f.theta[2]
#     f.sigsq_x <- f.theta[3]
#
#     if (errors == "neither") {
#       f.sigsq_p <- 0
#       f.sigsq_m <- 0
#     } else if (errors == "processing") {
#       f.sigsq_p <- f.theta[4]
#       f.sigsq_m <- 0
#     } else if (errors == "measurement") {
#       f.sigsq_p <- 0
#       f.sigsq_m <- f.theta[4]
#     } else if (errors == "both") {
#       f.sigsq_p <- f.theta[4]
#       f.sigsq_m <- f.theta[5]
#     }
#
#     if (some.p) {
#
#       # Likelihood for pools with precisely measured X:
#       # L = f(Y|X) f(X)
#       ll.p <- sum(-log(1 + exp(f.beta_x * (x2.p - x1.p))) +
#                     dnorm(x = c(x1.p, x2.p), log = TRUE,
#                           mean = g.p * f.mu_x,
#                           sd = sqrt(g.p * f.sigsq_x)))
#
#     } else {
#       ll.p <- 0
#     }
#
#     if (some.i) {
#
#       # Likelihood for pools with single Xtilde:
#       # L = f(Y,Xtilde|C) = [\int_X f(Y|X,C) f(X|Xtilde,C) dX] f(Xtilde|C)
#       #                    = \int_X f(Y|X,C) f(Xtilde|X) f(X|C) dX
#
#       if (approx_integral) {
#
#         # E(X_1*,X_2*|Xtilde_1*,Xtilde_2*) and V()
#         D <- g.i * f.sigsq_x + g.i^2 * f.sigsq_p * Ig.i + g.i^2 * f.sigsq_m
#         Mu_x.xtilde_1 <-
#           g.i * f.mu_x + g.i * f.sigsq_x / D * (xtilde1.i - g.i * f.mu_x)
#         Mu_x.xtilde_2 <-
#           g.i * f.mu_x + g.i * f.sigsq_x / D * (xtilde2.i - g.i * f.mu_x)
#
#         Sigma_x.xtilde_11 <-
#           Sigma_x.xtilde_22 <-
#           g.i^2 * f.sigsq_x * (f.sigsq_p * Ig.i + f.sigsq_m) / (D / g.i)
#
#         # Approximation of \int_x f(Y|X) f(X|Xtilde) dx
#         t <- (f.beta_x * (Mu_x.xtilde_1 - Mu_x.xtilde_2)) /
#           sqrt(1 + f.beta_x^2 * (Sigma_x.xtilde_11 + Sigma_x.xtilde_22) / 1.7^2)
#
#         # t <- (f.beta_x * (Mu_x.xtilde_1 + Mu_x.xtilde_2)) /
#         #   sqrt(1 + f.beta_x^2 * (Sigma_x.xtilde_11 + Sigma_x.xtilde_22) / 1.7^2)
#
#         p <- exp(t) / (1 + exp(t))
#         part1 <- log(p)
#
#         # log[f(Xtilde|C)]
#         part2 <-
#           dnorm(x = xtilde1.i, log = TRUE, mean = g.i * f.mu_x, sd = sqrt(D)) +
#           dnorm(x = xtilde2.i, log = TRUE, mean = g.i * f.mu_x, sd = sqrt(D))
#
#         # Log-likelihood
#         ll.i <- sum(part1 + part2)
#
#       } else {
#
#         # Full likelihood
#
#         mu_x1 <- g.i * f.mu_x
#         sigsq_x1 <- g.i * f.sigsq_x
#         sigsq_xtilde1 <- g.i * f.sigsq_x + g.i^2 * (f.sigsq_p * Ig.i + f.sigsq_m)
#
#         # Function for integrating out X
#         int.f_i1 <- function(x12_i,
#                              xtilde1_i, xtilde2_i,
#                              mu_x1_i,
#                              sigsq_x1_i, sigsq_xtilde1_i) {
#
#           # Transformation
#           x1_i <- x12_i[1]
#           x2_i <- x12_i[2]
#           s1_i <- x1_i / (1 - x1_i^2)
#           s2_i <- x2_i / (1 - x2_i^2)
#
#           # P(Y|X,C)
#           #p_y1.x <- (1 + exp(-f.beta_x * (s1_i - s2_i)))^(-1)
#
#           # E(X1*,X2*,Xtilde1*,Xtilde2*) and V()
#           Mu <- matrix(mu_x1_i, ncol = 1, nrow = 4)
#           Sigma <- matrix(c(sigsq_x1_i, 0, sigsq_x1_i, 0,
#                             0, sigsq_x1_i, 0, sigsq_x1_i,
#                             sigsq_x1_i, 0, sigsq_xtilde1_i, 0,
#                             0, sigsq_x1_i, 0, sigsq_xtilde1_i),
#                           ncol = 4)
#
#           # f(Y1,Y2,X1*,X2*,Xtilde1*,Xtilde2*)
#           f_stuff <-
#             (1 + exp(-f.beta_x * (s1_i - s2_i)))^(-1) *
#             dmvnorm(x = c(s1_i, s2_i, xtilde1_i, xtilde2_i),
#                     mean = Mu,
#                     sigma = Sigma)
#
#           # Back-transformation
#           out <- f_stuff *
#             (1 + x1_i^2) / (1 - x1_i^2)^2 *
#             (1 + x2_i^2) / (1 - x2_i^2)^2
#           return(out)
#
#         }
#
#         # int.vals <- c()
#         # for (ii in 1: length(xtilde1.i)) {
#         #
#         #   # Get values for ith participant
#         #   xtilde1_i <- xtilde1.i[ii]
#         #   xtilde2_i <- xtilde2.i[ii]
#         #   mu_x1_i <- mu_x1[ii]
#         #   sigsq_x1_i <- sigsq_x1[ii]
#         #   sigsq_xtilde1_i <- sigsq_xtilde1[ii]
#         #
#         #   # Try integrating out X_i with default settings
#         #   int.ii <-
#         #     adaptIntegrate(f = int.f_i1,
#         #                    tol = 1e-3,
#         #                    lowerLimit = rep(-1, 2),
#         #                    upperLimit = rep(1, 2),
#         #                    vectorInterface = FALSE,
#         #                    xtilde1_i = xtilde1_i,
#         #                    xtilde2_i = xtilde2_i,
#         #                    mu_x1_i = mu_x1_i,
#         #                    sigsq_x1_i = sigsq_x1_i,
#         #                    sigsq_xtilde1_i = sigsq_xtilde1_i)
#         #
#         #   int.vals[ii] <- int.ii$integral
#         #
#         # }
#
#         # Function for integrating out X
#         int.f_i1 <- function(x12_i,
#                              xtilde1_i, xtilde2_i,
#                              mu_x1_i,
#                              sigsq_x1_i, sigsq_xtilde1_i) {
#
#           x12_i <- matrix(x12_i, nrow = 2)
#           dens <- apply(x12_i, 2, function(z) {
#
#             # Transformation
#             x1_i <- z[1]
#             x2_i <- z[2]
#             s1_i <- x1_i / (1 - x1_i^2)
#             s2_i <- x2_i / (1 - x2_i^2)
#
#             # E(X1*,X2*,Xtilde1*,Xtilde2*) and V()
#             Mu <- matrix(mu_x1_i, ncol = 1, nrow = 4)
#             Sigma <- matrix(c(sigsq_x1_i, 0, sigsq_x1_i, 0,
#                               0, sigsq_x1_i, 0, sigsq_x1_i,
#                               sigsq_x1_i, 0, sigsq_xtilde1_i, 0,
#                               0, sigsq_x1_i, 0, sigsq_xtilde1_i),
#                             ncol = 4)
#
#             # f(Y1,Y2,X1*,X2*,Xtilde1*,Xtilde2*)
#             (1 + exp(-f.beta_x * (s1_i - s2_i)))^(-1) *
#               dmvnorm(x = c(s1_i, s2_i, xtilde1_i, xtilde2_i),
#                       mean = Mu,
#                       sigma = Sigma)
#
#           })
#
#           # Back-transformation
#           out <- matrix(
#             dens * (1 + x1_i^2) / (1 - x1_i^2)^2 *
#               (1 + x2_i^2) / (1 - x2_i^2)^2,
#             ncol = ncol(x12_i)
#           )
#
#           return(out)
#
#         }
#
#         int.vals <- c()
#         for (ii in 1: length(xtilde1.i)) {
#
#           # Get values for ith participant
#           xtilde1_i <- xtilde1.i[ii]
#           xtilde2_i <- xtilde2.i[ii]
#           mu_x1_i <- mu_x1[ii]
#           sigsq_x1_i <- sigsq_x1[ii]
#           sigsq_xtilde1_i <- sigsq_xtilde1[ii]
#
#           # Try integrating out X_i with default settings
#           int.ii <-
#             adaptIntegrate(f = int.f_i1,
#                            tol = 1e-3,
#                            lowerLimit = rep(-1, 2),
#                            upperLimit = rep(1, 2),
#                            vectorInterface = TRUE,
#                            xtilde1_i = xtilde1_i,
#                            xtilde2_i = xtilde2_i,
#                            mu_x1_i = mu_x1_i,
#                            sigsq_x1_i = sigsq_x1_i,
#                            sigsq_xtilde1_i = sigsq_xtilde1_i)
#
#           int.vals[ii] <- int.ii$integral
#
#         }
#
#
#         ll.i <- sum(log(int.vals))
#
#       }
#
#     } else {
#
#       ll.i <- 0
#
#     }
#
#     # Return negative log-likelihood
#     ll <- ll.p + ll.i
#     return(-ll)
#
#   }
#
#   # Create list of extra arguments, and assign default starting values and
#   # lower values if not specified by user
#   extra.args <- list(...)
#   if (is.null(extra.args$start)) {
#     if (errors == "neither") {
#       extra.args$start <- c(rep(0.01, 2), 1)
#     } else if (errors %in% c("processing", "measurement")) {
#       extra.args$start <- c(rep(0.01, 2), rep(1, 2))
#     } else if (errors == "both") {
#       extra.args$start <- c(rep(0.01, 2), rep(1, 3))
#     }
#   }
#   if (is.null(extra.args$lower)) {
#     if (errors == "neither") {
#       extra.args$lower <- c(rep(-Inf, 2), 1e-3)
#     } else if (errors %in% c("processing", "measurement")) {
#       extra.args$lower <- c(rep(-Inf, 2), rep(1e-3, 2))
#     } else if (errors == "both") {
#       extra.args$lower <- c(rep(-Inf, 2), rep(1e-3, 3))
#     }
#   }
#   if (is.null(extra.args$control$rel.tol)) {
#     extra.args$control$rel.tol <- 1e-6
#   }
#   if (is.null(extra.args$control$eval.max)) {
#     extra.args$control$eval.max <- 1000
#   }
#   if (is.null(extra.args$control$iter.max)) {
#     extra.args$control$iter.max <- 750
#   }
#
#   # Obtain ML estimates
#   ml.max <- do.call(nlminb, c(list(objective = ll.f), extra.args))
#
#   # Create list to return
#   theta.hat <- ml.max$par
#   names(theta.hat) <- theta.labels
#   ret.list <- list(theta.hat = theta.hat)
#
#   # If requested, add variance-covariance matrix to ret.list
#   if (estimate_var) {
#     hessian.mat <- hessian(f = ll.f, estimating.hessian = TRUE,
#                                    x0 = theta.hat)
#     theta.variance <- try(solve(hessian.mat), silent = TRUE)
#     if (class(theta.variance) == "try-error") {
#       print(hessian.mat)
#       message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
#       ret.list$theta.var <- NULL
#     } else {
#       colnames(theta.variance) <- rownames(theta.variance) <- theta.labels
#       ret.list$theta.var <- theta.variance
#     }
#   }
#
#   # Add nlminb object and AIC to ret.list
#   ret.list$nlminb.object <- ml.max
#   ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)
#
#   # Return ret.list
#   return(ret.list)
#
# }
