#' Poolwise Logistic Regression
#'
#' Fit homogeneous-pools logistic regression model described by Weinberg &
#' Umbach (1999).
#'
#'
#' @param g Numeric vector with pool sizes, i.e. number of members in each pool.
#' @param y Numeric vector with poolwise \code{Y} values, coded 0 if all members
#' are controls and 1 if all members are cases.
#' @param x Numeric matrix with poolwise \strong{\code{X}} values, with one row
#' for each pool. Can be a vector if there is only 1 predictor.
#' @param method Character string specifying method to use for estimation.
#' Choices are "glm" for \code{\link[stats]{glm}} function and \code{"ml"} for
#' maximum likelihood.
#' @param prev Numeric value specifying disease prevalence, allowing
#' for valid estimation of the intercept with case-control sampling. Can specify
#' \code{samp_y1y0} instead if sampling rates are known.
#' @param samp_y1y0 Numeric vector of length 2 specifying sampling probabilities
#' for cases and controls, allowing for valid estimation of the intercept with
#' case-control sampling. Can specify \code{prev} instead if it's easier.
#' @param estimate_var Logical value for whether to return variance-covariance
#' matrix for parameter estimates.
#' @param start Numeric value specifying starting values for parameters. Only
#' used if \code{method = "ml"}.
#' @param lower Numeric value specifying lower bounds for parameters. Only used
#' if \code{method = "ml"}.
#' @param upper Numeric value specifying upper bounds for parameters. Only used
#' if \code{method = "ml"}.
#' @param nlminb_list List of arguments to pass to \code{\link[stats]{nlminb}}
#' for log-likelihood maximization.
#' @param hessian_list List of arguments to pass to
#' \code{\link[numDeriv]{hessian}} for approximating the Hessian matrix. Only
#' used if \code{method = "ml"} and \code{estimate_var = TRUE}.
#'
#'
#' @return
#' List containing:
#' \enumerate{
#' \item Numeric vector of parameter estimates.
#' \item Variance-covariance matrix (if \code{estimate_var = TRUE}).
#' \item Fitted \code{\link[stats]{glm}} object (if \code{method = "glm"}) or
#' returned \code{\link[stats]{nlminb}} object (if \code{method = "ml"}).
#' \item Akaike information criterion (AIC).
#' }
#'
#'
#' @references
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
#' # Load dataset containing (Y, Xtilde, C) values for pools of size 1, 2, and 3
#' data(pdat1)
#'
#' # Estimate log-OR for Xtilde and Y adjusted for C
#' fit <- p_logreg(g = pdat1$g, y = pdat1$allcases, x = pdat1[, c("xtilde", "c")])
#' fit$theta.hat
#'
#'
#' @export
p_logreg <- function(
  g,
  y,
  x,
  method = "glm",
  prev = NULL,
  samp_y1y0 = NULL,
  estimate_var = TRUE,
  start = 0.01,
  lower = -Inf,
  upper = Inf,
  nlminb_list = list(control = list(trace = 1, eval.max = 500, iter.max = 500)),
  hessian_list = list(method.args = list(r = 4))
) {

  # Check that inputs are valid
  if (! method %in% c("glm", "ml")) {
    stop("The input 'method' should be set to 'glm' for glm function or 'ml' for maximum likelihood.")
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

  # Get number of X variables (and assign names)
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

  # Create matrix of (g, X) values
  gx <- cbind(g, x)

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

  # Create labels for parameter estimates
  beta.labels <- paste("beta", c("0", x.varnames), sep = "_")

  # Use glm function or ML depending on method input
  if (method == "glm") {

    # Fit logistic regression
    glm.fit <- glm(y ~ gx - 1, family = "binomial", offset = qg)

    # Create list to return
    theta.hat <- glm.fit$coef
    names(theta.hat) <- beta.labels
    ret.list <- list(theta.hat = theta.hat)

    # If requested, add variance-covariance matrix to ret.list
    if (estimate_var) {
      glm.variance <- vcov(glm.fit)
      colnames(glm.variance) <- rownames(glm.variance) <- beta.labels
      ret.list$glm.var <- glm.variance
    }

    # Add fitted glm object and AIC to ret.list
    ret.list$glm.fit <- glm.fit
    ret.list$aic <- AIC(glm.fit)

  } else if (method == "ml") {

    # Log-likelihood function
    n.betas <- length(beta.labels)
    llf <- function(f.theta) {

      # Likelihood:
      # L_i = f(Y|X)

      # P(Y|X)
      eta <- gx %*% f.theta + qg
      p_y.x <- (1 + exp(-eta))^(-1)

      # Log-likelihood
      ll <- sum(dbinom(x = y, size = 1, prob = p_y.x, log = TRUE))
      return(-ll)

    }

    # Obtain ML estimates
    ml.max <- do.call(nlminb,
                      c(list(start = rep(start, n.betas),
                             objective = llf,
                             lower = lower,
                             upper = upper),
                        nlminb_list))

    # Output message if nlminb indicates non-convergence
    if (ml.max$convergence == 1) {
      message("Object returned by 'nlminb' function indicates non-convergence. You may want to try different starting values.")
    }

    # Create list to return
    theta.hat <- ml.max$par
    names(theta.hat) <- beta.labels
    ret.list <- list(theta.hat = theta.hat)

    # If requested, add variance-covariance matrix to ret.list
    if (estimate_var) {

      # Estimate Hessian
      hessian.mat <- do.call(numDeriv::hessian,
                             c(list(func = llf, x = theta.hat),
                               hessian_list))

      # Estimate variance-covariance matrix
      theta.variance <- solve(hessian.mat)
      colnames(theta.variance) <- rownames(theta.variance) <- beta.labels
      ret.list$theta.var <- theta.variance

    }

    # Add nlminb object and AIC to ret.list
    ret.list$nlminb.object <- ml.max
    ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)

  }

  # Return ret.list
  return(ret.list)

}
