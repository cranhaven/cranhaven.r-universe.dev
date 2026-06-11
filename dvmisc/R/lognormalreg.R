#' Linear Regression of log(Y) vs. Covariates with Y Potentially Subject to 
#' Multiplicative Lognormal Errors
#' 
#' Uses maximum likelihood to fit
#' Y|\strong{X} ~ Lognormal(beta_0 + \strong{beta_x}^T \strong{X}, sigsq). Y 
#' can be precisely measured or subject to multiplicative mean-1 lognormal 
#' errors, in which case replicates can be incorporated by specifying \code{y} 
#' as a list).
#' 
#' 
#' @param y Numeric vector or list.
#' @param x Numeric vector or matrix. If \code{NULL}, model reduces to marginal
#' lognormal model Y ~ Lognormal(beta_0, sigsq).
#' @param merror Logical value for whether to model multiplicative lognormal 
#' measurement errors in Y.
#' @param estimate_var Logical value for whether to return Hessian-based
#' variance-covariance matrix.
#' @param fix_posdef Logical value for whether to repeatedly reduce
#' \code{integrate_tol_hessian} by factor of 5 and re-estimate Hessian to try
#' to avoid non-positive definite variance-covariance matrix.
#' @param ... Additional arguments to pass to \code{\link[stats]{nlminb}}.
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
#' @export
# betas <- c(0.5, 0.25)
# sigsq <- 0.5
# sigsq_m <- 0.5
# 
# n <- 300000
# k <- rep(1, n)
# k[1: 500] <- 2
# x <- rnorm(n)
# y <- rlnorm(n, meanlog = betas[1] + betas[2] * x, sdlog = sqrt(sigsq))
# ytilde <- list()
# for (ii in 1: n) {
#   ytilde[[ii]] <- y[ii] * rlnorm(k[ii],
#                                  meanlog = -sigsq_m / 2,
#                                  sdlog = sqrt(sigsq_m))
# }
# truth <- lognormalreg(y = y,
#                       x = x,
#                       merror = FALSE,
#                       estimate_var = FALSE,
#                       control = list(trace = 1))
# 
# naive <- lognormalreg(y = sapply(ytilde, function(x) x[1]),
#                       x = x,
#                       merror = FALSE,
#                       estimate_var = FALSE,
#                       control = list(trace = 1))
# 
# corrected.noreps <- lognormalreg(y = sapply(ytilde, function(x) x[1]),
#                                  x = x,
#                                  merror = TRUE,
#                                  control = list(trace = 1))
# 
# corrected.reps <- lognormalreg(y = ytilde,
#                                x = x,
#                                merror = TRUE,
#                                control = list(trace = 1))
lognormalreg <- function(y, 
                         x = NULL, 
                         merror = FALSE, 
                         estimate_var = TRUE, 
                         fix_posdef = FALSE, 
                         ...) {
  
  # Get information about covariates X
  if (is.null(x)) {
    x.varnames <- NULL
    n.xvars <- 0
    some.xs <- FALSE
  } else {
    x.varname <- deparse(substitute(x))
    if (class(x) != "matrix") {
      x <- as.matrix(x)
    }
    n.xvars <- ncol(x)
    some.xs <- TRUE
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
        x.varnames <- paste("c", 1: n.xvars, sep = "")
      }
    }
  }
  
  # Get number of betas
  n.betas <- 1 + n.xvars
  
  # Construct (1, X) design matrix
  n <- length(y)
  onex <- cbind(rep(1, n), x)
  
  # Separate observations into single Y and replicate Y's
  if (! merror) {
    
    some.s <- TRUE
    y.s <- y
    onex.s <- onex
    
    some.r <- FALSE
    
  } else {
    
    if (class(y) == "list") {
      k <- sapply(y, length)
    } else {
      k <- rep(1, n)
    }
    
    which.s <- which(k == 1)
    n.s <- length(which.s)
    some.s <- n.s > 0
    if (some.s) {
      y.s <- unlist(y[which.s])
      onex.s <- onex[which.s, , drop = FALSE]
    }
    
    which.r <- which(k > 1)
    n.r <- length(which.r)
    some.r <- n.r > 0
    if (some.r) {
      y.r <- y[which.r]
      k.r <- k[which.r]
      onex.r <- onex[which.r, , drop = FALSE]
    }
    
  }
  
  # Get indices for parameters being estimated and create labels
  loc.betas <- 1: n.betas
  beta.labels <- paste("beta", c("0", x.varnames), sep = "_")
  
  loc.sigsq <- n.betas + 1
  
  if (merror) {
    theta.labels <- c(beta.labels, "sigsq", "sigsq_m")
  } else {
    theta.labels <- c(beta.labels, "sigsq")
  }
  
  # Log-likelihood function
  llf <- function(f.theta, estimating.hessian = FALSE) {
    
    # Extract parameters
    f.betas <- matrix(f.theta[loc.betas], ncol = 1)
    f.beta_0 <- f.betas[1]
    f.beta_x <- matrix(f.betas[-c(1: 2)], ncol = 1)

    f.sigsq <- f.theta[loc.sigsq]
    
    if (merror) {
      f.sigsq_m <- f.theta[loc.sigsq + 1]
    } else {
      f.sigsq_m <- 0
    }
    
    if (some.s) {
      
      # Log-likelihood for single Y's
      ll.s <- sum(dlnorm(y.s, log = TRUE, 
                         meanlog = onex.s %*% f.betas - 1/2 * f.sigsq_m, 
                         sdlog = sqrt(f.sigsq + f.sigsq_m)))
      
    } else {
      ll.s <- 0
    }
    
    if (some.r) {
      
      # Log-likelihood for replicate Y's
      means <- onex.r %*% f.betas - f.sigsq_m / 2
      ll.r <- 0
      for (ii in 1: n.r) {
        y.ii <- unlist(y.r[ii])
        ll.r <- ll.r - log(prod(y.ii)) + 
          dmvnorm(log(y.ii), log = TRUE, 
                  mean = rep(means[ii], k.r[ii]),  
                  sigma = f.sigsq + f.sigsq_m * diag(k.r[ii]))
      }
      
    } else {
      ll.r <- 0
    }
    
    # Return negative log-likelihood
    ll <- ll.s + ll.r
    return(-ll)
    
  }
  
  # Create list of extra arguments, and assign default starting values and lower
  # values if not specified by user
  extra.args <- list(...)
  if (is.null(extra.args$start)) {
    if (merror) {
      extra.args$start <- c(rep(0.01, n.betas), 1, 1)
    } else {
      extra.args$start <- c(rep(0.01, n.betas), 1)
    }
  }
  if (is.null(extra.args$lower)) {
    if (merror) {
      extra.args$lower <- c(rep(-Inf, n.betas), 1e-3, 1e-3)
    } else {
      extra.args$lower <- c(rep(-Inf, n.betas), 1e-3)
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
  ml.max <- do.call(nlminb, c(list(objective = llf), extra.args))
  
  # Create list to return
  theta.hat <- ml.max$par
  names(theta.hat) <- theta.labels
  ret.list <- list(theta.hat = theta.hat)
  
  # If requested, add variance-covariance matrix to ret.list
  if (estimate_var) {
    
    # Estimate Hessian
    hessian.mat <- pracma::hessian(f = llf, estimating.hessian = TRUE,
                                   x0 = theta.hat)
    theta.variance <- try(solve(hessian.mat), silent = TRUE)
    if (class(theta.variance) == "try-error" ||
        ! all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {
      
      # Repeatedly divide integrate_tol_hessian by 5 and re-try
      while (integrate_tol_hessian > 1e-15 & fix_posdef) {
        integrate_tol_hessian <- integrate_tol_hessian / 5
        message(paste("Trying integrate_tol_hessian =", integrate_tol_hessian))
        hessian.mat <- pracma::hessian(f = llf, estimating.hessian = TRUE,
                                       x0 = theta.hat)
        theta.variance <- try(solve(hessian.mat), silent = TRUE)
        if (class(theta.variance) != "try-error" &&
            all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {
          break
        }
        
      }
    }
    
    if (class(theta.variance) == "try-error" ||
        ! all(eigen(x = theta.variance, only.values = TRUE)$values > 0)) {
      
      print(hessian.mat)
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
