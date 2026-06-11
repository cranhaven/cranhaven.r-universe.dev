#' Constant-Scale Gamma Model for Y vs. Covariates with Y Potentially Subject 
#' to Multiplicative Lognormal Errors
#' 
#' Uses maximum likelihood to fit
#' Y|\strong{X} ~ Gamma(exp(beta_0 + \strong{beta_x}^T \strong{X}), b), with the
#' shape-scale (as opposed to shape-rate) parameterization described in
#' \code{\link[stats]{GammaDist}}. Y can be precisely measured or subject to 
#' multiplicative mean-1 lognormal errors, in which case replicates can be 
#' incorporated by specifying \code{y} as a list.
#' 
#' @param y Numeric vector.
#' @param x Numeric vector or matrix. If \code{NULL}, model reduces to marginal
#' Gamma model Y ~ Gamma(exp(beta_0), b).
#' @param merror Logical value for whether to model multiplicative lognormal 
#' measurement errors in Y.
#' @param integrate_tol Numeric value specifying the \code{tol} input to
#' \code{\link[cubature]{hcubature}}. Only used if \code{merror = TRUE}.
#' @param integrate_tol_hessian Same as \code{integrate_tol}, but for use when
#' estimating the Hessian matrix only. Sometimes more precise integration
#' (i.e. smaller tolerance) than used for maximizing the likelihood helps
#' prevent cases where the inverse Hessian is not positive definite.
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
# b <- 0.5
# sigsq_m <- 1
# 
# n <- 2000
# k <- rep(1, n)
# k[1: 20] <- 2
# x <- rnorm(n)
# y <- rlnorm(n, meanlog = betas[1] + betas[2] * x, sdlog = 0.3)
# var(y)
# #y <- rgamma(n, shape = exp(betas[1] + betas[2] * x), scale = b)
# ytilde <- list()
# for (ii in 1: n) {
#   ytilde[[ii]] <- y[ii] * rlnorm(k[ii],
#                                  meanlog = -sigsq_m / 2,
#                                  sdlog = sqrt(sigsq_m))
# }
# truth <- gamma_constantscale(y = y,
#                            x = x,
#                            merror = FALSE,
#                            estimate_var = FALSE,
#                            control = list(trace = 1))
# 
# naive <- gamma_constantscale(y = sapply(ytilde, function(x) x[1]),
#                              x = x,
#                              merror = FALSE,
#                              estimate_var = FALSE,
#                              control = list(trace = 1))
# 
# corrected.noreps <- gamma_constantscale(y = sapply(ytilde, function(x) x[1]),
#                                         x = x,
#                                         merror = TRUE,
#                                         integrate_tol = 1e-4,
#                                         estimate_var = FALSE,
#                                         control = list(trace = 1))
# 
# corrected.reps <- gamma_constantscale(y = ytilde, 
#                                       x = x, 
#                                       merror = TRUE, 
#                                       integrate_tol = 1e-4, 
#                                       estimate_var = FALSE, 
#                                       control = list(trace = 1))
gammareg <- function(y, 
                     x = NULL, 
                     merror = FALSE, 
                     integrate_tol = 1e-8, 
                     integrate_tol_hessian = integrate_tol, 
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
  
  # Separate observations into precise Y, single imprecise Y, and replicate 
  # imprecise Y's
  if (! merror) {
    some.p <- TRUE
    
    y.p <- y
    onex.p <- onex
    
    some.s <- FALSE
    some.r <- FALSE
    
  } else {
    
    some.p <- FALSE
    if (class(y) == "list") {
      k <- sapply(y, length)
    } else {
      k <- rep(1, n)
    }
    
    which.s <- which(k == 1)
    n.s <- length(which.s)
    some.s <- n.s > 0
    if (some.s) {
      ytilde.s <- unlist(y[which.s])
      onex.s <- onex[which.s, , drop = FALSE]
    }
    
    which.r <- which(k > 1)
    n.r <- length(which.r)
    some.r <- n.r > 0
    if (some.r) {
      ytilde.r <- y[which.r]
      onex.r <- onex[which.r, , drop = FALSE]
    }
    
  }
  
  # Get indices for parameters being estimated and create labels
  loc.betas <- 1: n.betas
  beta.labels <- paste("beta", c("0", x.varnames), sep = "_")
  
  loc.b <- n.betas + 1
  
  if (merror) {
    theta.labels <- c(beta.labels, "b", "sigsq_m")
  } else {
    theta.labels <- c(beta.labels, "b")
  }
  
  # Likelihood function for single imprecise Y
  lf.s <- function(ytilde, y, shape, scale, sigsq_m) {
    
    # f(Ytilde|X)
    y <- matrix(y, nrow = 1)
    f_ytildey.x <- apply(y, 2, function(z) {
      
      # Transformation
      s <- z / (1 - z)
      
      # Density
      dlnorm(ytilde, 
             meanlog = log(s) - sigsq_m / 2, 
             sdlog = sqrt(sigsq_m)) * 
        dgamma(s, shape = shape, scale = scale)
      
      # 1 / ytilde * dnorm(x = log(ytilde),
      #                    mean = log(s) - 1/2 * f.sigsq_m, 
      #                    sd = sqrt(f.sigsq_m))
      
        
      
    })
    
    # Back-transformation
    out <- matrix(f_ytildey.x / (1 - y)^2, ncol = ncol(y))
    
  }
  
  # Likelihood function for replicate Y
  lf.r <- function(ytilde, y, shape, scale, sigsq_m) {
    
    # f(Ytilde|X)
    y <- matrix(y, nrow = 1)
    f_ytildey.x <- apply(y, 2, function(z) {
      
      # Transformation
      s <- z / (1 - z)
      
      # Density
      prod(dlnorm(ytilde, 
                  meanlog = log(s) - 1/2 * sigsq_m, 
                  sdlog = sqrt(sigsq_m))) * 
        dgamma(s, shape = shape, scale = scale)
      
      # 1 / prod(ytilde) * dmvnorm(log(ytilde), 
      #                            mean = rep(log(s) - 1/2 * sigsq_m, 2), 
      #                            sigma = diag(sigsq_m, 2)) * 
      #   dgamma(s, shape = shape, scale = scale)
      
      
    })
    
    # Back-transformation
    out <- matrix(f_ytildey.x / (1 - y)^2, ncol = ncol(y))
    
  }
  
  # Log-likelihood function
  llf <- function(f.theta, estimating.hessian = FALSE) {
    
    # Extract parameters
    f.betas <- matrix(f.theta[loc.betas], ncol = 1)
    f.beta_0 <- f.betas[1]
    f.beta_x <- matrix(f.betas[-c(1: 2)], ncol = 1)

    f.b <- f.theta[loc.b]
    
    if (merror) {
      f.sigsq_m <- f.theta[loc.b + 1]
    } else {
      f.sigsq_m <- 0
    }
    
    if (some.p) {
      
      # Log-likelihood for precise Y's
      shapes <- exp(onex.p %*% f.betas)
      ll.p <- sum(dgamma(y.p, log = TRUE, 
                         shape = shapes, 
                         scale = f.b))
      
    } else {
      ll.p <- 0
    }
    
    # Set skip.rest flag to FALSE
    skip.rest <- FALSE
    
    if (some.s) {
      
      # Log-likelihood for single imprecise Y's
      shapes <- exp(onex.s %*% f.betas)
      
      # Get integration tolerance
      if (estimating.hessian) {
        int.tol <- integrate_tol_hessian
      } else {
        int.tol <- integrate_tol
      }
      
      int.vals <- c()
      for (ii in 1: n.s) {
        
        # Perform integration
        int.ii <- cubature::hcubature(f = lf.s,
                                      tol = integrate_tol,
                                      lowerLimit = 0,
                                      upperLimit = 1,
                                      vectorInterface = TRUE,
                                      ytilde = ytilde.s[ii], 
                                      shape = shapes[ii], 
                                      scale = f.b, 
                                      sigsq_m = f.sigsq_m)
        int.vals[ii] <- int.ii$integral
        if (int.ii$integral == 0) {
          print(paste("Integral is 0 for ii = ", ii, sep = ""))
          print(f.theta)
          skip.rest <- TRUE
          break
        }
        
      }
      
      ll.s <- sum(log(int.vals))
      
    } else {
      ll.s <- 0
    }
    
    if (some.r & ! skip.rest) {
      
      # Log-likelihood for replicate Y
      shapes <- exp(onex.r %*% f.betas)
      
      # Get integration tolerance
      if (estimating.hessian) {
        int.tol <- integrate_tol_hessian
      } else {
        int.tol <- integrate_tol
      }
      
      int.vals <- c()
      for (ii in 1: n.r) {
        
        # Perform integration
        int.ii <- cubature::hcubature(f = lf.r,
                                      tol = integrate_tol,
                                      lowerLimit = 0,
                                      upperLimit = 1,
                                      vectorInterface = TRUE,
                                      ytilde = unlist(ytilde.r[ii]), 
                                      shape = shapes[ii], 
                                      scale = f.b, 
                                      sigsq_m = f.sigsq_m)
        int.vals[ii] <- int.ii$integral
        if (int.ii$integral == 0) {
          print(paste("Integral is 0 for ii = ", ii, sep = ""))
          print(f.theta)
          break
        }
        
      }
      
      ll.r <- sum(log(int.vals))
      
    } else {
      ll.r <- 0
    }
    
    # Return negative log-likelihood
    ll <- ll.p + ll.s + ll.r
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
