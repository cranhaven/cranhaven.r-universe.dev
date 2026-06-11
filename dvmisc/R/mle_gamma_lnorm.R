#' Maximum Likelihood Estimation for X[1], ..., X[n] ~ Gamma(alpha, beta) Lognormal(mu, sigsq)
#' 
#' Each observation is assumed to be the product of a Gamma(alpha, beta) and 
#' Lognormal(mu, sigsq) random variable. Performs maximization via 
#' \code{\link[stats]{nlminb}}. alpha and beta correspond to the shape and scale 
#' (not shape and rate) parameters described in \code{\link[stats]{GammaDist}}, 
#' and mu and sigsq correspond to meanlog and sdlog^2 in 
#' \code{\link[stats]{Lognormal}}.
#' 
#' @param x Numeric vector.
#' @param gamma_mean1 Whether to use restriction that the Gamma variable is 
#' mean-1.
#' @param lnorm_mean1 Whether to use restriction that the lognormal variable is 
#' mean-1.
#' @param integrate_tol Numeric value specifying the \code{tol} input to
#' \code{\link[cubature]{hcubature}}.
#' @param estimate_var Logical value for whether to return Hessian-based
#' variance-covariance matrix.
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
#' @examples
#' # Generate 1,000 values from Gamma(0.5, 1) x Lognormal(-1.5/2, 1.5) and 
#' # estimate parameters
#' \dontrun{
#' set.seed(123)
#' x <- rgamma(1000, 0.5, 1) * rlnorm(1000, -1.5/2, sqrt(1.5))
#' mle_gamma_lnorm(x, control = list(trace = 1))
#' }
#' 
#' 
#' @export
mle_gamma_lnorm <- function(x, 
                            gamma_mean1 = FALSE, 
                            lnorm_mean1 = TRUE,  
                            integrate_tol = 1e-8, 
                            estimate_var = FALSE, ...) {
  
  # Check that at least one of gamma_mean1 and lnorm_mean1 is TRUE
  if (! gamma_mean1 & ! lnorm_mean1) {
    stop("For identifiability, either 'gamma_mean1' or 'lnorm_mean1' (or both) has to be TRUE.")
  }
  
  # Sample size
  n <- length(x)
  
  # Likelihood function
  lf <- function(x, g, alpha, beta, mu, sigsq) {
    
    # f(X)
    g <- matrix(g, nrow = 1)
    f_xg <- apply(g, 2, function(z) {
      
      # Transformation
      s <- z / (1 - z)
      
      # Density
      dlnorm(x, 
             meanlog = log(s) + mu, 
             sdlog = sqrt(sigsq)) * 
        dgamma(s, shape = alpha, scale = beta)
      
    })
    
    # Back-transformation
    out <- matrix(f_xg / (1 - g)^2, ncol = ncol(g))
    
  }
  
  # Log-likelihood function
  extra.args <- list(...)
  
  if (gamma_mean1 & ! lnorm_mean1) {
    
    theta.labels <- c("beta", "mu", "sigsq")
    extra.args <- list_override(
      list1 = list(start = c(1, 0, 1), lower = c(0, -Inf, 0), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    
  } else if (! gamma_mean1 & lnorm_mean1) {
    
    theta.labels <- c("alpha", "beta", "sigsq")
    extra.args <- list_override(
      list1 = list(start = c(1, 1, 1), lower = c(0, 0, 0), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    
  } else if (gamma_mean1 & lnorm_mean1) {
    
    theta.labels <- c("beta", "sigsq")
    extra.args <- list_override(
      list1 = list(start = c(1, 1), lower = c(0, 0), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    
  }
  
  llf <- function(f.theta) {
    
    # Extract parameters
    if (gamma_mean1 & ! lnorm_mean1) {
      f.beta <- f.theta[1]
      f.mu <- f.theta[2]
      f.sigsq <- f.theta[3]
      f.alpha <- 1 / f.beta
    } else if (! gamma_mean1 & lnorm_mean1) {
      f.alpha <- f.theta[1]
      f.beta <- f.theta[2]
      f.sigsq <- f.theta[3]
      f.mu <- -1/2 * f.sigsq
    } else if (gamma_mean1 & lnorm_mean1) {
      f.beta <- f.theta[1]
      f.sigsq <- f.theta[2]
      f.alpha <- 1 / f.beta
      f.mu <- -1/2 * f.sigsq
    }
    
    int.vals <- c()
    for (ii in 1: n) {
      
      # Perform integration
      int.ii <- cubature::hcubature(f = lf,
                                    tol = integrate_tol,
                                    lowerLimit = 0,
                                    upperLimit = 1,
                                    vectorInterface = TRUE,
                                    x = x[ii], 
                                    alpha = f.alpha, 
                                    beta = f.beta, 
                                    mu = f.mu, 
                                    sigsq = f.sigsq)
      int.vals[ii] <- int.ii$integral
      if (int.ii$integral == 0) {
        print(paste("Integral is 0 for ii = ", ii, sep = ""))
        print(f.theta)
        skip.rest <- TRUE
        break
      }
      
    }
    
    return(-sum(log(int.vals)))
    
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
    hessian.mat <- pracma::hessian(f = llf, x0 = theta.hat)
    theta.variance <- try(solve(hessian.mat), silent = TRUE)
    
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
