#' Maximum Likelihood Estimation for X[1], ..., X[n] ~ Gamma(alpha, beta)
#' 
#' Performs maximization via \code{\link[stats]{nlminb}}. alpha and beta 
#' correspond to the shape and scale (not shape and rate) parameters described 
#' in \code{\link[stats]{GammaDist}}.
#' 
#' @param x Numeric vector.
#' @param alpha Numeric value specifying known alpha.
#' @param beta Numeric value specifying known beta.
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
#' # Generate 1,000 values from Gamma(0.5, 1) and estimate alpha and beta
#' set.seed(123)
#' x <- rgamma(1000, shape = 0.5, scale = 1)
#' mle_gamma(x)
#' 
#' 
#' @export
mle_gamma <- function(x, alpha = NULL, beta = NULL, estimate_var = FALSE, ...) {
  
  # Return error if alpha and beta are both specified
  if (! is.null(alpha) & ! is.null(beta)) {
    stop("alpha and beta cannot both be specified; there would be nothing to estimate!")
  }
  
  # Log-likelihood function
  extra.args <- list(...)
  if (is.null(alpha) & is.null(beta)) {
    
    theta.labels <- c("alpha", "beta")
    extra.args <- list_override(
      list1 = list(start = c(1, 1), lower = c(0, 0), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dgamma(x, shape = f.theta[1], scale = f.theta[2], log = TRUE)))
    }
    
  } else if (is.null(alpha)) {
    
    theta.labels <- "alpha"
    extra.args <- list_override(
      list1 = list(start = 1, lower = 0, 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dgamma(x, shape = alpha, scale = f.theta, log = TRUE)))
    }
    
  } else if (is.null(beta)) {
    
    theta.labels <- "beta"
    extra.args <- list_override(
      list1 = list(start = 1, lower = 0, 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dgamma(x, shape = f.theta, scale = beta, log = TRUE)))
    }
    
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
      if (length(theta.labels) == 1) theta.variance <- as.numeric(theta.variance)
      ret.list$theta.var <- theta.variance
      
    }
    
  }
  
  # Add nlminb object and AIC to ret.list
  ret.list$nlminb.object <- ml.max
  ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)
  
  # Return ret.list
  return(ret.list)
  
}