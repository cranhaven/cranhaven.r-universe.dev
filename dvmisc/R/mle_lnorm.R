#' Maximum Likelihood Estimation for X[1], ..., X[n] ~ Lognormal(mu, sigsq)
#' 
#' Performs maximization via \code{\link[stats]{nlminb}}. mu and sigsq 
#' correspond to meanlog and sdlog^2 in \code{\link[stats]{Lognormal}}.
#' 
#' @param x Numeric vector.
#' @param mu Numeric value specifying known mu.
#' @param sigsq Numeric value specifying known sigsq.
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
#' # Generate 1,000 values from Lognormal(0.5, 1) and estimate mu and sigsq
#' set.seed(123)
#' x <- rlnorm(1000, meanlog = 0.5, sdlog = sqrt(1))
#' mle_lnorm(x)
#' 
#' 
#' @export
mle_lnorm <- function(x, mu = NULL, sigsq = NULL, estimate_var = FALSE, ...) {
  
  # Return error if mu and sigsq are both specified
  if (! is.null(mu) & ! is.null(sigsq)) {
    stop("mu and sigsq cannot both be specified; there would be nothing to estimate!")
  }
  
  # Log-likelihood function
  extra.args <- list(...)
  if (is.null(mu) & is.null(sigsq)) {
    
    theta.labels <- c("mu", "sigsq")
    extra.args <- list_override(
      list1 = list(start = c(0, 1), lower = c(-Inf, 0), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dlnorm(x, meanlog = f.theta[1], sdlog = sqrt(f.theta[2]), log = TRUE)))
    }
    
  } else if (is.null(mu)) {
    
    theta.labels <- "mu"
    extra.args <- list_override(
      list1 = list(start = 0, lower = -Inf, 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dlnorm(x, meanlog = f.theta, sdlog = sqrt(sigsq), log = TRUE)))
    }
    
  } else if (is.null(sigsq)) {
    
    theta.labels <- "sigsq"
    extra.args <- list_override(
      list1 = list(start = 1, lower = 0, 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dlnorm(x, meanlog = mu, sdlog = sqrt(f.theta), log = TRUE)))
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