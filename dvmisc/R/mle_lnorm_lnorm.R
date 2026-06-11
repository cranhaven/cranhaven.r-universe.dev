#' Maximum Likelihood Estimation for X[1], ..., X[n] ~ Lognormal(mu1, sigsq1) 
#' Lognormal(mu2, sigsq2)
#' 
#' Each observation is assumed to be the product of a Lognormal(mu1, sigsq1) and 
#' Lognormal(mu2, sigsq2) random variable, with mu2 and sigsq2 known. Performs 
#' maximization via \code{\link[stats]{nlminb}}. mu and sigsq correspond to 
#' meanlog and sdlog^2 in \code{\link[stats]{Lognormal}}.
#' 
#' @param x Numeric vector.
#' @param mu2 Numeric value specifying known mu2.
#' @param sigsq2 Numeric value specifying known sigsq2.
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
#' # Generate 1,000 values from Lognormal(0.5, 1) x Lognormal(0.75, 1.5) and 
#' # estimate parameters based on known mu and sigsq for one of them
#' set.seed(123)
#' x <- rlnorm(1000, 0.5, sqrt(1)) * rlnorm(1000, 0.75, sqrt(1.5))
#' mle_lnorm_lnorm(x, mu2 = 0.75, sigsq2 = 1.5)
#' 
#' 
#' @export
mle_lnorm_lnorm <- function(x, 
                            mu2 = NULL, sigsq2 = NULL, 
                            estimate_var = FALSE, ...) {
  
  # Return error if mu2 and sigsq2 are not both specified
  if (is.null(mu2) & is.null(sigsq2)) {
    stop("mu2 and sigsq2 have to be specified")
  }
  
  # Log-likelihood function
  extra.args <- list(...)
  
  if (is.null(mu2) & is.null(sigsq2)) {
    
    theta.labels <- c("mu1", "sigsq1", "mu2", "sigsq2")
    extra.args <- list_override(
      list1 = list(start = c(0, 1, 0, 1), lower = c(-Inf, 0, -Inf, 0), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dlnorm(x, 
                         meanlog = f.theta[1] + f.theta[2], 
                         sdlog = sqrt(f.theta[3] + f.theta[4]), log = TRUE)))
    }
    
  } else if (is.null(mu2)) {
    
    theta.labels <- c("mu1", "sigsq1", "mu2")
    extra.args <- list_override(
      list1 = list(start = c(0, 1, 0), lower = c(-Inf, 0, -Inf), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dlnorm(x, 
                         meanlog = f.theta[1] + f.theta[2], 
                         sdlog = sqrt(f.theta[3] + sigsq2), log = TRUE)))
    }
    
  } else if (is.null(sigsq2)) {
    
    theta.labels <- c("mu1", "sigsq1", "sigsq2")
    extra.args <- list_override(
      list1 = list(start = c(0, 1, 1), lower = c(-Inf, 0, 0), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dlnorm(x, 
                         meanlog = f.theta[1] + mu2, 
                         sdlog = sqrt(f.theta[2] + f.theta[3]), log = TRUE)))
    }
    
  } else {
    
    theta.labels <- c("mu1", "sigsq1")
    extra.args <- list_override(
      list1 = list(start = c(0, 1), lower = c(-Inf, 0), 
                   control = list(rel.tol = 1e-6, eval.max = 1000, iter.max = 750)), 
      list2 = extra.args
    )
    llf <- function(f.theta) {
      return(-sum(dlnorm(x, 
                         meanlog = f.theta[1] + mu2, 
                         sdlog = sqrt(f.theta[2] + sigsq2), log = TRUE)))
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
      ret.list$theta.var <- theta.variance
      
    }
    
  }
  
  # Add nlminb object and AIC to ret.list
  ret.list$nlminb.object <- ml.max
  ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)
  
  # Return ret.list
  return(ret.list)
  
}