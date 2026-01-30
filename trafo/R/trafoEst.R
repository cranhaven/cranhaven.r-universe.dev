#' Estimation of optimal transformation parameter - lm
#' 
#' @param x matrix of regressors
#' @param y vector of response variables
#' @param lambdarange range for the estimation parameter expr(lambda) - default c(-2, 2)
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),
#' (iii) Kurtosis optimization ("kurt"), (iv) Divergence minimization by 
#' Kolmogorov-Smirnov ("div.ks"), by Cramer-von-Mises ("div.cvm") or by 
#' Kullback-Leibler ("div.kl"). Defaults to "ml".
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
est_lm <- function(y, x , method, lambdarange, trafo, custom_func, 
                   custom_func_std, ...) {
  
  # Number of explanatory variables
  k <- ncol(x)
  

    # if (diff(range(lambdarange)) <= 1000) {
    #   tol <- 0.0001
    # } else if (diff(range(lambdarange)) > 1000 && diff(range(lambdarange)) <= 2000) {
    #   tol <- 0.001
    # } else if (diff(range(lambdarange)) > 2000 && diff(range(lambdarange)) <= 5000) {
    #   tol <- 0.01
    # } else if (diff(range(lambdarange)) > 5000 && diff(range(lambdarange)) <= 10000) {
    #   tol <- 0.1
    # } else if (diff(range(lambdarange)) > 10000) {
    #   tol <- 1
    # } 
  
    # else if (diff(range(lambdarange)) > 50000 && diff(range(lambdarange)) <= 100000) {
    #   tol <- 10
    # } else if (diff(range(lambdarange)) > 100000) {
    #   tol <- 100
    # }

  
  # Get the optimal lambda via optimization on lambdarange
  res <- suppressWarnings(optimize(f = estim_lm, y = y, x = x, method = method,
                                   trafo = trafo, interval = lambdarange, 
                                   custom_func = custom_func, 
                                   custom_func_std = custom_func_std,
                                   tol = 0.0001))
  
  if (is.infinite(res$objective)) {
    stop("For some lambda in the interval, the likelihood does not converge.
         Choose another lambdarange.")
  }
  
  # Optimal lambda and corresponding measure: likelihood, skewness or divergence
  lambdaoptim <-  res$minimum
  measoptim <- res$objective
  
  return(list(lambdaoptim = lambdaoptim, 
              measoptim = measoptim))
}  
  


#' Box Cox Estimation - lme
#' 
#' @param x matrix of regressors
#' @param y vector of response variables
#' @param lambdarange range for the estimation parameter expr(lambda) - default c(-2, 2)
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Restricted maximum likelihood approach ("reml"), 
#' (ii) Skewness minimization ("skew") and pooled skewness minimization ("pskew"), 
#' (iii) Divergence minimization by Kolmogorov-Smirnov ("div.ks"), 
#' by Cramer-von-Mises ("div.cvm") or by Kullback-Leibler ("div.kl").
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
est_lme <- function(y, x, formula, data, rand_eff, method = method, 
                    lambdarange = lambdarange, trafo, custom_func, 
                    custom_func_std) {
  
  # Number of explanatory variables
  k <- ncol(x)

  # Get the optimal lambda via optimization on lambdarange
  res <- suppressWarnings(optimize(f = estim_lme, 
                                   y = y,
                                   formula = formula,
                                   data = data,
                                   rand_eff = rand_eff,
                                   method = method,
                                   trafo = trafo,
                                   custom_func = custom_func, 
                                   custom_func_std = custom_func_std,
                                   interval = lambdarange, tol = 0.0001))
  
  if (is.infinite(res$objective)) {
    stop("For some lambda in the interval, the likelihood does not converge.
         Choose another lambdarange.")
  }
  
  # Optimal lambda and corresponding measure: likelihood, skewness or divergence
  lambdaoptim <-  res$minimum
  measoptim <- res$objective
  
  return(list(lambdaoptim = lambdaoptim, 
              measoptim = measoptim))
}







