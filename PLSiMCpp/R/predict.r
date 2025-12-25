

#' @title Predict according to the Estimated Parameters
#' 
#' @description Predict Y based on new observations.
#' 
#' @param object fitted partially linear single-index model, which could be obtained by 
#' @param x_test input matrix (linear covariates of test set).
#' @param z_test input matrix (nonlinear covariates of test set).
#' @param \dots additional arguments.
#' 
#' \link{plsim.MAVE}, or \link{plsim.est}, or \link{plsim.vs.soft}. 
#'
#' @return
#' \item{y_hat}{prediction.}
#'
#' @export
#'
#' @examples
#'
#' n = 50
#' sigma = 0.1
#'
#' alpha = matrix(1, 2, 1)
#' alpha = alpha/norm(alpha, "2")
#'
#' beta = matrix(4, 1, 1)
#'
#' x = matrix(1, n, 1)
#' x_test = matrix(1,n,1)
#'
#' z = matrix(runif(n*2), n, 2)
#' z_test = matrix(runif(n*2), n, 2)
#'
#' y = 4*((z%*%alpha-1/sqrt(2))^2) + x%*%beta + sigma*matrix(rnorm(n),n,1)
#' y_test = 4*((z_test%*%alpha-1/sqrt(2))^2) + x_test%*%beta + sigma*matrix(rnorm(n),n,1)
#'
#' 
#' # Obtain parameters in PLSiM using Profile Least Squares Estimator
#' fit_plsimest = plsim.est(x, z, y)
#' 
#' preds_plsimest = predict(fit_plsimest, x_test, z_test)
#' 
#' # Print the MSE of the Profile Least Squares Estimator method
#' print( sum( (preds_plsimest-y_test)^2)/nrow(y_test) )
#' 
#' # Obtain parameters in PLSiM using Penalized Profile Least Squares Estimator
#' fit_plsim = plsim.vs.soft(x, z, y,lambda = 0.01)
#' 
#' preds_plsim = predict(fit_plsim, x_test, z_test)
#' 
#' # Print the MSE of the Penalized Profile Least Squares Estimator method
#' print( sum( (preds_plsim-y_test)^2)/nrow(y_test) )
#'
predict.pls <- function(object, x_test=NULL, z_test, ...)
{
  z_train = object$data$z
  zeta = object$zeta
  h = object$data$h
  eta = object$eta
  
  n_test = nrow(z_test)
  
  if( is.null(x_test))
  {
    d1 = 0
  }
  else
  {
    d1 = ncol(x_test) 
  }
  d2 = ncol(z_train)
  
  alpha = zeta[1:d2]
  if( !is.null(x_test) )
  {
    beta = zeta[(d2+1):(d2+d1)] 
  }
  
  np_train = z_train%*%alpha
  np_test = z_test%*%alpha
  
  eta_test = matrix(0,n_test,1)
  
  for(i in 1:n_test)
  {
    w = .kernel_f(np_test[i],np_train,h)
    
    if(sum(w) == 0)
    {
      idx = which.min(abs(np_test[i]-np_train))
      eta_test[i] = eta[idx]
    }
    else
    {
      w = w/sum(w)
      eta_test[i] = sum(eta*w);
    }
    
  }
  
  if(is.null(x_test))
  {
    y_hat = eta_test
  }
  else
  {
    y_hat = x_test%*%beta + eta_test 
  }
  
  return(y_hat)
  
}
