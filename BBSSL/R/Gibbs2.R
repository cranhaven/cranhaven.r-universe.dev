#' @export
#' @name Gibbs2
#' @title Gibbs2
#' @description This function runs one-site Gibbs sampler for linear regression with Spike-and-Slab LASSO prior.
#'
#' @param y A vector of continuous responses (n x 1).
#' @param X The design matrix (n x p), without an intercept.
#' @param a,b Parameters of the prior.
#' @param lambda A two-dim vector = c(lambda0, lambda1).
#' @param maxiter An integer which specifies the maximum number of iterations for MCMC.
#' @param burn.in An integer which specifies the number of burn-in iterations for MCMC.
#' @param initial.beta A vector of initial values of beta to used. If set to NULL, the LASSO solution with 10-fold cross validation is used. Default is NULL.
#' @param sigma Noise standard deviation. Default is 1.
#'
#' @return A list, including matrix beta and gamma of dimension (maxiter-burn.in) x p, vector theta ((maxiter-burn.in) x 1)
#'
#' @examples
#' n = 100; p = 1000;
#' truth.beta = c(2, 3, -3, 4); # high-dimensional case
#' truth.sigma = 1
#' data = Generate_data(truth.beta, p, n, truth.sigma = 1, rho = 0.6,"block",100)
#' y = data$y; X = data$X; beta = data$beta
#'
#' # --------------- set parameters -----------------
#' lambda0 = 50; lambda1 = 0.05; lambda = c(lambda0, lambda1)
#' a = 1; b = p #beta prior for theta
#' MCchain2 = Gibbs2(y, X, a, b, lambda, maxiter = 15001, burn.in = 5001)
Gibbs2 = function(y, X, a, b, lambda, maxiter, burn.in, initial.beta = NULL, sigma = 1){

  n = nrow(X)
  p = ncol(X)
  lambda1 = lambda[2]
  lambda0 = lambda[1]

  X = apply(X, 2, function(t) (t-mean(t)))
  y = y - mean(y)

  # -------------- Initialization -----------------
  if (is.null(initial.beta)){
    cvlambda = glmnet::cv.glmnet(X,y)$lambda.min
    cvfit = glmnet::glmnet(X, y, lambda = cvlambda)
    initial.beta = cvfit$beta
    initial.beta = matrix(initial.beta, nrow = 1)
  }
  prob1 = greybox::dlaplace(initial.beta, mu = 0, scale = 1/lambda1)
  prob0 = greybox::dlaplace(initial.beta, mu = 0, scale = 1/lambda0)
  prob = prob1 / (prob1 + prob0)
  initial.gamma = rep(NA, p)
  for (i in 1:p){
    initial.gamma[i] = rbinom(1, size = 1, prob = prob[i])
  }
  initial.theta = rbeta(1, sum(initial.gamma) + a, p - sum(initial.gamma) + b)

  # ------------- Create data --------------------
  n = length(y)
  p = length(initial.beta)
  beta = matrix(NA, nrow = maxiter, ncol = p)
  gamma = matrix(NA, nrow = maxiter, ncol = p)
  theta = rep(NA, maxiter)

  beta[1,] = initial.beta
  gamma[1,] = initial.gamma
  theta[1] = initial.theta

  XTX = rep(NA, p)
  for (j in 1:p){
    XTX[j] = sum(X[,j]^2)
  }
  res = y - X %*% as.matrix(c(initial.beta), ncol=1)

  # -------------- SSVS --------------------------

  for (i in 2:maxiter){
    start_time <- Sys.time()

    #res = y - X %*% as.matrix(beta[i-1,], ncol=1)

    for (j in 1:p){
      res = res + X[,j] * beta[i-1,j]
      resprod = sum( res * X[,j] )

      s = sigma / sqrt(XTX[j])
      mu10 = (resprod + sigma^2 * lambda1) / XTX[j]
      mu11 = (resprod - sigma^2 * lambda1) / XTX[j]
      mu00 = (resprod + sigma^2 * lambda0) / XTX[j]
      mu01 = (resprod - sigma^2 * lambda0) / XTX[j]
      p01 = 1-pnorm(q=0, mean=mu01, sd=s)
      p00 = pnorm(q=0, mean=mu00, sd=s)
      p11 = 1-pnorm(q=0, mean=mu11, sd=s)
      p10 = pnorm(q=0, mean=mu10, sd=s)

      if(resprod >=0 ){
        c1_div_c0 = lambda1 / lambda0 * exp( ( sigma^2*(lambda1^2-lambda0^2)+2*resprod*(lambda1-lambda0) ) / 2/XTX[j] )
        c1_div_c0 = c1_div_c0 * ( p10 + p11 * exp( -lambda1*2*resprod/XTX[j] ) ) / ( p00 + p01 * exp( -lambda0*2*resprod/XTX[j] ) )
        if(is.na(c1_div_c0)){
          c1_div_c0 = lambda1 / lambda0 * exp( ( sigma^2*(lambda1^2-lambda0^2)+2*resprod*(lambda1-lambda0) ) / 2/XTX[j] -
                                                 lambda0*2*resprod/XTX[j] ) * ( p10 + p11 * exp( -lambda1*2*resprod/XTX[j] ) )
        }
      } else {
        c1_div_c0 = lambda1 / lambda0 * exp( ( sigma^2*(lambda1^2-lambda0^2)-2*resprod*(lambda1-lambda0) ) / 2/XTX[j] )
        c1_div_c0 = c1_div_c0 * ( p11 + p10 * exp( lambda1*2*resprod/XTX[j] ) ) / ( p01 + p00 * exp( lambda0*2*resprod/XTX[j] ) )
        if(is.na(c1_div_c0)){
          c1_div_c0 = lambda1 / lambda0 * exp( ( sigma^2*(lambda1^2-lambda0^2)-2*resprod*(lambda1-lambda0) ) / 2/XTX[j] -
                                                 lambda0*2*resprod/XTX[j]) * ( p11 + p10 * exp( lambda1*2*resprod/XTX[j] ) )
        }
      }

      # update gamma
      tmp = theta[i-1] * c1_div_c0 / (1-theta[i-1])
      prob = 1 - 1 / (tmp + 1)
      gamma[i,j] = rbinom(1, 1, prob)

      # update beta
      thislambda = lambda[gamma[i,j]+1]
      uu = runif(1);
      if (gamma[i,j]==0){
        uu0 = p00 / ( p00 + p01 * exp(- 2*resprod*thislambda / XTX[j] ) )
        if(is.na(uu0)){
          uu0 = 1 / ( 1 + p01 / s * exp(- 2*resprod*thislambda / XTX[j] + mu00^2/2/s^2 )*sqrt(2*pi)*mu00 )
        }
      } else {
        uu0 = p10 / ( p10 + p11 * exp(- 2*resprod*thislambda / XTX[j] ) )
        if(is.na(uu0)){
          uu0 = 1 / ( 1 + p11 / s * exp(- 2*resprod*thislambda / XTX[j] + mu10^2/2/s^2 )*sqrt(2*pi)*mu10 )
        }
      }
      mu0 = (resprod + sigma^2 * thislambda) / XTX[j]
      mu1 = (resprod - sigma^2 * thislambda) / XTX[j]
      if (uu <= uu0){
        beta[i,j] = truncnorm::rtruncnorm( n=1, a=-Inf, b=0, mean=mu0, sd = s )
      }else{
        beta[i,j] = truncnorm::rtruncnorm( n=1, a=0, b=Inf, mean=mu1, sd = s )
      }

      # update residual
       res = res - X[,j] * beta[i,j]
    }

    # update theta
    theta[i] = rbeta( n=1, shape1=a+sum(gamma[i,]), shape2=b+n-sum(gamma[i,]) )
    end_time <- Sys.time()

    svMisc::progress(i, maxiter, progress.bar = T)
    if (i == maxiter) cat("Done!\n")

  }


  # ------------ output ------------------------
  return(list(beta = beta[-(1:burn.in),], gamma = gamma[-(1:burn.in),], theta = theta[-(1:burn.in)]))
}
