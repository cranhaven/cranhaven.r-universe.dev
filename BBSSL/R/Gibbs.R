#' @export
#' @name Gibbs
#' @title Gibbs
#' @description This function runs SSVS for linear regression with Spike-and-Slab LASSO prior.
#' By default, this function uses the speed-up trick in Bhattacharya et al., 2016 when p > n.
#'
#' @param y A vector of continuous responses (n x 1).
#' @param X The design matrix (n x p), without an intercept.
#' @param a,b Parameters of the prior.
#' @param lambda A two-dim vector = c(lambda0, lambda1).
#' @param maxiter An integer which specifies the maximum number of iterations for MCMC.
#' @param burn.in An integer which specifies the maximum number of burn-in iterations for MCMC.
#' @param initial.beta A vector of initial values of beta to used. If set to NULL, the LASSO solution with 10-fold cross validation is used. Default is NULL.
#' @param sigma Noise standard deviation. Default is 1.
#'
#' @return A list, including matrix 'beta' ((maxiter-burn.in) x p), matrix 'tau2' ((maxiter-burn.in) x p),
#' matrix 'gamma' ((maxiter-burn.in) x p), vector 'theta' ((maxiter-burn.in) x 1).
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
#' MCchain1 = Gibbs(y, X, a, b, lambda, maxiter = 15001, burn.in = 5001)
Gibbs = function(y, X, a, b, lambda, maxiter, burn.in, initial.beta = NULL, sigma = 1){

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
  initial.tau2 = rep(NA, p)
  for (i in 1:p){
    initial.gamma[i] = rbinom(1, size = 1, prob = prob[i])
    mu = lambda[initial.gamma[i]+1] / abs(initial.beta[i] + 0.5)
    lamb = lambda[initial.gamma[i]+1]^2
    inv.tau = statmod::rinvgauss(n=1, mean = mu, shape = lamb)
    initial.tau2[i] = 1 / inv.tau
  }
  initial.theta = rbeta(1, sum(initial.gamma) + a, p - sum(initial.gamma) + b)

  # ------------- Create data --------------------
  n = length(y)
  p = length(initial.beta)
  beta = matrix(NA, nrow = maxiter, ncol = p)
  tau2 = matrix(NA, nrow = maxiter, ncol = p)
  gamma = matrix(NA, nrow = maxiter, ncol = p)
  theta = rep(NA, maxiter)

  beta[1,] = initial.beta
  tau2[1,] = initial.tau2
  gamma[1,] = initial.gamma
  theta[1] = initial.theta


  # -------------- SSVS --------------------------
  for (i in 2:maxiter){
    start_time <- Sys.time()
    #update beta
    if (p>n){ # p^2n

      d = tau2[i-1,] # d is 1-by-p
      u = as.matrix(rnorm(p) * sqrt(d), nrow = 1) # u is 1-by-p
      delta = rnorm(n)
      XD = X * matrix(d, byrow = T, ncol = p, nrow = n) # complexity O(np)
      v = X %*% u / sigma + delta # complexity O(np)
      H = XD %*% t(X) / sigma^2 + diag(rep(1,n)) # complexity O(n^2p), this is the main bottleneck
      w = Matrix::solve(H, y/sigma-v) # complexity O(n^3)
      beta[i,] = u + (t(XD) / sigma) %*% w # complexity O(np)

    } else { # p^2n

      D = diag(1/tau2[i-1,])
      H = Matrix::solve(t(X) %*% X/sigma^2 + D) # complexity O(p^2n+p^3)
      beta[i,] = mvnfast::rmvn(1, mu = H %*% t(X) %*% y / sigma^2, sigma = H)
    }

    for (j in 1:p){
      #update tau
      mu = lambda[gamma[i-1,j]+1] / abs(beta[i, j])
      lamb = lambda[gamma[i-1,j]+1]^2
      inv.tau = statmod::rinvgauss(n=1, mean = mu, shape = lamb)
      tau2[i,j] = 1 / inv.tau

      #update gamma
      tmp = lambda[2]^2 / lambda[1]^2 * exp( - (lambda[2]^2 - lambda[1]^2) * tau2[i,j] / 2) * theta[i-1] /  (1 - theta[i-1])
      prob = 1 - 1 / (tmp + 1)
      gamma[i,j] = rbinom(1, 1, prob)
    }

    #update theta
    active = sum(gamma[i,])
    theta[i] = rbeta(1, active + a, p - sum(gamma[i,]) + b)

    end_time <- Sys.time()

    svMisc::progress(i, maxiter, progress.bar = T)
    if (i == maxiter) cat("Done!\n")

  }


  # ------------ output ------------------------
  return(list(beta = beta[-(1:burn.in),], tau2 = tau2[-(1:burn.in),], gamma = gamma[-(1:burn.in),], theta = theta[-(1:burn.in)]))
}
