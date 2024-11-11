#' @export
#' @title BB-SSL
#' @name BB_SSL
#' @description This function runs BB-SSL, WBB with fixed prior weight, and WBB with random prior weight.
#' It solves the optimization by calling function SSLASSO_2, a variant of the function SSLASSO in CRAN package 'SSLASSO': in the version used,
#' we do NOT standardize the design matrix and allow inputting initial values of beta's.
#'
#' @usage BB_SSL(y, X, method = 3, lambda, NSample, a, b, maxiter=500, eps = 1e-3, burn.in = FALSE,
#' length.out = 50, discard = FALSE, alpha = 3, sigma = 1, initial.beta, penalty = c("adaptive","separable"),
#' theta=0.5)
#'
#' @param y A vector of continuous responses (n x 1).
#' @param X The design matrix (n x p), without an intercept.
#' @param method A number between c(1,2,3) to specify which method to run, method = 1 is fixed WBB, method = 2 is random WBB, method = 3 is BB-SSL.
#' @param lambda A two-dim vector = c(lambda0, lambda1).
#' @param NSample An integer which specifies the number of samples to be generated.
#' @param a,b Parameters of the prior.
#' @param maxiter An integer which specifies the maximum number of iterations for SSLASSO_2 (default maxiter= 500).
#' @param eps Convergence criterion when running SSLASSO_2: converged when difference in regression coefficients is less than eps (default eps = 0.001).
#' @param burn.in A boolean to specify whether to use annealing on a sequence of lambda0's (default burn.in = FALSE).
#' @param length.out An integer to specify the length of sequence of lambda0's used in annealing. This value is not used when burn.in = FALSE. Default is 50.
#' @param discard A boolean to specify whether to discard unconverged sample points.
#' @param alpha The parameter for generating weights in BB-SSL, which follows n x Dirichlet(alpha,...,alpha). Default is 3.
#' @param sigma Noise standard deviation.
#' @param initial.beta A vector of initial values of beta to used when solving SSLASSO_2 (n x 1).
#' @param penalty The penalty (prior) to be applied to the model. Either "separable" (with a fixed theta) or "adaptive" (with a random theta, where theta ~ B(a,p)). The default is "adaptive".
#' @param theta Prior mixing proportion. For "separable" penalty, this value is fixed. For "adaptive" penalty, this value is used as a starting value. Default is 0.5.
#'
#' @return A list of matrices, including matrix beta and matrix gamma (n x p).
#'
#' @author Lizhen Nie <lizhen@statistics.uchicago.edu>, Veronika Rockova <Veronika.Rockova@chicagobooth.edu>
#' @references Nie, L., & Ročková, V. (2020). Bayesian Bootstrap Spike-and-Slab LASSO. arXiv:2011.14279.
#'
#' @examples
#' # -------------- Generate Data --------------
#' n = 100; p = 1000;
#' truth.beta = c(2, 3, -3, 4); # high-dimensional case
#' truth.sigma = 1
#' data = Generate_data(truth.beta, p, n, truth.sigma = 1, rho = 0.6,"block",100)
#' y = data$y; X = data$X; beta = data$beta
#'
#' # --------------- set parameters -----------------
#' lambda0 = 50; lambda1 = 0.05; lambda = c(lambda0, lambda1)
#' a = 1; b = p #beta prior for theta
#'
#' # Use SSLASSO_2 solution to get an initial value of beta's
#' result = SSLASSO_2(X, y, penalty = "adaptive", variance = "fixed", sigma = truth.sigma,
#'                  lambda1 = lambda1, lambda0 = seq(lambda1, lambda0, length.out = 50), a = a, b = b,
#'                  max.iter = 500, initial.beta = rep(0,p))
#'
#' #--------------- WBB with fixed prior --------------
#' fixed.WBB.result = BB_SSL(y, X, method = 1, lambda = c(lambda0, lambda1),  NSample = 1000, a, b,
#' maxiter = 500, length.out = 50, burn.in = F, discard = T,initial.beta = result$beta[,50])
#'
#' #--------------- WBB with random prior--------------
#' random.WBB.result = BB_SSL(y, X, method = 2, lambda = c(lambda0, lambda1), NSample = 1000, a, b,
#' maxiter = 500, length.out = 50, burn.in = F, discard = T,initial.beta = result$beta[,50])
#'
#' #--------------- BB-SSL -------------
#' BB.SSL.result = BB_SSL(y, X, method = 3, lambda = c(lambda0, lambda1), NSample = 100, a, b,
#' maxiter = 500, length.out = 50, burn.in = F, discard = T, alpha=1,initial.beta = result$beta[,50])
#'
BB_SSL = function(y, X, method = 3, lambda, NSample, a, b, maxiter=500, eps = 1e-3, burn.in = FALSE, length.out = 50, discard = FALSE, alpha = 3,
                  sigma = 1, initial.beta, penalty = "adaptive", theta=0.5){

  n = nrow(X)
  p = ncol(X)
  beta = matrix(NA, nrow = NSample, ncol = p)
  gamma = matrix(0, nrow = NSample, ncol = p)
  lambda1 = lambda[1]; lambda0 = lambda[2]

  i = 1

  while (i <= NSample){

    start_time <- Sys.time()

    # WBB with fixed prior weight
    if (method==1){
      mu = rep(0, p)
      w = rexp(n)
    }

    # WBB with random prior weight
    if (method==2){
      wp = rexp(1)
      mu = rep(0,p)
      w = rexp(n)
      w = w / wp
    }

    # BB-SSL
    if (method==3){
      mu = rmutil::rlaplace(p, m=0,s=1/lambda[1])
      w = rgamma(n, shape = alpha, rate = 1)
      adj = sum(w)
      w = w * (n/adj)
    }

    sqrt.w = w^(1/2)
    weighted.X = matrix(sqrt.w, nrow = n, ncol = p) * X # O(np)
    weighted.y = sqrt.w * y
    weighted.y = weighted.y - weighted.X %*% mu

    if(burn.in == TRUE){
      result = SSLASSO_2(weighted.X, weighted.y, penalty = penalty, variance = "fixed", sigma = sigma^2, theta = theta,
                       lambda1 = lambda[2], lambda0 = seq(lambda[2], lambda[1], length.out = length.out), a = a, b = b, max.iter = maxiter,
                      eps = eps, initial.beta = initial.beta)

      #if doesn't converge, discard this result
      if(result$iter[length.out]==maxiter & discard == TRUE){next}

    }else{
      result = SSLASSO_2(weighted.X, weighted.y, penalty = penalty, variance = "fixed", sigma = sigma^2, theta = theta,
                       lambda1 = lambda[2], lambda0 = lambda[1], a = a, b = b, max.iter = maxiter, eps = eps,
                       initial.beta = initial.beta)

      if(result$iter==maxiter & discard == TRUE){next}
    }

    tmp = result$beta
    mtmp = ncol(tmp)
    beta[i,] = tmp[,mtmp]
    beta[i,] = beta[i,] + mu

    # threshold beta to get gamma
    tmp = lambda1/lambda0 * exp(-(lambda1-lambda0)*abs(beta[i,]))
    gamma[i,] = (tmp / (tmp + 1) > 0.5)
    index = is.na(gamma[i,])
    gamma[i,index] = 1


    end_time <- Sys.time()

    svMisc::progress(i, NSample-1, progress.bar = TRUE)
    if (i == NSample) cat("Done!\n")
    #if (i %% 100 == 0){
    #  print(i); print(end_time - start_time)
    #}
    # print(i); print(end_time - start_time)

    i = i + 1
  }

  return(list(beta = beta, gamma = gamma))
}
