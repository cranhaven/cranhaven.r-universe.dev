#' Robust Mixture Regression with Thresholding-Embedded EM Algorithm for Penalized Estimation
#'
#' A robust mixture regression model that simultaneously conducts outlier detection and
#' robust parameter estimation. It uses a sparse, case-specific, and scale-dependent
#' mean-shift mixture model parameterization (Yu et al., 2017):
#' \deqn{f(y_i|\boldsymbol{x}_i,\boldsymbol{\theta},\boldsymbol{\gamma}_i) = \sum_{j=1}^C\pi_j\phi(y_i;\boldsymbol{x}^{\top}\boldsymbol{\beta}_j+\gamma_{ij}\sigma_j,\sigma_j^2),}
#' \eqn{i=1,\cdots,n}, where \eqn{C} is the number of components in the model,
#' \eqn{\boldsymbol{\theta}=(\pi_1,\boldsymbol{\beta}_1,\sigma_1,..,\pi_{C},\boldsymbol{\beta}_C,\sigma_C)^{\top}}
#' is the parameter to estimate,
#' and \eqn{\boldsymbol{\gamma}_i=(\gamma_{i1},...,\gamma_{iC})^{\top}} is a vector of mean-shift parameter for the ith observation.
#'
#' @usage
#' mixregRM2(x, y, C = 2, ini = NULL, nstart = 20, tol = 1e-02, maxiter = 50,
#'           method = c("HARD", "SOFT"), sigma.const = 0.001, lambda = 0.001)
#'
#' @details
#'   The parameters are estimated by maximizing the corresponding penalized log-likelihood function using an EM algorithm.
#'   The thresholding rule involes the estimation of \eqn{\gamma_{ij}} corresponding to different penalty:
#'   * Soft threshold: \eqn{\hat{\gamma}_{ij} = sgn(\epsilon_{ij})(|\epsilon_{ij}|-\lambda_{ij}^*)_{+})}, corresponding to the \eqn{l_1} penalty.
#'   * Hard threshold: \eqn{\hat{\gamma}_{ij} = \epsilon_{ij}I(|\epsilon_{ij}|>\lambda_{ij}^*))}, corresponding to the \eqn{l_0} penalty.
#'
#'   Here, \eqn{\epsilon_{ij} = (y_i-\boldsymbol{x}_i^{\top}\boldsymbol{\beta_j})/\sigma_j} and
#'   \eqn{(\cdot)_{+}=\max(\cdot,0)}. Also, \eqn{\lambda_{ij}^*} is taken as \eqn{\lambda/p_{ij}^{(k+1)}} for soft threshold and
#'   \eqn{\lambda/\sqrt{p_{ij}^{(k+1)}}} for hard threshold.
#'
#' @param x an n by p data matrix where n is the number of observations and p is the number of explanatory variables.
#'   The intercept term will automatically be added to the data.
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values
#'   using the \code{\link{mixreg}} function. It can be a list with the form of \code{list(pi, beta, sigma, gamma)}, where
#'   \code{pi} is a vector of C mixing proportions,
#'   \code{beta} is a C by (p + 1) matrix for regression coefficients of C components,
#'   \code{sigma} is a vector of C standard deviations, and
#'   \code{gamma} is a vector of C mean shift values.
#' @param nstart number of initializations to try. Default is 20.
#' @param tol stopping criteria (threshold value) for the EM algorithm. Default is 1e-02.
#' @param maxiter maximum number of iterations for the EM algorithm. Default is 50.
#' @param method character, determining which threshold method to use: \code{HARD} or \code{SOFT}.
#'   Default is \code{HARD}. See details.
#' @param sigma.const constraint on the ratio of minimum and maximum values of sigma. Default is 0.001.
#' @param lambda tuning parameter in the penalty term. It can be found based on BIC. See Yu et al. (2017) for more details.
#'
#' @return A list containing the following elements:
#'   \item{pi}{C-dimensional vector of estimated mixing proportions.}
#'   \item{beta}{C by (p + 1) matrix of estimated regression coefficients.}
#'   \item{sigma}{C-dimensional vector of estimated standard deviations.}
#'   \item{gamma}{n-dimensional vector of estimated mean shift values.}
#'   \item{posterior}{n by C matrix of posterior probabilities of each observation belonging to each component.}
#'   \item{run}{total number of iterations after convergence.}
#'
#' @seealso \code{\link{mixreg}} for initial value calculation.
#'
#' @references
#'   Yu, C., Yao, W., and Chen, K. (2017). A new method for robust mixture regression.
#'   Canadian Journal of Statistics, 45(1), 77-94.
#'
#' @importFrom stats optim
#' @export
#' @examples
#' data(tone)
#' y = tone$tuned
#' x = tone$stretchratio
#' k = 160
#' x[151:k] = 0
#' y[151:k] = 5
#' \donttest{est_RM2 = mixregRM2(x, y, lambda = 1)}
mixregRM2 <- function(x, y, C = 2, ini = NULL, nstart = 20, tol = 1e-02, maxiter = 50,
                       method = c("HARD", "SOFT"), sigma.const = 0.001, lambda = 0.001){

  #SK-- Used functions: mixreg, hard_plh (or soft_plh), hard_ipod (or soft_ipod)
  # Match arguments
  method <- match.arg(method)
  f_plh <- switch(method, "HARD" = hard_plh, "SOFT" = soft_plh)
  f_ipod <- switch(method, "HARD" = hard_ipod, "SOFT" = soft_ipod)


  outer.conv <- inner.conv <- tol
  runnum1 <- runnum2 <- maxiter

  m = C

  # Add intercept variable --SK 9/3/2023
  #if (addintercept){
      X = cbind(1, x)
  #}

  n = length(y)
  p = ncol(X)
  y = matrix(y)

  # Add default value for pi, beta, sigma, gamma Xin Shen
  # If the initial value is not assigned, estimate by mix regression with Gaussian assumption.
  # If the output of mixreg is updated to be more user friendly, be sure to update here as well
  #if(is.null(pi0) | is.null(beta0) | is.null(sig0) | is.null(gamma0)){
  #  ini_est = mixreg(x, y, m, nstart) # SK 08/31/2023 20 -> nstart
  #}
  #if(is.null(pi0)){
  #  pi0 = matrix(ini_est$theta[, p + 2])
  #}
  #if(is.null(beta0)){
  #  beta0 = t(ini_est$theta[, 1:p])
  #}
  #if(is.null(sig0)){
  #  sig0 = matrix(ini_est$theta[, p + 1])
  #}
  #if(is.null(gamma0)){
  #  gamma0 = matrix(0, nrow = n)
  #}
  ## SK 08/31/2023
  if(is.null(ini)){
    ini = mixreg(x, y, m, nstart, tol) #SK mixreg(x, y, m, nstart)
    beta0 = t(ini$beta)
    sig0 = matrix(ini$sigma)
    pi0 = matrix(ini$pi)
    #beta0 = t(ini_est$theta[, 1:p])
    #sig0 = matrix(ini_est$theta[, p + 1])
    #pi0 = matrix(ini_est$theta[, p + 2])
    gamma0 = matrix(0, nrow = n)
  } else {
    pi0 = ini$pi
    beta0 = ini$beta
    sig0 = ini$sigma
    gamma0 = ini$gamma
  }

  pi1 = as.matrix(pi0)
  beta1 = as.matrix(beta0)
  sig1 = as.matrix(sig0)
  gamma1 = gamma0

  ###--- tuning lambda
  #if(is.null(lambda)){
  #
  #}
  ###---

  out_lh = f_plh(y = y, X = X, pi = pi1, beta = beta1, sigma = sig1, gamma = gamma1, lambda = lambda)

  dif = 1
  obj <- vector()
  run = 0
  restart = 0

  ## EM loop
  while (dif > outer.conv & run < runnum1 & restart < 10){

    out_lh0 = out_lh
    run = run + 1
    oldgamma = gamma1
    oldpi = pi1
    oldbeta = beta1
    oldsig = sig1

    pk = matrix(rep(0, m * n), nrow = n)
    for(j in seq(m)){
      pk[, j] = oldpi[j, ] * pmax(10^(-300), stats::dnorm(y - oldgamma * oldsig[j, ] - X %*% oldbeta[, j], mean = 0, sd = oldsig[j, ]))
    }

    p_ij = pk/matrix(rep(apply(pk, 1, sum), m), nrow = n)
    n_p = apply(p_ij, 2, sum)

    # M-step
    pi2 = oldpi
    beta2 = oldbeta
    sig2 = oldsig
    gamma2 = oldgamma

    inner_lh = f_plh(y = y, X = X, pi = oldpi, beta = oldbeta, sigma = oldsig, gamma = oldgamma, lambda = lambda)

    run1 = 0
    dif1 = 1
    while(dif1 > inner.conv & run1 < runnum2){

      inner_lh0 = inner_lh
      run1 = run1 + 1
      prepi = pi2
      prebeta = beta2
      presig = sig2
      pregamma = gamma2
      pi = matrix(rep(0, m), nrow = m)
      beta = matrix(rep(0, p * m), nrow = p)
      sig = matrix(rep(0, m), nrow = m)

      # update gamma first
      gamma = matrix(c(rep(0, n)), nrow = n)
      for(i in seq(n)){
        gamma[i] = f_ipod(theta = ((p_ij[i,]) %*% ((y[i, ] - (X %*% beta2)[i, ])/(sig2))), lambda = lambda)
      }
      gamma2 = as.matrix(gamma, nrow = n)

      for(k in seq(m)){
        pi[k] = n_p[k]/n
        wt = diag(p_ij[, k])
        beta[, k] = MASS::ginv(t(X) %*% wt %*% X) %*% t(X) %*% wt %*% (y - gamma2 * presig[k, ])
      }
      sig = stats::optim(presig[1, ], y = y, X = X, beta = beta, p_ij = p_ij, gamma = gamma2, fn = sigmaf,
                  method = "BFGS", hessian = FALSE, m = m)$par
      pi2 = as.matrix(pi, nrow = m)
      beta2 = as.matrix(beta, nrow = p)
      sig2 = matrix(c(rep(sig, m)), nrow = m)

      # check if there is extreme value
      if(min(sig2)/max(sig2) < sigma.const){

        warning('One of the sigma is going to 0, start again with another initial value.')

        # generate one new initial value from mixrege
        ini_est = mixreg(x, y, m, 1, tol) #SK mixreg(x, y, m, 1)

        beta_new = t(ini_est$beta)
        sig_new = matrix(ini_est$sigma)
        pi_new = matrix(ini_est$pi)
        #beta_new = t(ini_est$theta[, 1:p])
        #sig_new = matrix(ini_est$theta[, p + 1])
        #pi_new = matrix(ini_est$theta[, p + 2])
        gamma_new = matrix(0, nrow = n)
        out1 = list(pi = pi_new, beta = beta_new, sig = sig_new, gamma = gamma_new, run = 0)
        out_lh0 = 0
        obj <- vector()
        restart = restart + 1
        break
      }
      # sig2[sig2<sigma.const] <- 0.5        ##truncate the extremely small sigma
      # sig2[sig2>3] <- 1

      # updated loglikelihood in the inner-loop
      inner_lh = f_plh(y = y, X = X, pi = pi2, beta = beta2, sigma = sig2, gamma = gamma2, lambda = lambda)
      dif1 = inner_lh - inner_lh0
      #if(dif1 < -0.1 & sigma.flag==FALSE) message(paste("Warning: Inner loop decreases.  DIFF = ", round(dif1,4),"  Sig.truncated = ", sigma.flag, sep=" "))
      ####
      if(dif1 > (-1e-10) & dif1 < 0){
        dif1 = 0
      }
      out1 = list(pi = pi2, beta = beta2, sig = sig2, gamma = gamma2, run = run1)
    }
    pi1 = out1$pi
    beta1 = out1$beta
    sig1 = out1$sig
    gamma1 = out1$gamma

    out_lh = f_plh(y = y, X = X, pi = pi1, beta = beta1, sigma = sig1, gamma = gamma1, lambda = lambda)

    obj[run] <- out_lh
    dif = out_lh - out_lh0
    if(dif < 0 & sum((beta1 - beta2)^2) > 1e-2){
      warning(paste("Outer loop decreases. DIFF =", dif, sep =" "))
    }
    if(dif> (-1e-10) & dif < 0){
      dif = 0
    }
  }

  # check if the algorithm converged
  cov = TRUE
  if(run >= runnum1){
    warning('The algorithm didn\'t converge. stopped due to reaching the maximum iteration.')
    cov = FALSE
  }
  if(restart >= 10){
    warning('The algorithm didn\'t converge. stopped due to too many restarts.')
    cov = FALSE
  }

  out = list(pi = as.vector(pi1), beta = t(beta1), sigma = as.vector(sig1), gamma = as.vector(gamma1),
             posterior = p_ij, run = run)#SK, cov = cov)#,diff=dif1,dif=dif) #seems do not need these value to return
}

##############################################
## mixreg based on multiple initial values
##############################################
#----------------------------------------------------------------------------------------------
# Programming Note:
# This function is used to estimate the initial values of parameters in mixregRM2 function.
# If any update is made about the output of this function, make sure to update corresponding part in mixregRM2.
#----------------------------------------------------------------------------------------------
#' MLE of Mixture Regression with Normal Errors
#'
#' `mixreg' provides the MLE estimates of a mixture of regression models with normal errors.
#' The result from this function can be used as initial values of the \code{\link{mixregRM2}} function.
#'
#' @usage
#' mixreg(x, y, C = 2, nstart = 20, tol = 1e-05)
#'
#' @param x an n by p data matrix where n is the number of observations and p is the number of explanatory variables.
#'   The intercept term will automatically be added to the data.
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param nstart number of initializations to try. Default is 20.
#' @param tol stopping criteria (threshold value) for the EM algorithm. Default is 1e-05.
#'
#' @return A list containing the following elements:
#'   \item{pi}{C-dimensional vector of estimated mixing proportions.}
#'   \item{beta}{C by (p + 1) matrix of estimated regression coefficients.}
#'   \item{sigma}{C-dimensional vector of estimated standard deviations.}
#'   \item{lik}{final likelihood.}
#'   \item{run}{total number of iterations after convergence.}
#'
#' @seealso \code{\link{mixregRM2}}
#'
#' @export
#' @examples
#' data(tone)
#' y = tone$tuned
#' x = tone$stretchratio
#' k = 160
#' x[151:k] = 0
#' y[151:k] = 5
#' est = mixreg(x, y, 2, nstart = 1)
mixreg <- function(x, y, C = 2, nstart = 20, tol = 1e-05){

  #SK-- Used functions: mixregone

  k = C
  n = length(y)
  x = matrix(x, nrow = n)
  a = dim(x)
  p = a[2] + 1
  n1 = 2 * p
  lh = rep(0, nstart)
  bet = matrix(rep(0, k * p), nrow = k)
  sig = 0

  for(j in seq(k)){
    ind = sample(1:n, n1)
    X = cbind(rep(1, n1), x[ind, ])
    bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
    sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
  }
  pr = rep(1/k, k)
  sig = sig/n1/k
  est = mixregone(x, y, bet, sig, pr, k, tol) #SK mixregone(x, y, bet, sig, pr, k)
  lh[1] = est$lik

  for(i in seq(nstart - 1)){
    bet = matrix(rep(0, k * p), nrow = k)
    sig = 0
    for(j in seq(k)){
      ind = sample(1:n, n1)
      X = cbind(rep(1, n1), x[ind, ])
      bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
      sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
    }
    pr = rep(1/k, k)
    sig = sig/n1/k
    temp = mixregone(x, y, bet, sig, pr, k, tol) #SK mixregone(x, y, bet, sig, pr, k)
    lh[i + 1] = temp$lik
    if(lh[i + 1] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])){
      est = temp
    }
  }

  #SK est$inilikelihoodseq = lh
  #colnames(out$theta) <- c("beta.intercept", "beta.slope", "sigma", "prop")
  npar <- ncol(est$theta)
  out <- list(pi = est$theta[, npar],
              beta = est$theta[, - c(npar - 1, npar)],
              sigma = est$theta[, npar - 1])
  return(out)
}

#' Robust EM Algorithm For Mixture of Linear Regression Based on Bisquare Function
#'
#' `mixregBisq' is used to robustly estimate the parameters of a mixture regression model
#' using the bisquare function based on multiple initial values (Bai et al., 2012). The solution is the mode
#' of the solutions obtained from all initial values.
#'
#' @usage
#' mixregBisq(x, y, C = 2, nstart = 20)
#'
#' @param x an n by p data matrix where n is the number of observations and p is the number of explanatory variables.
#'   The intercept term will automatically be added to the data.
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param nstart number of initializations to try. Default is 20.
#'
#' @return A list containing the following element:
#'   \item{pi}{C-dimensional vector of estimated mixing proportions.}
#'   \item{beta}{C by (p + 1) matrix of estimated regression coefficients.}
#'   \item{sigma}{C-dimensional vector of estimated standard deviations.}
#'
#' @references
#'   Bai, X., Yao, W., and Boyer, J. E. (2012). Robust fitting of mixture regression models.
#'   Computational Statistics & Data Analysis, 56(7), 2347-2359.
#'
#' @export
#' @examples
#' data(tone)
#' y = tone$tuned
#' x = tone$stretchratio
#' k = 160
#' x[151:k] = 0
#' y[151:k] = 5
#' est_bi = mixregBisq(x, y, 2, nstart = 20)
mixregBisq <- function(x, y, C = 2, nstart = 20){

  #SK-- Used functions: permutations, mixregBisqone

  # theta: estimated parameters matrix for the best estimator (the estimated value with most initial values converge to),
  # the columns are beta(intercept, slopes), sigma, proportion for each component
  # estall: all estimated parameters
  # uniqueest: unique estimated parameters
  # countuniqueest: number of unique estimated parameters
  # uniqueestindex: which initial values gives the unique estimators
  # bestindex: index of initial value gives the best estimated parameter
  # estindex: matrix of each initial value converge to which set of estimated parameters.

  m = C
  n = length(y)
  x = matrix(x, nrow = n)
  a = dim(x)
  p = a[2] + 1
  n1 = 2 * p
  perm = permutations(m, m)
  sig = 0
  ind1 = c()
  bet = matrix(rep(0, 2 * p), nrow = 2)

  for(j in seq(m)){
    ind1 = sample(1:n, n1)
    X = cbind(rep(1, n1), x[ind1, ])
    bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind1]
    sig = sig + sum((y[ind1] - X %*% bet[j, ])^2)
  }
  pr = rep(1/m, m)
  sig = max(sig/n1/m)
  est = mixregBisqone(x, y, bet, sig, pr, m)
  lenpar = length(c(est$theta))
  theta = matrix(rep(0, lenpar * (nstart)), ncol = lenpar)
  theta[1, ] = c(est$theta)
  minsig = 10^(-1) * sig
  trimbet = matrix(theta[1, 1:(p * m)], nrow = m)
  trimbet = matrix(rep(matrix(t(trimbet), ncol = p * m, byrow = TRUE), gamma(m + 1)), ncol = p * m, byrow = TRUE)
  ind = matrix(rep(0, nstart), nrow = 1)
  ind[1] = 1
  numsol = 1
  solindex = 1
  sol = matrix(theta[1, ], nrow = 1)

  for(i in 2:nstart){
    sig = 0
    ind1 = c()
    for(j in seq(m)){
      ind1 = sample(1:n, n1)
      X = cbind(rep(1, n1), x[ind1, ])
      bet[j, ]= MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind1]
      sig = sig + sum((y[ind1] - X %*% bet[j, ])^2)
    }
    pr = rep(1/m, m)
    sig = max(sig/n1/m, minsig)
    est = mixregBisqone(x, y, bet, sig, pr, m)
    theta[i, ] = est$theta
    temp = matrix(theta[i, 1:(p * m)], nrow = m)
    temp = matrix(t(temp[t(perm), ]), ncol = p * m, byrow = TRUE)
    dif = apply((trimbet - temp)^2, 1, sum)
    temp1 = which(dif == min(dif))
    theta[i, ] = c(c(matrix(temp[temp1[1], ], nrow = m, byrow = TRUE)), theta[i, p * m + perm[temp1[1], ]], theta[i, p * m + m + perm[temp1[1], ]])
    dif = apply((matrix(rep(theta[i, 1:(p * m)], numsol), nrow = numsol, byrow = TRUE) - sol[, 1:(p * m)])^2, 1, sum)
    if(min(dif) > 0.1){
      sol = rbind(sol, theta[i, ])
      numsol = numsol + 1
      solindex = c(solindex, i)
      ind = rbind(ind, rep(0, nstart))
      ind[numsol, i] = 1
    } else {
      ind1 = which(dif == min(dif))
      ind[ind1, i] = 1
    }
  }
  num = apply(ind, 1, sum)
  ind1 = order(- num)
  bestindex = ind1
  for(j in seq(numsol)){
    if(min(sol[ind1[j], (p * m + m + 1):(p * m + 2 * m)]) > 0.05){
      index =1
      est = matrix(sol[ind1[j], ], nrow = m)
      for(l in seq(m - 1)){
        temp = matrix(rep(est[l, 1:p], m - l), nrow = m - l, byrow = TRUE) - est[(l + 1):m, 1:p]
        temp = matrix(temp, nrow = m - l)
        dif = apply(temp^2, 1, sum)
        if(min(dif) < 0.1){
          index = 0
          break
        }
      }
      if(index == 1){
        bestindex = ind1[j]
        break
      }
    }
  }
  est = sol[bestindex[1], ]
  # out=list(theta=matrix(est,nrow=m),estall=theta, uniqueest=sol, countuniqueest=num,uniqueestindex=solindex, bestindex= solindex[bestindex],estindex=ind);out }
  #SK-- 9/4/2023
  #out = list(theta = matrix(est, nrow = m))
  #colnames(out$theta) <- c("beta.intercept", "beta.slope", "sigma", "prop")
  est = list(theta = matrix(est, nrow = m))
  npar <- ncol(est$theta)
  out <- list(pi = est$theta[, npar],
              beta = est$theta[, - c(npar - 1, npar)],
              sigma = est$theta[, npar - 1])
  return(out)
}

#' Robust Regression Estimator Using Trimmed Likelihood
#'
#' `mixregTrim' is used for robust regression estimation of a mixture model using the trimmed likelihood estimator
#' (Neykov et al., 2007). It trims the data to reduce the impact of outliers on the model.
#'
#' @usage
#' mixregTrim(x, y, C = 2, keep = 0.95, nstart = 20)
#'
#' @param x an n by p data matrix where n is the number of observations and p is the number of explanatory variables.
#'   The intercept term will automatically be added to the data.
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param keep proportion of data to be kept after trimming, ranging from 0 to 1. Default is 0.95.
#' @param nstart number of initializations to try. Default is 20.
#'
#' @return A list containing the following elements:
#'   \item{pi}{C-dimensional vector of estimated mixing proportions.}
#'   \item{beta}{C by (p + 1) matrix of estimated regression coefficients.}
#'   \item{sigma}{C-dimensional vector of estimated standard deviations.}
#'   \item{lik}{final likelihood.}
#'
#' @references
#'   Neykov, N., Filzmoser, P., Dimova, R., and Neytchev, P. (2007). Robust fitting of mixtures using
#'   the trimmed likelihood estimator. Computational Statistics & Data Analysis, 52(1), 299-308.
#'
#' @export
#' @examples
#' data(tone)
#' y = tone$tuned
#' x = tone$stretchratio
#' k = 160
#' x[151:k] = 0
#' y[151:k] = 5
#' est_TLE = mixregTrim(x, y, 2, 0.95, nstart = 1)
mixregTrim <- function(x, y, C = 2, keep = 0.95, nstart = 20){

  #SK-- Used functions: mixregTrimone

  k = C
  n = length(y)
  x = matrix(x, nrow = n)
  a = dim(x)
  p = a[2] + 1
  n1 = 2 * p
  bet = matrix(rep(0, k * p), nrow = k)
  sig = 0
  for(j in seq(k)){
    ind = sample(1:n, n1)
    X = cbind(rep(1, n1), x[ind, ])
    bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
    sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
  }
  pr = rep(1/k, k)
  sig = sig/n1/k
  lh = rep(0, nstart)
  est = mixregTrimone(x, y, k, keep, bet, sig, pr)
  lh[1] = est$lik
  for(i in seq(nstart - 1)){
    sig = 0
    for(j in seq(k)){
      ind = sample(1:n, n1)
      X = cbind(rep(1, n1), x[ind, ])
      bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
      sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
    }
    pr = rep(1/k, k)
    sig = sig/n1/k
    temp = mixregTrimone(x, y, k, keep, bet, sig, pr)
    lh[i + 1] = temp$lik
    if(lh[i + 1] > est$lik){
      est = temp
    }
  }

  #colnames(est$theta) <- c("beta.intercept", "beta.slope", "sigma", "prop")
  # est=list(theta=est$theta,finallikelihood=est$likelihood,likelihoodseq=lh)
  #est = list(theta = est$theta, lik = est$lik)
  #est

  #SK-- 9/4/2023
  #est = list(theta = matrix(est, nrow = m), lik = est$lik)
  npar <- ncol(est$theta)
  out <- list(pi = est$theta[, npar],
              beta = est$theta[, - c(npar - 1, npar)],
              sigma = est$theta[, npar - 1],
              lik = est$lik)
  return(out)
}

#' Robust Mixture Regression with T-distribution
#'
#' `mixregT' provides a robust estimation for a mixture of linear regression models
#' by assuming that the error terms follow the t-distribution (Yao et al., 2014). The degrees of freedom
#' is adaptively estimated.
#'
#' @usage
#' mixregT(x, y, C = 2, maxdf = 30, nstart = 20, tol = 1e-05)
#'
#' @param x an n by p data matrix where n is the number of observations and p is the number of explanatory variables.
#'   The intercept term will automatically be added to the data.
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param maxdf maximum degrees of freedom for the t-distribution. Default is 30.
#' @param nstart number of initializations to try. Default is 20.
#' @param tol threshold value (stopping criteria) for the EM algorithm. Default is 1e-05.
#'
#' @return A list containing the following elements:
#'   \item{pi}{C-dimensional vector of estimated mixing proportions.}
#'   \item{beta}{C by (p + 1) matrix of estimated regression coefficients.}
#'   \item{sigma}{C-dimensional vector of estimated standard deviations.}
#'   \item{lik}{final likelihood.}
#'   \item{df}{estimated degrees of freedom of the t-distribution.}
#'   \item{run}{total number of iterations after convergence.}
#'
#' @seealso \code{\link{mixregLap}} for robust estimation with Laplace distribution.
#'
#' @references
#'   Yao, W., Wei, Y., and Yu, C. (2014). Robust mixture regression using the t-distribution.
#'   Computational Statistics & Data Analysis, 71, 116-127.
#'
#' @importFrom robustbase covMcd
#' @export
#' @examples
#' data(tone)
#' y = tone$tuned
#' x = tone$stretchratio
#' k = 160
#' x[151:k] = 0
#' y[151:k] = 5
#' \donttest{est_t = mixregT(x, y, 2, nstart = 20, tol = 0.1)}
mixregT <- function(x, y, C = 2, maxdf = 30, nstart = 20, tol = 1e-05){

  # degreerange: all degree of freedoms used to estimate the parameters
  # vlikelihoodseq: all likelihood associated with degrees of freedom
  # lh: likelihood list for all initial values

  m = C
  n = length(y)
  x = matrix(x, nrow = n)
  a = dim(x)
  p = a[2] + 1
  n1 = 2 * p
  bet = matrix(rep(0, m * p), nrow = m)
  sig = 0
  for(j in seq(m)){
    ind = sample(1:n, n1)
    X = cbind(rep(1, n1), x[ind, ])
    bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
    sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
  }
  pr = rep(1/m, m)
  sig = sig/n1/m
  lh = rep(0, nstart)
  est = mixregTv(x, y, bet, sig, pr, m, maxdf, tol)
  lh[1] = est$lik
  for(i in seq(nstart - 1)){
    sig = 0
    for(j in seq(m)){
      ind = sample(1:n, n1)
      X = cbind(rep(1, n1), x[ind, ])
      bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
      sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
    }
    pr = rep(1/m, m)
    sig = sig/n1/m
    temp = mixregTv(x, y, bet, sig, pr, m, maxdf, tol)
    lh[i + 1] = temp$lik
    if(lh[i + 1] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])){
      est = temp
    }
  }
  est$degreerange = c(1, maxdf)
  if(maxdf < 30 | tol > 10^(-5)){
    bet = est$theta[, 1:p]
    sig = est$theta[, p + 1]
    pr = est$theta[, p + 2]
    est = mixregTv(x, y, bet, sig[1], pr, m, 30, 10^(-5))
  }

  #colnames(est$theta) <- c("beta.intercept", "beta.slope", "sigma", "prop")
  #est$inilikelihoodseq = lh

  #SK-- 9/4/2023
  npar <- ncol(est$theta)
  out <- list(pi = est$theta[, npar],
              beta = est$theta[, - c(npar - 1, npar)],
              sigma = est$theta[, npar - 1],
              lik = est$lik,
              df = est$df,
              run = est$run)
  #out = list(theta = est$theta, lik = est$lik, df = est$df, run = est$run)
  return(out)
}


#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
bisquare <- function(t, k = 4.685){
  out = t * pmax(0, (1 - (t/k)^2))^2
  return(out)
}
biscalew <- function(t){
  t[which(t == 0)] = min(t[which(t != 0)])/10
  out = pmin(1 - (1 - t^2/1.56^2)^3, 1)/t^2
  return(out)
}

########################### EM algorithm to fit the mixture of linear regression
# mixregone estimates the mixture regression parameters by MLE based on ONE initial value
mixregone <- function(x, y, bet, sig, pr, m = 2, tol = 1e-05) {
  run <- 0
  n <- length(y)
  X <- cbind(rep(1, n), x)

  if (length(sig) > 1) { # the case when the variance is unequal
    r <- matrix(rep(0, m * n), nrow = n)
    pk <- r
    lh <- 0

    for (j in seq(m)) {
      r[, j] <- y - X %*% bet[j, ]
      lh <- lh + pr[j] * stats::dnorm(r[, j], 0, sig[j])
    }

    lh <- sum(log(lh))

    # E-steps
    repeat {
      prest <- c(bet, sig, pr)
      run <- run + 1
      plh <- lh

      for (j in seq(m)) {
        pk[, j] <- pr[j] * pmax(10^(-300), stats::dnorm(r[, j], 0, sig[j]))
      }

      pk <- pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)

      # M-step
      np <- apply(pk, 2, sum)
      pr <- np / n
      lh <- 0

      for (j in seq(m)) {
        w <- diag(pk[, j])
        bet[j, ] <- MASS::ginv(t(X) %*% w %*% X) %*% t(X) %*% w %*% y
        r[, j] <- y - X %*% bet[j, ]
        sig[j] <- sqrt(t(pk[, j]) %*% (r[, j]^2) / np[j])
        lh <- lh + pr[j] * stats::dnorm(r[, j], 0, sig[j])
      }

      lh <- sum(log(lh))
      dif <- lh - plh

      #if (dif < 10^(-5) | run > 500) { SK
      if (dif < tol | run > 500) {
        break
      }
    }
  } else {  # the case when the variance is equal
    r <- matrix(rep(0, m * n), nrow = n)
    pk <- r
    lh <- 0

    for (j in seq(m)) {
      r[, j] <- y - X %*% bet[j, ]
      lh <- lh + pr[j] * stats::dnorm(r[, j], 0, sig)
    }

    lh <- sum(log(lh))

    # E-steps
    repeat {
      prest <- c(bet, sig, pr)
      run <- run + 1
      plh <- lh

      for (j in seq(m)) {
        pk[, j] <- pr[j] * pmax(10^(-300), stats::dnorm(r[, j], 0, sig))
      }

      pk <- pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)

      # M-step
      np <- apply(pk, 2, sum)
      pr <- np / n

      for (j in seq(m)) {
        w <- diag(pk[, j])
        bet[j, ] <- MASS::ginv(t(X) %*% w %*% X) %*% t(X) %*% w %*% y
        r[, j] <- y - X %*% bet[j, ]
      }

      sig <- sqrt(sum(pk * (r^2)) / n)
      lh <- 0

      for (j in seq(m)) {
        lh <- lh + pr[j] * stats::dnorm(r[, j], 0, sig)
      }

      lh <- sum(log(lh))
      dif <- lh - plh

      #if (dif < 10^(-5) | run > 500) { SK
      if (dif < tol | run > 500) {
        break
      }
    }

    sig <- sig * rep(1, m)
  }

  est <- list(theta = matrix(c(bet, sig, pr), nrow = m), lik = lh, run = run)#SK, diflh = dif)
  est
}

########################### Robust EM algorithm to fit the mixture of linear regression based on bisquare function
#' Robust EM algorithm to fit the mixture of linear regression based on bisquare function with one initial value
#'
#' mixregBisqone estimates the mixture regression parameters robustly using bisquare function based on one initial value
#' @param x explanatory variables matrix with one observation per row
#' @param y response variable vector
#' @param bet initial value of beta
#' @param sig initial value of sigma
#' @param pr initial value of proportion
#' @param m number of components, default is 2
#'
#' @return A list containing the following elements:
#'
#'   \item{theta}{estimated parameters matrix, the columns are beta(intercept, slopes), sigma, proportion for each component.}
#'   \item{run}{number of iterations to converge.}
#' @noRd
mixregBisqone <- function(x, y, bet, sig, pr, m = 2) {

  #SK-- Used functions: bisquare, biscalew

  run <- 0
  tol <- 10^(-4) * max(abs(c(bet, sig, pr)))
  n <- length(y)
  X <- cbind(rep(1, n), x)
  p <- dim(X)[2]

  if (length(sig) > 1) {
    r <- matrix(rep(0, m * n), nrow = n)
    pk <- r

    for (j in seq(m))
      r[, j] <- (y - X %*% bet[j, ]) / sig[j]

    # E-steps
    repeat {
      prest <- c(sig, bet, pr)
      run <- run + 1

      for (j in seq(m)) {
        pk[, j] <- pr[j] * pmax(10^(-300), stats::dnorm(r[, j], 0, 1)) / sig[j]
      }

      pk <- pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)

      # M-step
      np <- apply(pk, 2, sum)
      pr <- np / n
      r[which(r == 0)] <- min(r[which(r != 0)]) / 10

      for (j in seq(m)) {
        w <- diag(pk[, j] * bisquare(r[, j]) / r[, j])
        bet[j, ] <- solve(t(X) %*% w %*% X + 10^(-10) * diag(rep(1, p))) %*% t(X) %*% w %*% y
        r[, j] <- (y - X %*% bet[j, ]) / sig[j]
        sig[j] <- sqrt(sum(r[, j]^2 * sig[j]^2 * pk[, j] * biscalew(r[, j])) / np[j] / 0.5)
      }

      dif <- max(abs(c(sig, bet, pr) - prest))

      if (dif < tol | run > 500) {
        break
      }
    }
  } else {
    r <- matrix(rep(0, m * n), nrow = n)
    pk <- r

    for (j in seq(m))
      r[, j] <- (y - X %*% bet[j, ]) / sig

    # E-steps
    repeat {
      prest <- c(sig, bet, pr)
      run <- run + 1

      for (j in seq(m)) {
        pk[, j] <- pr[j] * pmax(10^(-300), stats::dnorm(r[, j], 0, 1)) / sig
      }

      pk <- pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)

      # M-step
      np <- apply(pk, 2, sum)
      pr <- np / n
      r[which(r == 0)] <- min(r[which(r != 0)]) / 10

      for (j in seq(m)) {
        w <- diag(pk[, j] * bisquare(r[, j]) / r[, j])
        bet[j, ] <- solve(t(X) %*% w %*% X + 10^(-10) * diag(rep(1, p))) %*% t(X) %*% w %*% y
        r[, j] <- (y - X %*% bet[j, ]) / sig
      }

      sig <- sqrt(sum(pk * (r^2 * sig[1]^2) * biscalew(r)) / n / 0.5)
      dif <- max(abs(c(sig, bet, pr) - prest))

      if (dif < tol | run > 500) {
        break
      }
    }

    sig <- rep(sig, m)
  }

  theta <- matrix(c(bet, sig, pr), nrow = m)
  est <- list(theta = theta, difpar = dif, run = run)
  est
}

########################### Trimmed likelihood estimator
# mixregTrimone uses the trimmed likelihood estimator based on ONE initial value.
mixregTrimone <- function(x, y, k = 2, keep = 0.95, bet, sig, pr) {

  #SK-- Used functions: mixregone

  n <- length(y)
  n1 <- round(n * keep)
  x <- matrix(x, nrow = n)
  a <- dim(x)
  p <- a[2] + 1
  X <- cbind(rep(1, n), x)

  if (dim(bet)[2] == k) {
    bet <- t(bet)
  }

  lh <- 0

  for (i in seq(k)) {
    lh <- lh + pr[i] * stats::dnorm(y - X %*% bet[i, ], 0, sig[1])
  }

  ind <- order(-lh)
  run <- 0
  tol <- 10^(-4)
  obj <- sum(log(lh[ind[1:n1]]))

  repeat {
    pobj <- obj
    run <- run + 1
    x1 <- x[ind[1:n1], ]
    y1 <- y[ind[1:n1]]
    fit <- mixregone(x1, y1, bet, sig, pr, k)
    fit <- fit$theta
    bet <- matrix(fit[1:(p * k)], nrow = k)
    sig <- fit[p * k + 1]
    pr <- fit[(p * k + k + 1):(p * k + 2 * k)]
    lh <- 0

    for (i in seq(k)) {
      lh <- lh + pr[i] * stats::dnorm(y - X %*% bet[i, ], 0, sig[1])
    }

    ind <- order(-lh)
    obj <- sum(log(lh[ind[1:n1]]))
    dif <- obj - pobj

    if (dif < tol | run > 50) {
      break
    }
  }

  if (length(sig) < 2) {
    sig <- rep(sig, k)
  }

  theta <- matrix(c(bet, sig, pr), nrow = k)
  # theta=matrix(c(bet,sig,pr),nrow=m)  # m is not working, updated with k by Xin Shen
  est <- list(theta = theta, lik = obj, diflikelihood = dif, run = run)
  est
}

########################### Robust EM algorithm to fit the mixture of linear regression using t-distribution
# Definition of t density
dent <- function(y, mu, sig, v) {
  est = gamma((v + 1) / 2) * sig^(-1) / ((pi * v)^(1 / 2) * gamma(v / 2) * (1 + (y - mu)^2 / (sig^2 * v))^(0.5 * (v + 1)))
  est
}

# mixregT estimates the mixture regression parameters robustly assuming the error distribution is t-distribution
mixregTonev <- function(x, y, bet, sig, pr, v = 3, m = 2, tol = 10^(-5)) {

  #SK-- Used functions: dent

  run = 0
  n = length(y)
  if (length(v) == 1) {
    v = rep(v, m)
  }
  X = cbind(rep(1, n), x)
  lh = -10^10
  a = dim(x)
  p = a[2] + 1
  r = matrix(rep(0, m * n), nrow = n)
  pk = r
  u = r
  logu = r

  if (length(sig) > 1) {  # the component variances are different
    for (j in seq(m)) {
      r[, j] = (y - X %*% bet[j, ]) / sig[j]
    }

    # E-steps
    repeat {
      run = run + 1
      prelh = lh

      for (j in seq(m)) {
        pk[, j] = pr[j] * pmax(10^(-300), dent(r[, j] * sig[j], 0, sig[j], v[j]))
        u[, j] = (v[j] + 1) / (v[j] + r[, j]^2)
        # logu[, j] = log(u[, j]) + (digamma((v[j] + 1) / 2) - log((v[j] + 1) / 2))
      }

      lh = sum(log(apply(pk, 1, sum)))
      pk = pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)
      dif = lh - prelh

      if (dif < tol | run > 500) {
        break
      }

      # M-step
      np = apply(pk, 2, sum)
      pr = np / n

      for (j in seq(m)) {
        w = diag(pk[, j] * u[, j])
        bet[j, ] = MASS::ginv(t(X) %*% w %*% X) %*% t(X) %*% w %*% y
        sig[j] = sqrt(sum((y - X %*% bet[j, ])^2 * pk[, j] * u[, j]) / sum(pk[, j]))
        r[, j] = (y - X %*% bet[j, ]) / sig[j]
      }
    }
  } else {
    for (j in seq(m)) {
      r[, j] = (y - X %*% bet[j, ]) / sig
    }

    # E-steps
    repeat {
      run = run + 1
      prelh = lh

      for (j in seq(m)) {
        pk[, j] = pr[j] * dent(y, X %*% bet[j, ], sig, v[j])
        u[, j] = (v[j] + 1) / (v[j] + r[, j]^2)
        # logu[, j] = log(u[, j]) + (digamma((v[j] + 1) / 2) - log((v[j] + 1) / 2))
      }

      lh = sum(log(apply(pk, 1, sum)))
      pk = pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)
      dif = lh - prelh

      if (dif < tol | run > 500) {
        break
      }

      # M-step
      np = apply(pk, 2, sum)
      pr = np / n

      for (j in seq(m)) {
        w = diag(pk[, j] * u[, j])
        bet[j, ] = MASS::ginv(t(X) %*% w %*% X) %*% t(X) %*% w %*% y
        r[, j] = y - X %*% bet[j, ]
      }

      sig = sqrt(sum(pk * r^2 * u) / sum(pk))
      r = r / sig
    }

    sig = sig * rep(1, m)
  }

  theta = matrix(c(bet, sig, pr), nrow = m)
  est = list(theta = theta, lik = lh, df = v, dif = dif, run = run)
  est
}

#' Robust Mixture Regression Based on T-distribution
#' mixregTv adaptively estimates the mixture regression parameters robustly assuming the error distribution is t-distribution
#' @param x explanatory variables matrix with rows for each observation
#' @param y response variable vector
#' @param bet initial values for beta
#' @param sig initial value for sigma; if the component variance is different then use a vector, otherwise a scaler would be fine.
#' @param pr initial value of proportion for each component, vector
#' @param m number of component, default is 2
#' @param maxdf maximum degree of freedom, default is 15
#' @param tol stopping criteria, default is 10^(-5)
#'
#'
#' @noRd
#'
# mixregTv estimates the mixture regression parameters robustly assuming the error distribution is t-distribution with varying degrees of freedom
mixregTv <- function(x, y, bet, sig, pr, m = 2, maxdf = 15, tol = 10^(-5)) {

  #SK-- Used functions: mixregone, mixregTonev

  # theta: estimated parameters beta(intercept, slopes), sigma, proportion for each component
  # likelihood: likelihood of the estimated parameter
  # dif: difference of likelihood of the last iteration and second to last iteration
  # run: number of iterations to converge
  # df: estimated degree of the t distribution
  # degreerange: all degrees of freedom used to estimate the parameters
  # vlikelihoodseq: all likelihoods associated with degrees of freedom

  est = mixregone(x, y, bet, sig, pr, m)
  fv = 0
  a = dim(x)
  p = a[2] + 1
  lh = rep(0, maxdf)
  lh[1] = est$lik

  for (v in 1:maxdf) {
    temp = mixregTonev(x, y, bet, sig, pr, v, m, tol)
    lh[v + 1] = temp$lik

    if (lh[v + 1] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])) {
      est = temp
      fv = v
    }
  }

  est = list(theta = est$theta, lik = est$lik, dif = est$dif, run = est$run)
  est$df = fv
  est$degreerange = c(1, maxdf)
  est$vlikelihoodseq = lh
  est
}


# Function to generate permutations
permutations <- function(n, r, v = 1:n) {
  if (r == 1) {
    matrix(v, n, 1)
  } else if (n == 1) {
    matrix(v, 1, r)
  } else {
    X <- NULL

    for (i in 1:n) {
      X <- rbind(X, cbind(v[i], permutations(n - 1, r - 1, v[-i])))
    }

    X
  }
}

###########################
hard_penalty <- function(gamma, lambda) {
  ((lambda^2) / 2) * (gamma != 0)
}

hard_ipod <- function(theta, lambda) {
  theta * (abs(theta) > lambda)
}

hard_plh <- function(y, X, pi, beta, sigma, gamma, lambda, m = 2) {
  lh = 0
  for (r in seq(m)) {
    lh = lh + pi[r, ] * pmax(10^(-300), stats::dnorm(y - gamma * sigma[r, ] - X %*% beta[, r], mean = 0, sd = sigma[r, ]))
  }
  ##the objective function =loglikehood-hard penalty
  sum(log(lh)) - sum(hard_penalty(gamma = gamma, lambda = lambda))
}

soft_penalty <- function(gamma, lambda) {
  abs(gamma) * lambda
}

soft_ipod <- function(theta, lambda) {
  a = abs(theta) - lambda
  ifelse(a < 0, 0, sign(theta) * a)
}

soft_plh <- function(y, X, pi, beta, sigma, gamma, lambda, m = 2) {
  lh = 0
  for (r in seq(m)) {
    lh = lh + pi[r, ] * pmax(10^(-300), stats::dnorm(y - gamma * sigma[r, ] - X %*% beta[, r], mean = 0, sd = sigma[r, ]))
  }
  ## the objective function = loglikehood - hard penalty
  sum(log(lh)) - sum(soft_penalty(gamma = gamma, lambda = lambda))
}

## objective function for sigma estimation
sigmaf <- function(sig, y, X, p_ij, beta, gamma, m) {
  func = 0
  for (r in seq(m)) {
    func = func + 0.5 * sum(p_ij[, r] * log(sig^2)) + (t(p_ij[, r]) %*% (y - gamma * sig - X %*% beta[, r])^2) / (2 * sig^2)
  }
  func
}


#-------------------------------------------------------------------------------
# Keep unused functions (SK, 08/31/2023)
#-------------------------------------------------------------------------------
#library(gregmisc)
#library(robustbase)
huberpsi <- function(t, k = 1.345){
  out = pmax(- k, pmin(k, t))
  return(out)
}

mixregTw <- function(x, y, m = 2, maxdf = 10, nstart = 20, tol = 10^(-5)) {

  #SK-- Used functions: mixregT

  n = length(y)
  x = matrix(x, nrow = n)
  w = robustbase::covMcd(x)
  w = w$mcd.wt
  x = x[w == 1, ]
  y = y[w == 1]
  est = mixregT(x, y, m, maxdf, nstart, tol)
  est
}

########################### mixregt with unknown degree of freedoms
#%%start from one initial value
# mixregTonev1 estimates the mixture regression parameters robustly assuming the error distribution is t-distribution with varying degrees of freedom
mixregTonev1 <- function(x, y, bet, sig, pr, m = 2, v = 3, maxdf = 30, tol = 10^(-5)) {

  #SK-- Used functions: dent

  run = 0
  n = length(y)
  if (length(v) == 1) {
    v = rep(v, m)
  }
  X = cbind(rep(1, n), x)
  lh = -10^10
  a = dim(x)
  p = a[2] + 1
  r = matrix(rep(0, m * n), nrow = n)
  pk = r
  u = r
  logu = r

  if (length(sig) > 1) {
    for (j in seq(m))
      r[, j] = (y - X %*% bet[j, ]) / sig[j]

    # E-steps
    repeat {
      # prest=c(sig,bet,pr)
      run = run + 1
      prelh = lh

      for (j in seq(m)) {
        pk[, j] = pr[j] * pmax(10^(-300), dent(r[, j] * sig[j], 0, sig[j], v[j]))
        u[, j] = (v[j] + 1) / (v[j] + r[, j]^2)
        # logu[, j] = log(u[, j]) + (digamma((v[j] + 1) / 2) - log((v[j] + 1) / 2))
      }

      lh = sum(log(apply(pk, 1, sum)))
      pk = pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)
      dif = lh - prelh

      if (dif < tol | run > 500) {
        break
      }

      # M-step
      np = apply(pk, 2, sum)
      pr = np / n

      for (j in seq(m)) {
        w = diag(pk[, j] * u[, j])
        bet[j, ] = MASS::ginv(t(X) %*% w %*% X) %*% t(X) %*% w %*% y
        sig[j] = sqrt(sum((y - X %*% bet[j, ])^2 * pk[, j] * u[, j]) / sum(pk[, j]))
        r[, j] = (y - X %*% bet[j, ]) / sig[j]

        q = rep(0, maxdf)

        for (k in seq(maxdf)) {
          q[k] = -log(gamma(0.5 * k)) + 0.5 * k * log(0.5 * k) + 0.5 * k * (sum((log(u[, j]) - u[, j]) * pk[, j]) / np[j] + digamma(0.5 * (v[j] + 1)) - log(0.5 * (v[j] + 1)))
        }

        ordq = order(-q)
        v[j] = ordq[1]
      }
    }
  } else {
    for (j in seq(m))
      r[, j] = (y - X %*% bet[j, ]) / sig

    # E-steps
    repeat {
      run = run + 1
      prelh = lh

      for (j in seq(m)) {
        pk[, j] = pr[j] * dent(y, X %*% bet[j, ], sig, v[j])
        u[, j] = (v[j] + 1) / (v[j] + r[, j]^2)
      }

      lh = sum(log(apply(pk, 1, sum)))
      pk = pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)
      dif = lh - prelh

      if (dif < tol | run > 500) {
        break
      }

      # M-step
      np = apply(pk, 2, sum)
      pr = np / n

      for (j in seq(m)) {
        w = diag(pk[, j] * u[, j])
        bet[j, ] = MASS::ginv(t(X) %*% w %*% X) %*% t(X) %*% w %*% y
        r[, j] = (y - X %*% bet[j, ]) / sig

        q = rep(0, maxdf)

        for (k in seq(maxdf)) {
          q[k] = -log(gamma(0.5 * k)) + 0.5 * k * log(0.5 * k) + 0.5 * k * (sum((log(u[, j]) - u[, j]) * pk[, j]) / np[j] + digamma(0.5 * (v[j] + 1)) - log(0.5 * (v[j] + 1)))
        }

        ordq = order(-q)
        v[j] = ordq[1]
      }

      sig = sqrt(sum(pk * r^2 * u) / sum(pk))
      r = r / sig
    }

    sig = sig * rep(1, m)
  }

  theta = matrix(c(bet, sig, pr), nrow = m)
  est = list(theta = theta, df = v, lik = lh, degreerange = c(1, maxdf), run = run, dif = dif)
  est
}

# Start from multiple degrees
mixregTone <- function(x, y, bet, sig, pr, m = 2, maxdf = 30, tol = 10^(-5), numdeg = 100) {

  #SK-- Used functions: mixregTonev1, mixregTonev

  lh = rep(0, numdeg)
  u = stats::runif(m, 0, 1)
  u = pmax(1, round(maxdf * u))
  est = mixregTonev1(x, y, bet, sig, pr, m, u, maxdf, tol)
  lh[1] = est$lik
  a = dim(x)
  p = a[2] + 1

  for (v in seq(numdeg)) {
    u = stats::runif(m, 0, 1)
    u = pmax(1, round(maxdf * u))
    temp = mixregTonev1(x, y, bet, sig, pr, m, u, maxdf, tol)
    lh[v + 1] = temp$lik

    if (lh[v + 1] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])) {
      est = temp
    }
  }

  if (max(est$df) == maxdf & maxdf < 30) {
    est$df[est$df == maxdf] = 30
    temp = mixregTonev(x, y, bet, sig, pr, est$df, m, tol)
    lh[v + 2] = temp$lik

    if (lh[v + 2] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])) {
      est = temp
    }
  }

  est$vlikelihoodseq = lh
  est
}

# Start from multiple initial values
mixregTueqv <- function(x, y, m = 2, maxdf = 30, nstart = 20, tol = 10^(-5)) {

  #SK-- Used functions: mixregTone

  n = length(y)
  x = matrix(x, nrow = n)
  a = dim(x)
  p = a[2] + 1
  n1 = 2 * p
  bet = matrix(rep(0, m * p), nrow = m)
  sig = 0

  for (j in seq(m)) {
    ind = sample(1:n, n1)
    X = cbind(rep(1, n1), x[ind, ])
    bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
    sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
  }

  pr = rep(1/m, m)
  sig = sig / n1 / m
  lh = rep(0, nstart)
  est = mixregTone(x, y, bet, sig, pr, m, maxdf, tol)
  lh[1] = est$lik

  for (i in seq(nstart - 1)) {
    sig = 0

    for (j in seq(m)) {
      ind = sample(1:n, n1)
      X = cbind(rep(1, n1), x[ind, ])
      bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
      sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
    }

    pr = rep(1/m, m)
    sig = sig / n1 / m
    temp = mixregTone(x, y, bet, sig, pr, m, maxdf, tol)
    lh[i + 1] = temp$lik

    if (lh[i + 1] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])) {
      est = temp
    }
  }

  if (maxdf < 30 | tol > 10^(-5)) {
    bet = est$theta[, 1:p]
    sig = est$theta[, p + 1]
    pr = est$theta[, p + 2]
    est = mixregTone(x, y, bet, sig[1], pr, m, 30, 10^(-5))
  }

  est$inilikelihoodseq = lh
  est
}

mixregTwueqv <- function(x, y, m = 2, maxdf = 30, nstart = 20, tol = 10^(-5)) {

  #SK-- Used functions: mixregTueqv

  n = length(y)
  x = matrix(x, nrow = n)
  w = robustbase::covMcd(x)
  w = w$mcd.wt
  x = x[w == 1, ]
  y = y[w == 1]
  est = mixregTueqv(x, y, m, maxdf, nstart, tol)
  est
}

# mixregT adaptively estimates the mixture regression parameters robustly assuming the error distribution is
# t-distribution
mixregTv1 <- function(x, y, m = 2, v, nstart = 20, tol = 10^(-5)) {

  #SK-- Used functions: mixregTonev

  n = length(y)
  x = matrix(x, nrow = n)
  a = dim(x)
  p = a[2] + 1
  n1 = 2 * p
  bet = matrix(rep(0, m * p), nrow = m)
  sig = 0

  for (j in seq(m)) {
    ind = sample(1:n, n1)
    X = cbind(rep(1, n1), x[ind, ])
    bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
    sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
  }

  pr = rep(1/m, m)
  sig = sig / n1 / m
  lh = rep(0, nstart)
  est = mixregTonev(x, y, bet, sig, pr, v, m, tol)
  lh[1] = est$lik

  for (i in seq(nstart - 1)) {
    sig = 0

    for (j in seq(m)) {
      ind = sample(1:n, n1)
      X = cbind(rep(1, n1), x[ind, ])
      bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
      sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
    }

    pr = rep(1/m, m)
    sig = sig / n1 / m
    temp = mixregTonev(x, y, bet, sig, pr, v, m, tol)
    lh[i + 1] = temp$lik

    if (lh[i + 1] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])) {
      est = temp
    }
  }

  if (tol > 10^(-5)) {
    bet = est$theta[, 1:p]
    sig = est$theta[, p + 1]
    pr = est$theta[, p + 2]
    est = mixregTonev(x, y, bet, sig[1], pr, v, m, 10^(-5))
  }

  est$inilikelihoodseq = lh
  est
}

mixregTueqv1 <- function(x, y, m = 2, v, nstart = 20, tol = 10^(-5)) {

  #SK-- Used functions: mixregTonev1

  n = length(y)
  x = matrix(x, nrow = n)
  a = dim(x)
  p = a[2] + 1
  n1 = 2 * p
  bet = matrix(rep(0, m * p), nrow = m)
  sig = 0

  for (j in seq(m)) {
    ind = sample(1:n, n1)
    X = cbind(rep(1, n1), x[ind, ])
    bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
    sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
  }

  pr = rep(1/m, m)
  sig = sig / n1 / m
  lh = rep(0, nstart)
  est = mixregTonev1(x, y, bet, sig, pr, m, v, tol)
  lh[1] = est$lik

  for (i in seq(nstart - 1)) {
    sig = 0

    for (j in seq(m)) {
      ind = sample(1:n, n1)
      X = cbind(rep(1, n1), x[ind, ])
      bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
      sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
    }

    pr = rep(1/m, m)
    sig = sig / n1 / m
    temp = mixregTonev1(x, y, bet, sig, pr, m, v, tol)
    lh[i + 1] = temp$lik

    if (lh[i + 1] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])) {
      est = temp
    }
  }

  if (tol > 10^(-5)) {
    bet = est$theta[, 1:p]
    sig = est$theta[, p + 1]
    pr = est$theta[, p + 2]
    est = mixregTonev1(x, y, bet, sig[1], pr, m, v, 10^(-5))
  }

  est$inilikelihoodseq = lh
  est
}
