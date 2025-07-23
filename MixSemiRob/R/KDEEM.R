#' Kernel Density-based EM-type algorithm with Least Square Estimation
#' for Semiparametric Mixture Regression with Unspecified Homogenous
#' Error Distributions
#'
#' `kdeem.lse' is used for semiparametric mixture regression based on least squares estimation (Hunter and Young, 2012)
#' using a kernel density-based expectation-maximization (EM)-type algorithm with unspecified
#' homogeneous error distributions.
#'
#' @usage
#' kdeem.lse(x, y, C = 2, ini = NULL)
#'
#' @details
#'   As of version 1.1.0, this function can only be used for a two-component mixture-of-regressions model
#'   with independent identically distributed errors. Assuming \eqn{C=2}, the model is defined as follows:
#'   \deqn{f_{Y|\boldsymbol{X}}(y,\boldsymbol{x},\boldsymbol{\theta},g) = \sum_{j=1}^C\pi_jg(y-\boldsymbol{x}^{\top}\boldsymbol{\beta}_j).}
#'   Here, \eqn{\boldsymbol{\theta}=(\pi_1,...,\pi_{C-1},\boldsymbol{\beta}_1^{\top},\cdots,\boldsymbol{\beta}_C^{\top})},
#'   and \eqn{g(\cdot)} represents identical unspecified density functions.
#'   The bandwidth of the kernel density estimation is calculated adaptively using the \code{\link[stats]{bw.SJ}} function from the `stats'
#'   package, which implements the method of Sheather & Jones (1991) for bandwidth selection based on pilot estimation
#'   of derivatives. This function employs weighted least square estimation for \eqn{\beta} in the M-step (Hunter and Young, 2012),
#'   where the weight is the posterior probability of an observation belonging to each component.
#'
#' @param x an n by p data matrix where n is the number of observations and p is the
#'   number of explanatory variables (including the intercept).
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. As of version 1.1.0, C must be set to 2.
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values
#'   using the \code{\link[mixtools]{regmixEM}} function from the `mixtools' package.
#'   If specified, it can be a list with the form of \code{list(beta, prop, tau, pi, h)}, where
#'   \code{beta} is a p by C matrix for regression coefficients of C components,
#'   \code{prop} is an n by C matrix for probabilities of each observation belonging to each component,
#'   caculated based on the initial \code{beta} and \code{h}, \code{tau} is a vector of C precision parameters
#'   (inverse of standard deviation), \code{pi} is a vector of C mixing proportions, and
#'   \code{h} is the bandwidth for kernel estimation.
#'
#' @return A list containing the following elements:
#'   \item{posterior}{posterior probabilities of each observation belonging to each component.}
#'   \item{beta}{estimated regression coefficients.}
#'   \item{tau}{estimated precision parameter, the inverse of standard deviation.}
#'   \item{pi}{estimated mixing proportions.}
#'   \item{h}{bandwidth used for the kernel estimation.}
#'
#' @seealso \code{\link{kdeem}}, \code{\link{kdeem.h}}, \code{\link[stats]{bw.SJ}}
#'   for bandwidth calculation, and \code{\link[mixtools]{regmixEM}} for initial value calculation.
#'
#' @references
#'   Hunter, D. R., and Young, D. S. (2012). Semiparametric mixtures of regressions.
#'   Journal of Nonparametric Statistics, 24(1), 19-38.
#'
#'   Ma, Y., Wang, S., Xu, L., and Yao, W. (2021). Semiparametric mixture regression
#'   with unspecified error distributions. Test, 30, 429-444.
#'
#' @export
#' @examples
#' # See examples for the `kdeem' function.
kdeem.lse <- function(x, y, C = 2, ini = NULL){

  # Warning for inappropriate settings
  if(C != 2){
    stop("This function currently only supports the case when C = 2")
  }

  M = C

  if (is.null(ini)){
    ini = mixtools::regmixEM(y, x, arbmean = TRUE, arbvar = TRUE, epsilon = 1e-4)
  }
  n = length(y)
  X = cbind(1, x)
  Dimen = dim(X)[2]
  beta.matrix.new = ini$beta
  beta.matrix.old = matrix(0, Dimen, M)
  P = ini$posterior
  PI = colMeans(P)

  Eps.matrix = matrix(0, n, M)
  for(j in 1:M){
    Eps.matrix[, j] = y - X %*% beta.matrix.new[, j]
  }

  ############################################################################
  # use EM algorithm to calculate the mle
  numiter = 0
  GR.matrix = matrix(0, n, M)

  while(norm(beta.matrix.new - beta.matrix.old, '2') > 1e-2){

    beta.matrix.old = beta.matrix.new

    Tao = rep(0, M)
    R.matrix = matrix(0, n, M)
    for(k in 1:M){
      Tao[k] = (sum(P[, k] * (Eps.matrix[, k]^2))/sum(P[, k]))^(- 0.5)
      R.matrix[, k] = Eps.matrix[, k] * Tao[k]
    }
    ###############################################
    Residual.1 = R.matrix[, 1][which(P[, 1] >= P[, 2])]
    Residual.2 = R.matrix[, 2][which(P[, 1] <= P[, 2])]
    H = stats::bw.SJ(c(Residual.1, Residual.2))
    ###############################################

    for(j in 1:M){
      for(i in 1:n){
        ###############################
        GR.matrix[i, j]=(t(P[,1]) %*% stats::dnorm((R.matrix[, 1] - R.matrix[i, j])/H)/H +
                           t(P[, 2]) %*% stats::dnorm((R.matrix[, 2] - R.matrix[i, j])/H)/H)/n
        ###############################
      }
    }

    for(j in 1:M){
      PI[j] = mean(P[, j])
      P[, j] = (PI[j] * GR.matrix[, j] * Tao[j])/(PI[1] * GR.matrix[, 1] * Tao[1] + PI[2] * GR.matrix[, 2] * Tao[2]) # need to be updated to a more general case
      # P[,j]=(PI[j]*GR.matrix[,j]*Tao[j])/rowSums(PI*GR.matrix*Tao);
      Wj = diag(P[, j])
      beta.matrix.new[, j] = solve(t(X) %*% Wj %*% X) %*% t(X) %*% Wj %*% y
      Eps.matrix[, j] = y - X %*% beta.matrix.new[, j]
    }
    numiter = numiter + 1
    if(numiter > 200){
      break
    }
  } # End of the loop while.
  ############################################################################
  return(list(posterior = P, beta = beta.matrix.new, tau = Tao, pi = PI, h = H))
}

#' Kernel Density-based EM-type algorithm for Semiparametric Mixture Regression
#' with Unspecified Homogenous Error Distributions
#'
#' `kdeem.h' is used for semiparametric mixture regression using a kernel density-based
#' expectation-maximization (EM)-type algorithm with unspecified
#' homogeneous error distributions (Hunter and Young, 2012).
#'
#' @usage
#' kdeem.h(x, y, C = 2, ini = NULL, maxiter = 200)
#'
#' @details
#'   'kdeem.h' can be used to estimate parameters in a mixture-of-regressions model
#'   with independent identically distributed errors. The model is defined as follows:
#'   \deqn{f_{Y|\boldsymbol{X}}(y,\boldsymbol{x},\boldsymbol{\theta},g) = \sum_{j=1}^C\pi_jg(y-\boldsymbol{x}^{\top}\boldsymbol{\beta}_j).}
#'   Here, \eqn{\boldsymbol{\theta}=(\pi_1,...,\pi_{C-1},\boldsymbol{\beta}_1^{\top},\cdots,\boldsymbol{\beta}_C^{\top})},
#'   and \eqn{g(\cdot)} represents identical unspecified density functions.
#'   The bandwidth of the kernel density estimation is calculated adaptively using the \code{\link[stats]{bw.SJ}} function from the `stats'
#'   package, which implements the method of Sheather & Jones (1991) for bandwidth selection based on pilot estimation
#'   of derivatives.
#'
#'   For the calculation of \eqn{\beta} in the M-step,
#'   this function employs the universal optimizer \code{\link[ucminf]{ucminf}} from the `ucminf' package.
#'
#' @param x an n by p data matrix where n is the number of observations and p is the
#'   number of explanatory variables (including the intercept).
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values
#'   using the \code{\link{kdeem.lse}} function.
#'   If specified, it can be a list with the form of \code{list(beta, prop, tau, pi, h)}, where
#'   \code{beta} is a p by C matrix for regression coefficients of C components,
#'   \code{prop} is an n by C matrix for probabilities of each observation belonging to each component,
#'   calculated based on the initial \code{beta} and \code{h}, \code{tau} is a vector of C precision parameters
#'   (inverse of standard deviation), \code{pi} is a vector of C mixing proportions, and
#'   \code{h} is the bandwidth for kernel estimation.
#' @param maxiter maximum number of iterations for the algorithm. Default is 200.
#'
#'@return A list containing the following elements:
#'   \item{posterior}{posterior probabilities of each observation belonging to each component.}
#'   \item{beta}{estimated regression coefficients.}
#'   \item{pi}{estimated mixing proportions.}
#'   \item{h}{bandwidth used for the kernel estimation.}
#'
#' @seealso \code{\link{kdeem}}, \code{\link{kdeem.lse}}, \code{\link[stats]{bw.SJ}}
#'   for bandwidth calculation, and \code{\link[ucminf]{ucminf}} for beta calculation.
#'
#' @references
#'   Hunter, D. R., & Young, D. S. (2012). Semiparametric mixtures of regressions.
#'   Journal of Nonparametric Statistics, 24(1), 19-38.
#'
#'   Ma, Y., Wang, S., Xu, L., & Yao, W. (2021). Semiparametric mixture regression
#'   with unspecified error distributions. Test, 30, 429-444.
#'
#' @export
#' @examples
#' # See examples for the `kdeem' function.
kdeem.h <- function(x, y, C = 2, ini = NULL, maxiter = 200){

  M = C

  if(is.null(ini)){
    #SK-- 9/4/2023
    ini = kdeem.lse(x = x, y = y, C = C)
    #ini = mixtools::regmixEM(y, x, arbmean = TRUE, arbvar = TRUE, epsilon = 1e-4)
  }
  n = length(y)
  X = cbind(1, x)
  Dimen = dim(X)[2]
  #if(inherits(ini, "mixEM")){ # 08/28/2023 by SK
    beta.matrix.new = ini$beta
  #} else {
  #  beta.matrix.new = ini$Beta
  #}
  beta.matrix.old = matrix(0, Dimen, M)
  P = ini$posterior
  H = ini$h

  Eps.matrix = matrix(0, n, M) # residual matrix
  PI = c()
  for(k in 1:M){
    PI[k] = mean(P[, k])
    Eps.matrix[, k] = y - X %*% beta.matrix.new[, k]
  }

  numiter = 0
  while(norm(beta.matrix.new - beta.matrix.old, 'f') > 1e-2 & numiter < maxiter){

    numiter = numiter + 1
    beta.matrix.old = beta.matrix.new

    grid = list()
    for(j in 1:M){
      grid[[j]] = seq(min(Eps.matrix[, j]) - 3 * H, max(Eps.matrix[, j]) + 3 * H, by = H/10)
    }
    ###############################OK
    gfungrid = list()
    for(j in 1:M){
      g.vector = vector()
      for(i in 1:length(grid[[j]])){
        g.vector[i] = (t(P[, j]) %*% stats::dnorm((Eps.matrix[, j] - grid[[j]][i])/H)/H)/sum(P[, j])
      }
      gfungrid[[j]] = g.vector
    }
    ###############################

    # M-step
    for(j in 1:M){
      estg <- function(x){
        out = stats::approx(grid[[j]], gfungrid[[j]], x)$y
        out[x > max(grid[[j]])] = 0
        out[x < min(grid[[j]])] = 0
        return(out)
      }
      fn <- function(bet){
        out = - P[, j] %*% log(estg((y - X %*% bet)))
      }

      beta.matrix.new[, j] = ucminf::ucminf(beta.matrix.new[, j], fn)$par
      Eps.matrix[, j] = y - X %*% beta.matrix.new[, j]
      P[, j] = PI[j] * estg(Eps.matrix[, j])
    }
    P = P/matrix(rep(rowSums(P), M), ncol = M)
    PI = colMeans(P)
  }
  # print(beta.matrix.new)
  # print(numiter)
  ############################################################################
  return(list(posterior = P, beta = beta.matrix.new, pi = PI, h = H))
}

#' Kernel Density-based EM-type algorithm for Semiparametric Mixture Regression
#' with Unspecified Error Distributions
#'
#' `kdeem' is used for semiparametric mixture regression using a kernel density-based
#' expectation-maximization (EM)-type algorithm with unspecified
#' homogeneous or heterogenous error distributions (Ma et al., 2012).
#'
#' @usage
#' kdeem(x, y, C = 2, ini = NULL, maxiter = 200)
#'
#' @details
#'   It can be used for a semiparametric mixture of linear regression models with
#'   unspecified component error distributions. The errors can be either homogeneous or heterogenous.
#'   The model is as follows:
#'   \deqn{f_{Y|\boldsymbol{X}}(y,\boldsymbol{x},\boldsymbol{\theta},g) = \sum_{j=1}^C\pi_j\tau_jg\{(y-\boldsymbol{x}^{\top}\boldsymbol{\beta}_j)\tau_j\}.}
#'   Here, \eqn{\boldsymbol{\theta}=(\pi_1,...,\pi_{C-1},\boldsymbol{\beta}_1^{\top},..,\boldsymbol{\beta}_C^{\top},\tau_1,...,\tau_C)^{\top}},
#'   \eqn{g(\cdot)} is an unspecified density function with mean 0 and variance 1, and \eqn{\tau_j} is a precision parameter.
#'   For the calculation of \eqn{\beta} in the M-step, this function employs the universal optimizer function \code{\link{ucminf}} from the `ucminf' package.
#'
#' @param x an n by p data matrix where n is the number of observations and p is the
#'   number of explanatory variables (including the intercept).
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values
#'   using the \code{\link{kdeem.lse}} function.
#'   If specified, it can be a list with the form of \code{list(beta, prop, tau, pi, h)}, where
#'   \code{beta} is a p by C matrix for regression coefficients of C components,
#'   \code{prop} is an n by C matrix for probabilities of each observation belonging to each component,
#'   caculated based on the initial \code{beta} and \code{h}, \code{tau} is a vector of C precision parameters
#'   (inverse of standard deviation), \code{pi} is a vector of C mixing proportions, and
#'   \code{h} is the bandwidth for kernel estimation.
#' @param maxiter maximum number of iterations for the algorithm. Default is 200.
#'
#' @return A list containing the following elements:
#'   \item{posterior}{posterior probabilities of each observation belonging to each component.}
#'   \item{beta}{estimated regression coefficients.}
#'   \item{tau}{estimated precision parameters, the inverse of standard deviation.}
#'   \item{pi}{estimated mixing proportions.}
#'   \item{h}{bandwidth used for the kernel estimation.}
#'
#' @seealso \code{\link{kdeem.h}}, \code{\link{kdeem.lse}}, and \code{\link[ucminf]{ucminf}} for beta calculation.
#'
#' @references
#'   Ma, Y., Wang, S., Xu, L., & Yao, W. (2021). Semiparametric mixture regression
#'   with unspecified error distributions. Test, 30, 429-444.
#'
#' @importFrom Rlab rbern
#'
#' @export
#' @examples
#  # generate data
#' n = 300
#' C = 2
#' Dimen = 2
#' Beta.true.matrix = matrix(c(-3, 3, 3, -3), Dimen, C)
#' PI.true = c(0.5, 0.5)
#' x = runif(n)
#' X = cbind(1, x)
#' Group.ID = Rlab::rbern(n, prob = 0.5)
#' Error = rnorm(n, 0, 1)
#' n1 = sum(Group.ID)
#' n2 = n - n1
#' y = rep(0, n)
#' err = rep(0, n)
#'
#' for(i in 1:n){
#'   if(Group.ID[i] == 1){
#'     err[i] = Error[i]
#'     y[i] = X[i, ] %*% Beta.true.matrix[, 1] + err[i]
#'   } else {
#'     err[i] = 0.5 * Error[i]
#'     y[i] = X[i, ] %*% Beta.true.matrix[, 2] + err[i]
#'   }
#' }
#' Result.kdeem.lse = kdeem.lse(x, y)
#' \donttest{Result.kdeem.h = kdeem.h(x, y, 2, Result.kdeem.lse, maxiter = 200)}
#' \donttest{Result.kdeem = kdeem(x, y, 2, Result.kdeem.lse, maxiter = 200)}
kdeem <- function(x, y, C = 2, ini = NULL, maxiter = 200){
  M = C


  if(is.null(ini)){
    #SK-- 9/4/2023
    ini = kdeem.lse(x = x, y = y, C = C)
    #ini = mixtools::regmixEM(y, x, arbmean = TRUE, arbvar = TRUE, epsilon = 1e-4)
  }

  n = length(y)
  X = cbind(1, x)
  Dimen = dim(X)[2]
  #if(inherits(ini, "mixEM")){ # 08/28/2023 by SK
    beta.matrix.new = ini$beta
  #} else {
  #  beta.matrix.new = ini$Beta
  #}
  beta.matrix.old = matrix(0, Dimen, M)
  P = ini$posterior
  H = ini$h
  Tao = ini$tau

  Eps.matrix = matrix(0, n, M) # residual matrix
  R.matrix = matrix(0, n, M)
  PI = c()
  for(k in 1:M){
    PI[k] = mean(P[, k])
    Eps.matrix[, k] = y - X %*% beta.matrix.new[, k]
    R.matrix[, k] = Eps.matrix[, k] * Tao[k] # standarized residual
  }
  numiter = 0
  while(norm(beta.matrix.new - beta.matrix.old, 'f') > 1e-2 & numiter < maxiter){
    numiter = numiter + 1
    beta.matrix.old = beta.matrix.new
    grid = seq(min(R.matrix) - 3 * H, max(R.matrix) + 3 * H, by = H/10)
    ###############################
    gfungrid = c()
    for(i in 1:length(grid)){
      gfungrid[i] = (t(P[, 1]) %*% stats::dnorm((R.matrix[, 1] - grid[i])/H)/H +
                       t(P[,2]) %*% stats::dnorm((R.matrix[,2] - grid[i])/H)/H)/n
    }
    ###############################
    estg <- function(x){
      out = stats::approx(grid, gfungrid, x)$y
      out[x > max(grid)] = 0
      out[x < min(grid)] = 0
      return(out)
    }
    # M-step
    for(j in 1:M){
      fn <- function(bet){
        out = - P[, j] %*% log(estg((y - X %*% bet) * Tao[j]))
      }
      beta.matrix.new[, j] = ucminf::ucminf(beta.matrix.new[, j], fn)$par
      Eps.matrix[, j] = y - X %*% beta.matrix.new[, j]
      Tao[j] = (sum(P[, j] * (Eps.matrix[, j]^2))/sum(P[, j]))^(- 0.5)
      R.matrix[, j] = Eps.matrix[, j] * Tao[j]
      P[, j] = PI[j] * Tao[j] * estg(R.matrix[, j])
    }
    P = P/matrix(rep(rowSums(P), M), ncol = M)
    PI = colMeans(P)
  }
  # print(beta.matrix.new)
  # print(numiter)
  ############################################################################
  return(list(posterior = P, beta = beta.matrix.new, tau = Tao, pi = PI, h = H))
}
