#' Semiparametric Mixture Regression Models with Single-index Proportion and Fully Iterative Backfitting
#'
#' Assume that \eqn{\boldsymbol{x} = (\boldsymbol{x}_1,\cdots,\boldsymbol{x}_n)} is an n by p matrix and
#' \eqn{Y = (Y_1,\cdots,Y_n)} is an n-dimensional vector of response variable.
#' The conditional distribution of \eqn{Y} given
#' \eqn{\boldsymbol{x}} can be written as:
#' \deqn{f(y|\boldsymbol{x},\boldsymbol{\alpha},\pi,m,\sigma^2) =
#' \sum_{j=1}^C\pi_j(\boldsymbol{\alpha}^{\top}\boldsymbol{x})
#' \phi(y|m_j(\boldsymbol{\alpha}^{\top}\boldsymbol{x}),\sigma_j^2(\boldsymbol{\alpha}^{\top}\boldsymbol{x})).}
#' `semimrFull' is used to estimate the mixture of single-index models described above,
#' where \eqn{\phi(y|m_j(\boldsymbol{\alpha}^{\top}\boldsymbol{x}),\sigma_j^2(\boldsymbol{\alpha}^{\top}\boldsymbol{x}))}
#' represents the normal density with a mean of \eqn{m_j(\boldsymbol{\alpha}^{\top}\boldsymbol{x})} and
#' a variance of \eqn{\sigma_j^2(\boldsymbol{\alpha}^{\top}\boldsymbol{x})}, and
#' \eqn{\pi_j(\cdot), \mu_j(\cdot), \sigma_j^2(\cdot)} are unknown smoothing single-index functions
#' capable of handling high-dimensional non-parametric problem.
#' This function employs kernel regression and a fully iterative backfitting (FIB) estimation procedure
#' (Xiang and Yao, 2020).
#'
#' @usage
#' semimrFull(x, y, h = NULL, coef = NULL, ini = NULL, grid = NULL, maxiter = 100)
#'
#' @param x an n by p matrix of observations where n is the number of observations and
#'   p is the number of explanatory variables.
#' @param y an n-dimensional vector of response values.
#' @param h bandwidth for the kernel regression. Default is NULL, and
#'   the bandwidth is computed in the function by cross-validation.
#' @param coef initial value of \eqn{\boldsymbol{\alpha}^{\top}} in the model, which plays a role
#'   of regression coefficient in a regression model. Default is NULL, and
#'   the value is computed in the function by sliced inverse regression (Li, 1991).
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values,
#'   assuming a linear mixture model.
#'   If specified, it can be a list with the form of \code{list(pi, mu, var)}, where
#'   \code{pi} is a vector of mixing proportions,
#'   \code{mu} is a vector of component means, and
#'   \code{var} is a vector of component variances.
#' @param grid grid points at which nonparametric functions are estimated.
#'   Default is NULL, which uses the estimated mixing proportions, component means, and
#'   component variances as the grid points after the algorithm converges.
#' @param maxiter maximum number of iterations. Default is 100.
#'
#' @return A list containing the following elements:
#'   \item{pi}{matrix of estimated mixing proportions.}
#'   \item{mu}{estimated component means.}
#'   \item{var}{estimated component variances.}
#'   \item{coef}{estimated regression coefficients.}
#'   \item{run}{total number of iterations after convergence.}
#'
#' @seealso \code{\link{semimrOne}}, \code{\link{sinvreg}} for initial value calculation of
#'   \eqn{\boldsymbol{\alpha}^{\top}}.
#'
#' @references
#'   Xiang, S. and Yao, W. (2020). Semiparametric mixtures of regressions with single-index
#'   for model based clustering. Advances in Data Analysis and Classification, 14(2), 261-292.
#'
#'   Li, K. C. (1991). Sliced inverse regression for dimension reduction.
#'   Journal of the American Statistical Association, 86(414), 316-327.
#'
#' @importFrom MASS ginv
#' @export
#' @examples
#' xx = NBA[, c(1, 2, 4)]
#' yy = NBA[, 3]
#' x = xx/t(matrix(rep(sqrt(diag(var(xx))), length(yy)), nrow = 3))
#' y = yy/sd(yy)
#' ini_bs = sinvreg(x, y)
#' ini_b = ini_bs$direction[, 1]
#' \donttest{est = semimrFull(x[1:50, ], y[1:50], h = 0.3442, coef = ini_b)}
semimrFull <- function(x, y, h = NULL, coef = NULL, ini = NULL, grid = NULL, maxiter = 100) {
  n = length(y)
  x = as.matrix(x)
  y = as.vector(y) ##SK-- 9/4/2023

  est_b <- coef

  if (is.null(h)) {
    h = cv_bw(x, y)$h_opt
  }

  if (is.null(est_b)) {
    ini_b = sinvreg(x, y)
    est_b = ini_b$direction[, 1]
  }
  est_b = as.matrix(est_b)
  z = x %*% est_b

  if (is.null(ini)) {
    ini = mixlinreg(z, y, z)
  }
  #SK-- 9/4/2023
  est_p = ini$pi * matrix(rep(1, n * length(ini$pi)), nrow = n)
  est_var = ini$var * matrix(rep(1, n * length(ini$var)), nrow = n)
  est_mu = ini$mu
  #est_p = ini$est_p * matrix(rep(1, n * length(ini$est_p)), nrow = n)
  #est_var = ini$est_var * matrix(rep(1, n * length(ini$est_var)), nrow = n)
  #est_mu = ini$est_mu

  iter = 1
  fmin = 999999
  diff = 1

  while (iter < maxiter && diff > 10^(-3)) {
    # Step 1: Estimate mixture parameters
    z = x %*% est_b
    est1 = mixnonpar(z, y, z, est_p, est_mu, est_var, h)
    est_p = est1$pi
    est_var = est1$var
    est_mu = est1$mu
    #est_p = est1$est_p
    #est_var = est1$est_var
    #est_mu = est1$est_mu

    # Define objective function for Step 2
    obj2 = function(b) {
      u = x %*% b
      est_p_u = matrix(rep(0, 2 * length(u)), ncol = 2)
      est_mu_u = matrix(rep(0, 2 * length(u)), ncol = 2)
      est_var_u = matrix(rep(0, 2 * length(u)), ncol = 2)

      for (i in 1:2) {
        est_p_u[, i] = stats::approx(z, est_p[, i], u)$y
        est_mu_u[, i] = stats::approx(z, est_mu[, i], u)$y
        est_var_u[, i] = stats::approx(z, est_var[, i], u)$y
      }

      d = matrix(rep(0, 2 * length(u)), ncol = 2)

      for (i in 1:2) {
        d[, i] = stats::dnorm(y, est_mu_u[, i], est_var_u[, i]^0.5)
      }

      f = -sum(log(d %*% t(est_p_u)))
      f
    }

    # Step 2: Optimize the objective function to estimate est_b
    fminsearch = optim(est_b, obj2)
    est_b = fminsearch$par
    fval = fminsearch$value

    diff = abs(fval - fmin)
    fmin = fval
    iter = iter + 1
  }

  est_b = est_b / max(svd(est_b)$d)
  z = x %*% est_b
  est2 = mixnonpar(z, y, z, est_p, est_mu, est_var, h)
  est_p = est2$pi
  est_var = est2$var
  est_mu = est2$mu
  #est_p = est2$est_p
  #est_var = est2$est_var
  #est_mu = est2$est_mu

  res_p = numeric()
  res_mu = numeric()
  res_var = numeric()

  if (is.null(grid)) {
    res_p = est_p
    res_mu = est_mu
    res_var = est_var
  } else {
    for (i in 1:2) {
      res1 = stats::approx(z, est_p[, i], grid)$y
      res2 = stats::approx(z, est_mu[, i], grid)$y
      res3 = stats::approx(z, est_var[, i], grid)$y
      res_p = cbind(res_p, res1)
      res_mu = cbind(res_mu, res2)
      res_var = cbind(res_var, res3)
    }
  }

  out = list(pi = res_p, mu = res_mu, var = res_var, coef = est_b, run = iter)
  out
}

#' Semiparametric Mixture Regression Models with Single-index and One-step Backfitting
#'
#' Assume that \eqn{\boldsymbol{x} = (\boldsymbol{x}_1,\cdots,\boldsymbol{x}_n)} is an n by p matrix and
#' \eqn{Y = (Y_1,\cdots,Y_n)} is an n-dimensional vector of response variable.
#' The conditional distribution of \eqn{Y} given
#' \eqn{\boldsymbol{x}} can be written as:
#' \deqn{f(y|\boldsymbol{x},\boldsymbol{\alpha},\pi,m,\sigma^2) =
#' \sum_{j=1}^C\pi_j(\boldsymbol{\alpha}^{\top}\boldsymbol{x})
#' \phi(y|m_j(\boldsymbol{\alpha}^{\top}\boldsymbol{x}),\sigma_j^2(\boldsymbol{\alpha}^{\top}\boldsymbol{x})).}
#' `semimrFull' is used to estimate the mixture of single-index models described above,
#' where \eqn{\phi(y|m_j(\boldsymbol{\alpha}^{\top}\boldsymbol{x}),\sigma_j^2(\boldsymbol{\alpha}^{\top}\boldsymbol{x}))}
#' represents the normal density with a mean of \eqn{m_j(\boldsymbol{\alpha}^{\top}\boldsymbol{x})} and
#' a variance of \eqn{\sigma_j^2(\boldsymbol{\alpha}^{\top}\boldsymbol{x})}, and
#' \eqn{\pi_j(\cdot), \mu_j(\cdot), \sigma_j^2(\cdot)} are unknown smoothing single-index functions
#' capable of handling high-dimensional non-parametric problem.
#' This function employs kernel regression and a one-step estimation procedure (Xiang and Yao, 2020).
#'
#' @usage
#' semimrOne(x, y, h, coef = NULL, ini = NULL, grid = NULL)
#'
#' @param x an n by p matrix of observations where n is the number of observations and
#'   p is the number of explanatory variables.
#' @param y a vector of response values.
#' @param h bandwidth for the kernel regression. Default is NULL, and
#'   the bandwidth is computed in the function by cross-validation.
#' @param coef initial value of \eqn{\boldsymbol{\alpha}^{\top}} in the model, which plays a role
#'   of regression coefficient in a regression model. Default is NULL, and
#'   the value is computed in the function by sliced inverse regression (Li, 1991).
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values,
#'   assuming a linear mixture model.
#'   If specified, it can be a list with the form of \code{list(pi, mu, var)}, where
#'   \code{pi} is a vector of mixing proportions,
#'   \code{mu} is a vector of component means, and
#'   \code{var} is a vector of component variances.
#' @param grid grid points at which nonparametric functions are estimated.
#'   Default is NULL, which uses the estimated mixing proportions, component means, and
#'   component variances as the grid points after the algorithm converges.
#'
#' @return A list containing the following elements:
#'   \item{pi}{estimated mixing proportions.}
#'   \item{mu}{estimated component means.}
#'   \item{var}{estimated component variances.}
#'   \item{coef}{estimated regression coefficients.}
#'
#' @seealso \code{\link{semimrFull}}, \code{\link{sinvreg}} for initial value calculation of
#'   \eqn{\boldsymbol{\alpha}^{\top}}.
#'
#' @references
#'   Xiang, S. and Yao, W. (2020). Semiparametric mixtures of regressions with single-index
#'   for model based clustering. Advances in Data Analysis and Classification, 14(2), 261-292.
#'
#'   Li, K. C. (1991). Sliced inverse regression for dimension reduction.
#'   Journal of the American Statistical Association, 86(414), 316-327.
#'
#' @export
#' @examples
#' xx = NBA[, c(1, 2, 4)]
#' yy = NBA[, 3]
#' x = xx/t(matrix(rep(sqrt(diag(var(xx))), length(yy)), nrow = 3))
#' y = yy/sd(yy)
#' ini_bs = sinvreg(x, y)
#' ini_b = ini_bs$direction[, 1]
#'
#' # used a smaller sample for a quicker demonstration of the function
#' set.seed(123)
#' est_onestep = semimrOne(x[1:50, ], y[1:50], h = 0.3442, coef = ini_b)
semimrOne <- function(x, y, h, coef = NULL, ini = NULL, grid = NULL) {
  n = length(y)
  x = as.matrix(x)
  y = as.vector(y) ##SK-- 9/4/2023

  ##SK-- 9/4/2023
  est_b <- coef

  if (is.null(h)) {
    h = cv_bw(x, y)$h_opt
  }
  ##

  if (is.null(est_b)) {
    ini_b = sinvreg(x, y)
    est_b = ini_b$direction[, 1]
  }
  est_b = as.matrix(est_b)
  z = x %*% est_b

  if (is.null(ini)) {
    ini = mixlinreg(z, y, z)
  }
  #SK-- 9/4/2023
  est_p = ini$pi * matrix(rep(1, n * length(ini$pi)), nrow = n)
  est_var = ini$var * matrix(rep(1, n * length(ini$var)), nrow = n)
  est_mu = ini$mu
  #est_p = ini$est_p * matrix(rep(1, n * length(ini$est_p)), nrow = n)
  #est_var = ini$est_var * matrix(rep(1, n * length(ini$est_var)), nrow = n)
  #est_mu = ini$est_mu

  # Estimate mixture parameters using mixnonpar
  est = mixnonpar(z, y, z, est_p, est_mu, est_var, h)
  est_p = est$pi
  est_var = est$var
  est_mu = est$mu
  #est_p = est$est_p
  #est_var = est$est_var
  #est_mu = est$est_mu

  res_p = numeric()
  res_mu = numeric()
  res_var = numeric()

  if (is.null(grid)) {
    res_p = est_p
    res_mu = est_mu
    res_var = est_var
  } else {
    for (i in 1:2) {
      res1 = stats::approx(z, est_p[, i], grid)$y
      res2 = stats::approx(z, est_mu[, i], grid)$y
      res3 = stats::approx(z, est_var[, i], grid)$y
      res_p = cbind(res_p, res1)
      res_mu = cbind(res_mu, res2)
      res_var = cbind(res_var, res3)
    }
  }

  out = list(pi = res_p, mu = res_mu, var = res_var, coef = est_b)
  out
}

#' Dimension Reduction Based on Sliced Inverse Regression
#'
#' `sinvreg' is used in the examples for the \code{\link{semimrFull}} and \code{\link{semimrOne}} functions to obtain
#' initial values based on sliced inverse regression (Li, 1991).
#'
#' @usage
#' sinvreg(x, y, nslice = NULL)
#'
#' @param x an n by p matrix of observations where n is the number of observations and
#'   p is the number of explanatory variables.
#' @param y an n-dimentionsl vector of response values.
#' @param nslice number of slices. Default is 10.
#'
#' @return A list containing the following elements:
#'   \item{direction}{direction vector.}
#'   \item{reducedx}{reduced x.}
#'   \item{eigenvalue}{eigenvalues for reduced x.}
#'   \item{nobs}{number of observations within each slice.}
#'
#' @seealso \code{\link{semimrFull}}, \code{\link{semimrOne}}
#'
#' @references
#'   Li, K. C. (1991). Sliced inverse regression for dimension reduction.
#'   Journal of the American Statistical Association, 86(414), 316-327.
#'
#' @export
#' @examples
#' # See examples for the 'semimrFull' function.
sinvreg <- function(x, y, nslice = NULL){

  x = as.matrix(x) ##SK-- 9/4/2023
  y = as.vector(y) ##SK-- 9/4/2023

  n = length(y)
  numslice <- nslice

  if (is.null(numslice)) {
    numslice = 10
  }

  # Sort the response variable y and obtain the indices
  ind = sort(y, index.return = TRUE)$ix

  # Calculate the mean of predictor variables x
  mx = apply(x, 2, mean)

  # Calculate the number of repetitions for each slice
  numrep = round(n/numslice + 1e-06)

  matx = 0  # Initialize matx

  xbar = numeric()

  # Calculate xbar for each slice
  for (i in 1:(numslice - 1)) {
    a = ((i - 1) * numrep + 1)
    b = i * numrep
    xbar1 = apply(x[ind[a:b], ], 2, mean)
    xbar = rbind(xbar, xbar1)
    matx = matx + numrep * t(matrix(xbar[i, ] - mx, nrow = 1)) %*% (xbar[i, ] - mx)
  }

  # Calculate xbar for the last slice
  xbar = rbind(xbar, apply(x[ind[((numslice - 1) * numrep + 1):n], ], 2, mean))
  matx = matx + (n - (numslice - 1) * numrep) * t(matrix(xbar[numslice, ] - mx, nrow = 1)) %*% (xbar[numslice, ] - mx)

  # Calculate the generalized inverse of the covariance matrix
  matx = MASS::ginv(stats::var(x) * (n - 1)) %*% matx

  # Perform eigenvalue decomposition
  ss = eigen(matx)
  sv = ss$vec
  sd = ss$val
  temp = abs(sd)
  b = sort(-temp, index.return = TRUE)$ix
  sd = temp[b]
  sv = sv[, b]

  out = list(direction = sv, reducedx = matx, eigenvalue = sd, nobs = numrep)
  out
}


#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
# Function to estimate a linear mixture model
mixlinreg <- function(x, y, u) {
  n = length(x)

  # Define the design matrix U
  U = cbind(rep(1, length(u)), u)

  # Create the design matrix X
  X = cbind(rep(1, n), x)

  # Use mixtools package to estimate the linear mixture model
  est = mixtools::regmixEM(y, x)
  est_beta = est$beta
  est_var = (est$sigma)^2
  est_p = est$lambda
  est_mu = X %*% est_beta
  est_mu_x0 = U %*% est_beta

  #SK-- 9/4/2023
  out = list(pi = est_p, mu = est_mu, var = est_var, mu_x0 = est_mu_x0)
  #out = list(est_p = est_p, est_mu = est_mu, est_var = est_var, est_mu_x0 = est_mu_x0)
  out
}

# Function to estimate a non-parametric mixture model
mixnonpar <- function(x, y, u, est_p, est_mu, est_var, h) {
  n = dim(est_mu)[1]
  numc = dim(est_mu)[2]
  m = length(u)
  r = matrix(numeric(n * numc), nrow = n)
  f = r
  est_mu_u = matrix(numeric(m * numc), nrow = m)
  est_p_u = est_mu_u
  est_var_u = est_mu_u
  rsd = 1
  likelihood = 1
  iter = 0
  acc = 10^(-3)

  repeat {
    iter = iter + 1

    # Calculate the likelihood and responsibilities
    for (i in 1:numc) {
      f[, i] = stats::dnorm(y, est_mu[, i], est_var[, i]^0.5)
    }

    for (i in 1:numc) {
      r[, i] = est_p[, i] * f[, i] / apply(f * est_p, 1, sum)

      # Calculate weights based on the kernel function
      W = exp(-(((t(matrix(rep(t(x), n), nrow = n)) - matrix(rep(x, n), ncol = n))^2) / (2 * h^2))) /
        (h * sqrt(2 * pi))

      R = t(matrix(rep(r[, i], n), ncol = n))
      RY = t(matrix(rep(r[, i] * y, n), ncol = n))

      # Update mixture parameters
      est_p[, i] = apply(W * R, 1, sum) / apply(W, 1, sum)
      est_mu[, i] = apply(W * RY, 1, sum) / apply(W * R, 1, sum)
      RE = (t(matrix(rep(y, n), ncol = n)) - matrix(rep(est_mu[, i], n), ncol = n))^2
      est_var[, i] = apply(W * RE * R, 1, sum) / apply(W * R, 1, sum)
    }

    rsd = likelihood - sum(log(apply(f * est_p, 1, sum)))
    likelihood = sum(log(apply(f * est_p, 1, sum)))

    # Check convergence
    if (abs(rsd) < acc || iter > 200) {
      break
    }
  }

  # Interpolate the estimated values to the grid points
  for (i in 1:2) {
    est_p_u[, i] = stats::approx(x, est_p[, i], u)$y
    est_mu_u[, i] = stats::approx(x, est_mu[, i], u)$y
    est_var_u[, i] = stats::approx(x, est_var[, i], u)$y
  }

  #SK-- 9/4/2023
  out = list(pi = est_p_u, mu = est_mu_u, var = est_var_u)
 # out = list(est_p = est_p_u, est_mu = est_mu_u, est_var = est_var_u)
  out
}

# Function for cross-validation to select bandwidth
cv_bw <- function(x, y) {
  x = as.matrix(x)
  y = as.matrix(y)
  n = length(y)
  numc = 2
  h0 = seq(from = 0.06, to = 0.16, length.out = 10)
  h_l = length(h0)
  J = 10
  cv = matrix(rep(0, h_l * J), nrow = h_l)
  size = floor(n / J)
  f = matrix(numeric(size * numc), ncol = numc)
  r = f
  true_b = as.matrix(c(1, 1, 1) / sqrt(3))

  for (hh in 1:h_l) {
    for (j in 1:J) {
      # Split the data into test and train sets
      if (j == 1) {
        test = matrix(c(y[((j - 1) * size + 1):(j * size)], x[((j - 1) * size + 1):(j * size), ]), nrow = size)
        train = matrix(c(y[(j * size + 1):n], x[(j * size + 1):n, ]), nrow = n - size)
      }
      if (j != 1 && j != 10) {
        test = matrix(c(y[((j - 1) * size + 1):(j * size)], x[((j - 1) * size + 1):(j * size), ]), nrow = size)
        train = matrix(c(y[1:((j - 1) * size)], y[(j * size + 1):n], x[1:((j - 1) * size), ], x[(j * size + 1):n, ]), nrow = n - size)
      }
      if (j == 10) {
        test = matrix(c(y[((j - 1) * size + 1):(j * size)], x[((j - 1) * size + 1):(j * size), ]), nrow = size)
        train = matrix(c(y[1:((j - 1) * size)], x[1:((j - 1) * size), ]), nrow = n - size)
      }

      # Estimate initial parameters
      ini = sinvreg(train[, -1], train[, 1])
      if (sum(true_b * ini$direction[, 1]) < 0) {
        ini_b = -ini$direction[, 1]
      } else {
        ini_b = ini$direction[, 1]
      }
      ini_b = as.matrix(ini_b)

      # Run the simulation
      est = semimrFull(train[, -1], train[, 1], h0[hh], ini_b)
      est_p = est$pi
      est_mu = est$mu
      est_var = est$var
      est_b = est$coef
      #est_p = est$est_p
      #est_mu = est$est_mu
      #est_var = est$est_var
      #est_b = est$est_b

      z0 = test[, -1] %*% est_b
      z = train[, -1] %*% est_b

      res_p = numeric()
      res_mu = numeric()
      res_var = numeric()
      for (i in 1:numc) {
        res1 = stats::approx(z, est_p[, i], z0)$y
        res2 = stats::approx(z, est_mu[, i], z0)$y
        res3 = stats::approx(z, est_var[, i], z0)$y
        res_p = cbind(res_p, res1)
        res_mu = cbind(res_mu, res2)
        res_var = cbind(res_var, res3)
      }

      for (c in 1:numc) {
        f[, c] = stats::dnorm(test[, 1], res_mu[, c], res_var[, c]^0.5)
        f[, c] = Re(f[, c] * is.finite(f[, c]))
      }
      for (c in 1:numc) {
        r[, c] = res_p[, c] * f[, c] / apply(f * res_p, 1, sum)
      }

      cal = (apply(r * res_mu, 1, sum) - test[, 1])^2
      cv[hh, j] = mean(cal[!is.na(cal)])
    }
  }

  cv_opt = apply(cv, 1, sum)
  m = max(cv_opt)
  I = which.min(cv_opt)
  h_opt = h0[I]
  CV = list(h_opt = h_opt, cv_opt = cv_opt)
}

#emest_linear<-function(X,y,numc,est_beta,est_var,est_p){
#  n=length(y);
#  f=matrix(numeric(n*numc),nrow=n);r=f;
#  rsd=1;likelihood=1;iter=0;acc=10^(-3);
#  repeat
#  {
#    iter=iter+1;
#    for(i in 1:numc){
#      f[,i]=dnorm(y,X%*%est_beta[,i],est_var[i]^0.5*rep(1,n));
#    }
#
#    for(i in 1:numc){
#      est_p=as.numeric(est_p)
#      r[,i]=est_p[i]*f[,i]/apply(f*est_p,1,sum);#(f%*%est_p);apply(f*est_p,1,sum);
#      #s=r[,i];yy=y*s;xx=X*matrix(rep(s,2),nrow=n);b=coefficients(lm(yy~xx));est_beta[,i]=c(b[2],b[3]);
#      S=diag(r[,i]);est_beta[,i]=ginv(t(X)%*%S%*%X)%*%t(X)%*%S%*%y;
#      est_var[i]=sum(r[,i]*(y-X%*%est_beta[,i])^2)/sum(r[,i]);
#    }
#
#    est_p=cbind(mean(r[,1]),mean(r[,2]));
#
#    rsd=likelihood-sum(log(f%*%t(est_p)));
#    likelihood=sum(log(f%*%t(est_p)));
#    if(abs(rsd)<acc|iter>20){break}
#  }

#  out=list(est_p=est_p,est_beta=est_beta,est_var=est_var,likelihood=likelihood)
#  out
#}
