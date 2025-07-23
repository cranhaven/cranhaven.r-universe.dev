#' Semiparametric Mixture Data Generator
#'
#' `semimrGen' is used to generate data for a two-component semiparametric mixture of regression models:
#' \deqn{p m_1(x) + (1-p) m_2(x),}
#' where \eqn{m_1(x) = 4 -\sin(2\pi x)} and \eqn{m_2(x) = 1.5 + \cos(3\pi x).}
#' This function is used in the examples for the \code{\link{semimrLocal}} and \code{\link{semimrGlobal}} functions.
#' See the examples for details.
#'
#' @usage
#' semimrGen(n, p = 0.5, var = c(.1, .1), u)
#'
#' @param n a scalar, specifying the number of observations in \eqn{x}.
#' @param p a scalar, specifying the probability of an observation belonging to the first component,
#'   i.e., \eqn{p} in the model.
#' @param var a vector of variances of observations for the two components.
#' @param u a vector of grid points for \eqn{x}. If some specific explanatory variable are needed, create a vector and assign to \code{u}.
#'
#' @return A list containing the following elements:
#'   \item{x}{vector of length n, which represents the explanatory variable
#'      that is randomly generated from Uniform(0,1).}
#'   \item{y}{vector of length n, which represent the response variable
#'     that is generated based on the mean functions \eqn{m_1(x)} and \eqn{m_2(x)},
#'     with the addition of normal errors having a mean of 0 and a standard deviation specified by the user.}
#'   \item{true_mu}{n by 2 matrix containing the values of \eqn{m_1(x)} and \eqn{m_2(x)} at x.}
#'   \item{true_mu_u}{length(u) by 2 matrix containing the values of \eqn{m_1(x)} and \eqn{m_2(x)} at u.}
#'
#' @seealso \code{\link{semimrLocal}}, \code{\link{semimrGlobal}}, \code{\link{semimrBinFull}}
#'
#' @export
#' @examples
#' n = 100
#' u = seq(from = 0, to = 1, length = 100)
#' true_p = c(0.3, 0.7)
#' true_var = c(0.09, 0.16)
#' out = semimrGen(n = n, p = true_p[1], var = true_var, u = u)
semimrGen <- function(n, p = 0.5, var = c(.1, .1), u) {

  # Generate the number of observations in class 1
  n1 = stats::rbinom(1, n, p)
  # Generate uniform random values
  x = stats::runif(n, 0, 1)

  # Initialize error vector
  err = numeric(n)
  # Generate errors based on the specified variances
  err[1:n1] = stats::rnorm(n1, 0, 1) * sqrt(var[1])
  err[(n1 + 1):n] = stats::rnorm(n - n1, 0, 1) * sqrt(var[2])

  # Initialize true values vector
  true = numeric(n)
  # Generate true values based on class
  true[1:n1] = 4 - sin(2 * pi * x[1:n1])
  true[(n1 + 1):n] = 1.5 + cos(3 * pi * x[(n1 + 1):n])

  # Combine true values and errors to get observed values
  y = true + err

  # Create true_mu matrices for all observations and for the input 'u'
  true_mu = matrix(c(4 - sin(2 * pi * x), 1.5 + cos(3 * pi * x)), nrow = n)
  true_mu_u = matrix(c(4 - sin(2 * pi * u), 1.5 + cos(3 * pi * u)), nrow = length(u))

  # Return the generated data as a list
  out = list(x = x, y = y, true_mu = true_mu, true_mu_u = true_mu_u)
  out
}

# Generate bootstrapped mixture regression data
semimrGen_boot <- function(x, phat, muhat, varhat) {
  n = dim(muhat)[1]  # Number of observations
  k = dim(muhat)[2]  # Number of components in the mixture

  # Extract the estimated mixing proportions
  p = round(phat[1] * 100) / 100

  # Generate the number of observations in class 1 based on the estimated mixing proportion
  n0 = stats::rbinom(1, n, p)
  n1 = c(n0, n - n0)                 # Number of observations in each class
  pos = 1                            # Initialize position
  err = matrix(rep(0, n), nrow = n)  # Initialize error matrix
  true = err                         # Initialize true values matrix

  # Generate errors and true values for each component
  for (j in 1:k) {
    ind = pos:sum(n1[1:j])      # Indices for the current component
    err[ind] = stats::rnorm(n1[j], mean = 0, sd = 1) * sqrt(varhat[j])  # Generate errors
    true[ind] = muhat[ind, j]   # True values based on component means
    pos = sum(n1[1:j]) + 1      # Update position for the next component
  }

  # Generate bootstrapped response values
  y_boot = true + err
}

#' Semiparametric Mixtures of Nonparametric Regressions with Local EM-type Algorithm
#'
#' `semimrLocal' is used to estimate a mixture of regression models, where the mixing proportions
#' and variances remain constant, but the component regression functions are smooth functions (\eqn{m(\cdot)})
#' of a covariate. The model is expressed as follows:
#' \deqn{\sum_{j=1}^C\pi_j\phi(y|m(x_j),\sigma^2_j).}
#' This function provides the one-step backfitting estimate using the local EM-type algorithm (LEM) (Xiang and Yao, 2018).
#' As of version 1.1.0, this function supports a two-component model.
#'
#' @usage
#' semimrLocal(x, y, u = NULL, h = NULL, ini = NULL)
#'
#' @param x a vector of covariate values.
#' @param y a vector of response values.
#' @param u a vector of grid points for spline method to estimate the proportions.
#'   If NULL (default), 100 equally spaced grid points are automatically generated
#'   between the minimum and maximum values of x.
#' @param h bandwidth for the nonparametric regression. If NULL (default), the bandwidth is
#'   calculated based on the method of Botev et al. (2010).
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values
#'   using regression spline approximation. If specified, it can be a list with the form of
#'   \code{list(pi, mu, var)}, where
#'   \code{pi} is a vector of length 2 of mixing proportions,
#'   \code{mu} is a length(x) by 2 matrix of component means with length(x) rows, and
#'   \code{var} is a vector of length 2 of variances.
#'
#' @return A list containing the following elements:
#'   \item{pi}{vector of length 2 of estimated mixing proportions.}
#'   \item{mu}{length(x) by 2 matrix of estimated mean functions at x, \eqn{m(x)}.}
#'   \item{mu_u}{length(u) by 2 matrix of estimated mean functions at grid point u, \eqn{m(u)}.}
#'   \item{var}{vector of length 2 estimated component variances.}
#'   \item{lik}{final likelihood.}
#'
#' @seealso \code{\link{semimrGlobal}}, \code{\link{semimrGen}}
#'
#' @references
#'   Xiang, S. and Yao, W. (2018). Semiparametric mixtures of nonparametric regressions.
#'   Annals of the Institute of Statistical Mathematics, 70, 131-154.
#'
#'   Botev, Z. I., Grotowski, J. F., and Kroese, D. P. (2010). Kernel density estimation via diffusion.
#'   The Annals of Statistics, 38(5), 2916-2957.
#'
#' @export
#' @examples
#' # produce data that matches the description using semimrGen function
#' # true_mu = (4 - sin(2 * pi * x), 1.5 + cos(3 * pi * x))
#' n = 100
#' u = seq(from = 0, to = 1, length = 100)
#' true_p = c(0.3, 0.7)
#' true_var = c(0.09, 0.16)
#' out = semimrGen(n, true_p[1], true_var, u)
#'
#' x = out$x
#' y = out$y
#' true_mu = out$true_mu
#' true = list(true_p = true_p, true_mu = true_mu, true_var = true_var)
#'
#' # estimate parameters using semimrLocal function.
#' \donttest{est = semimrLocal(x, y)}
semimrLocal <- function(x, y, u = NULL, h = NULL, ini = NULL) {

  n = length(y)  # Number of observations # added sep 18 2022 Xin Shen to fix the error message of 'no visible binding for global variable ‘n’'

  # Default values for 'u' and 'h' if not provided
  if (is.null(u)) {
    u = seq(from = min(x), to = max(x), length = 100)
  }
  if (is.null(h)) {
    h = kdebw(x, 2^14)
  }

  # Initialize 'ini' if not provided
  if (is.null(ini)) {
    # Cluster the data into 2 groups using k-means
    IDX = stats::kmeans(cbind(x, y), 2)$cluster
    ini_p = rbind(2 - IDX, IDX - 1)
    ini = list(p = t(ini_p))

    # Initialize 'ini' using mixbspline
    ini = mixbspline(u, x, y, ini)
  }

  est_mu_x = ini$mu
  est_p = ini$pi
  est_p = cbind(rep(est_p[1], n), rep(est_p[2], n))
  est_var = ini$var
  est_var = cbind(rep(est_var[1], n), rep(est_var[2], n))

  # Backfitting steps
  est1 = backfitting_step1(x, y, u, est_p, est_mu_x, est_var, h)
  est_p = est1$est_p
  est_var = est1$est_var
  est_mu_x = est1$est_mu_x
  est2 = backfitting_step2(x, y, u, est_p, est_mu_x, est_var)
  est_p = est2$est_p
  est_var = est2$est_var
  est3 = backfitting_step3(x, y, u, est_p, est_mu_x, est_var, h)
  est_mu_x = est3$est_mu_x
  est_mu_u = est3$est_mu_u
  likelihood = est3$lh

  # Return the results
  out = list(pi = est_p[1, ], mu = est_mu_x, var = est_var[1, ], mu_u = est_mu_u, lik = likelihood)
  return(out)
}

# Perform step 1 of the backfitting algorithm
backfitting_step1 <- function(x, y, u, est_p, est_mu_x, est_var, h) {
  n = dim(est_mu_x)[1]  # Number of observations
  numc = dim(est_mu_x)[2]  # Number of components
  m = length(u)  # Number of grid points

  r = matrix(numeric(n * numc), nrow = n)
  f = r
  est_p_u = matrix(numeric(m * numc), nrow = m)
  est_mu_u = matrix(numeric(m * numc), nrow = m)
  est_var_u = matrix(numeric(m * numc), nrow = m)

  rsd = 1
  likelihood = 1
  iter = 0
  acc = 10^(-3)

  repeat {
    iter = iter + 1

    # E-step: Calculate the conditional density
    for (i in 1:numc) {
      f[, i] = stats::dnorm(y, est_mu_x[, i], est_var[, i]^0.5)
    }

    # M-step: Update the conditional density
    for (i in 1:numc) {
      r[, i] = est_p[, i] * f[, i]/apply(f * est_p, 1, sum)

      # Compute the weights and update the parameters using linear interpolation
      W = exp(-(((t(matrix(rep(x, m), nrow = m)) - matrix(rep(u, n), ncol = n))^2)/(2 * h^2)))/(h * sqrt(2 * pi)) #Updated sep20 2022 after meeting with Yan Ge.
      R = t(matrix(rep(r[, i], m), ncol = m))
      RY = t(matrix(rep(r[, i] * y, m), ncol = m))
      est_p_u[, i] = apply(W * R, 1, sum)/apply(W, 1, sum)
      est_p[, i] = stats::approx(u, est_p_u[, i], x)$y
      est_mu_u[, i] = apply(W * RY, 1, sum)/apply(W * R, 1, sum)
      est_mu_x[, i] = stats::approx(u, est_mu_u[, i], x)$y
      RE = (t(matrix(rep(y, m), ncol = m)) - matrix(rep(est_mu_u[, i], n), ncol = n))^2
      est_var_u[, i] = apply(W * RE * R, 1, sum)/apply(W * R, 1, sum)
      est_var[, i] = stats::approx(u, est_var_u[, i], x)$y
    }

    rsd = likelihood - sum(log(apply(f * est_p, 1, sum)))
    likelihood = sum(log(apply(f * est_p, 1, sum)))

    # Convergence check
    if (abs(rsd) < acc | iter > 200) {
      break
    }
  }

  out = list(est_p = est_p, est_mu_x = est_mu_x, est_var = est_var)
  return(out)
}

# Perform step 2 of the backfitting algorithm
backfitting_step2 <- function(x, y, u, est_p, est_mu_x, est_var) {

  n = dim(est_mu_x)[1]  # Number of observations
  numc = dim(est_mu_x)[2]  # Number of components
  m = length(u)  # Number of grid points

  r = matrix(numeric(n * numc), nrow = n)
  f = r

  rsd = 1
  likelihood = 1
  iter = 0
  acc = 10^(-3)

  repeat {
    iter = iter + 1

    # E-step: Calculate the conditional density
    for (i in 1:numc) {
      f[, i] = stats::dnorm(y, est_mu_x[, i], est_var[, i]^0.5)
    }

    # M-step: Update the conditional density
    for (i in 1:numc) {
      r[, i] = est_p[, i] * f[, i]/apply(f * est_p, 1, sum)

      # Update est_p and est_var
      est_p[, i] = matrix(rep(sum(r[, i])/n, n), nrow = n)
      est_var[, i] = matrix(rep(sum(r[, i] * (y - est_mu_x[, i])^2)/sum(r[, i]), n), nrow = n)
    }

    rsd = likelihood - sum(log(apply(f * est_p, 1, sum)))
    likelihood = sum(log(apply(f * est_p, 1, sum)))

    # Convergence check
    if (abs(rsd) < acc | iter > 200) {
      break
    }
  }

  out = list(est_p = est_p, est_var = est_var)
  return(out)
}


# Perform step 3 of the backfitting algorithm
backfitting_step3 <- function(x, y, u, est_p, est_mu_x, est_var, h) {
  n = dim(est_mu_x)[1]  # Number of observations
  numc = dim(est_mu_x)[2]  # Number of components
  m = length(u)  # Number of grid points

  r = matrix(numeric(n * numc), nrow = n)
  f = r
  est_mu_u = matrix(numeric(m * numc), nrow = m)

  rsd = 1
  likelihood = 1
  iter = 0
  acc = 10^(-3)

  repeat {
    iter = iter + 1

    # E-step: Calculate the conditional density
    for (i in 1:numc) {
      f[, i] = stats::dnorm(y, est_mu_x[, i], est_var[, i]^0.5)
    }

    # M-step: Update the conditional density
    for (i in 1:numc) {
      r[, i] = est_p[, i] * f[, i]/apply(f * est_p, 1, sum)

      # Update est_mu_u and est_mu_x
      # W = exp(-(((matrix(rep(t(x),m),nrow=m)-matrix(rep(u,n),ncol=n))^2)/(2*h^2)))/(h*sqrt(2*pi));
      # Updated sep20 2022 after meeting with Yan Ge.
      W = exp(-(((t(matrix(rep(x, m), nrow = m)) - matrix(rep(u, n), ncol = n))^2)/(2 * h^2)))/(h * sqrt(2 * pi)) #Updated sep20 2022 after meeting with Yan Ge.
      R = t(matrix(rep(r[, i], m), ncol = m))
      RY = t(matrix(rep(r[, i] * y, m), ncol = m))
      est_mu_u[, i] = apply(W * RY, 1, sum)/apply(W * R, 1, sum)
      est_mu_x[, i] = stats::approx(u, est_mu_u[, i], x)$y
    }

    rsd = likelihood - sum(log(apply(f * est_p, 1, sum)))
    likelihood = sum(log(apply(f * est_p, 1, sum)))

    # Convergence check
    if (abs(rsd) < acc | iter > 200) {
      break
    }
  }

  out = list(est_mu_x = est_mu_x, est_mu_u = est_mu_u, lh = likelihood)
  return(out)
}

#' Semiparametric Mixtures of Nonparametric Regressions with Global EM-type Algorithm
#'
#' `semimrGlobal' is used to estimate a mixture of regression models, where the mixing proportions
#' and variances remain constant, but the component regression functions are smooth functions (\eqn{m(\cdot)})
#' of a covariate. The model is expressed as follows:
#' \deqn{\sum_{j=1}^C\pi_j\phi(y|m(x_j),\sigma^2_j).}
#' This function provides the one-step backfitting estimate using the global EM-type algorithm (GEM) (Xiang and Yao, 2018).
#' As of version 1.1.0, this function supports a two-component model.
#'
#' @usage
#' semimrGlobal(x, y, u = NULL, h = NULL, ini = NULL)
#'
#' @param x a vector of covariate values.
#' @param y a vector of response values.
#' @param u a vector of grid points for spline method to estimate the proportions.
#'   If NULL (default), 100 equally spaced grid points are automatically generated
#'   between the minimum and maximum values of x.
#' @param h bandwidth for the nonparametric regression. If NULL (default), the bandwidth is
#'   calculated based on the method of Botev et al. (2010).
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values
#'   using regression spline approximation. If specified, it can be a list with the form of
#'   \code{list(pi, mu, var)}, where
#'   \code{pi} is a vector of length 2 of mixing proportions,
#'   \code{mu} is a length(x) by 2 matrix of component means, and
#'   \code{var} is a vector of length 2 of component variances.
#'
#' @return A list containing the following elements:
#'   \item{pi}{vector of length 2 of estimated mixing proportions.}
#'   \item{mu}{length(x) by 2 matrix of estimated mean functions at x, \eqn{m(x)}.}
#'   \item{mu_u}{length(u) by 2 matrix of estimated mean functions at grid point u, \eqn{m(u)}.}
#'   \item{var}{vector of length 2 estimated component variances.}
#'   \item{lik}{final likelihood.}
#' @seealso \code{\link{semimrLocal}}, \code{\link{semimrGen}}
#'
#' @references
#'   Xiang, S. and Yao, W. (2018). Semiparametric mixtures of nonparametric regressions.
#'   Annals of the Institute of Statistical Mathematics, 70, 131-154.
#'
#'   Botev, Z. I., Grotowski, J. F., and Kroese, D. P. (2010). Kernel density estimation via diffusion.
#'   The Annals of Statistics, 38(5), 2916-2957.
#'
#' @export
#' @examples
#' # produce data that matches the description using semimrGen function
#' # true_mu = (4 - sin(2 * pi * x), 1.5 + cos(3 * pi * x))
#' n = 100
#' u = seq(from = 0, to = 1, length = 100)
#' true_p = c(0.3, 0.7)
#' true_var = c(0.09, 0.16)
#' out = semimrGen(n, true_p[1], true_var, u)
#'
#' x = out$x
#' y = out$y
#' true_mu = out$true_mu
#' true = list(true_p = true_p, true_mu = true_mu, true_var = true_var)
#'
#' # estimate parameters using semimrGlobal function.
#' est = semimrGlobal(x, y)
semimrGlobal <- function(x, y, u = NULL, h = NULL, ini = NULL) {

  if (is.null(u)) {
    u = seq(from = min(x), to = max(x), length = 100)
  }
  if (is.null(h)) {
    h = kdebw(x, 2^14)
  }

  if (is.null(ini)) {
    IDX = stats::kmeans(cbind(x, y), 2)$cluster
    ini_p = rbind(2 - IDX, IDX - 1)
    ini = list(p = t(ini_p))
    ini = mixbspline(u, x, y, ini)
  }

  n = length(y)  # Number of observations
  est_mu_x = ini$mu
  est_p = ini$pi
  est_p = cbind(rep(est_p[1], n), rep(est_p[2], n))
  est_var = ini$var
  est_var = cbind(rep(est_var[1], n), rep(est_var[2], n))

  n = dim(est_mu_x)[1]
  numc = dim(est_mu_x)[2]
  m = length(u)

  r = matrix(numeric(n * numc), nrow = n)
  f = r
  est_mu_u = matrix(numeric(m * numc), nrow = m)

  rsd = 1
  likelihood = 1
  iter = 0
  acc = 10^(-3)

  repeat {
    iter = iter + 1

    # E-step: Calculate the conditional density
    for (i in 1:numc) {
      f[, i] = stats::dnorm(y, est_mu_x[, i], est_var[, i]^0.5)
    }

    # M-step: Update the conditional density
    for (i in 1:numc) {
      r[, i] = est_p[, i] * f[, i]/apply(f * est_p, 1, sum)
      est_p[, i] = matrix(rep(sum(r[, i])/n, n), nrow = n)
      est_var[, i] = matrix(rep(sum(r[, i] * (y - est_mu_x[, i])^2)/sum(r[, i]), n), nrow = n)

      # Update est_mu_u and est_mu_x
      W = exp(-(((t(matrix(rep(x, m), nrow = m)) - matrix(rep(u, n),ncol = n))^2)/(2 * h^2)))/(h * sqrt(2 * pi))
      R = t(matrix(rep(r[, i], m), ncol = m))
      RY = t(matrix(rep(r[, i] * y, m), ncol = m))
      est_mu_u[, i] = apply(W * RY, 1, sum)/apply(W * R, 1, sum)
      est_mu_x[, i] = stats::approx(u, est_mu_u[, i], x)$y
    }

    rsd = likelihood - sum(log(apply(f * est_p, 1, sum)))
    likelihood = sum(log(apply(f * est_p, 1, sum)))

    # Convergence check
    if (abs(rsd) < acc | iter > 200) {
      break
    }
  }

  out = list(pi = est_p[1, ], mu = est_mu_x, var = est_var[1, ], mu_u = est_mu_u, lik = likelihood) #SK--9/6/2023

  return(out)
}

#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
#' Mixture of Nonparametric Regressions Using B-spline
#'
#' This function is used to fit mixture of nonparametric regressions using b-spline
#' assuming the error is normal and the variance of the errors are equal
#'
#' @references
#' Dziak, J. J., Li, R., Tan, X., Shiffman, S., & Shiyko, M. P. (2015). Modeling intensive longitudinal data with mixtures of nonparametric trajectories and time-varying effects. Psychological methods, 20(4), 444.
#'
#' @param u vector of grid points.
#' @param x the predictors we observed; needs to be standardized;
#' @param y the observation we observed
#' @param ini initial values of proportion. If left as NULL will be calculated from mixture of the linear regression
#' with normal error assumption.
#' @param C number of component, the default value is 2;
#' @param mm order of spline, default is cubic spline with mm=4;
#' @param tt the knots; default is quantile(x,seq(0.05,0.95,0.2))
#' @param a the interval that contains x; default is c\eqn{(mean(x)-2*sqrt(var(x)),mean(x)+2*sqrt(var(x)))};
#'
#' @return mu_u:estimated mean functions with interpolate;est_mu: estimated mean functions,est_p: estimated proportion;est_var: estimated variance
#'
#' @noRd
mixbspline <- function(u, x, y, ini = NULL, C = NULL, mm = NULL, tt = NULL, a = NULL) {
  # Set default values if not provided
  if (is.null(C)) {
    k <- 2
  } else {
    k <- C
  }

  if (is.null(mm)) {
    mm <- 4
  }

  if (is.null(tt)) {
    tt <- stats::quantile(x, seq(0.05, 0.95, 0.2))
  }

  if (is.null(a)) {
    a <- c(mean(x) - 2 * sqrt(stats::var(x)), mean(x) + 2 * sqrt(stats::var(x)))
  }

  if (is.null(ini)) {
    ini <- mixlin_smnr(x, y, k)
  }

  t <- c(rep(a[1], mm), rep(a[2], mm))
  K <- length(tt)
  BX <- bspline_basis(1, mm, t, x)
  BXu <- bspline_basis(1, mm, t, u)

  for (i in 2:(mm + K)) {
    BX <- cbind(BX, bspline_basis(1, mm, t, x))
    BXu <- cbind(BXu, bspline_basis(1, mm, t, u))
  }

  # Fit the model
  fit <- mixlin_smnr(BX, y, k, ini, FALSE)
  #fit <- mixlin_smnr(BX, y, k, ini, 0)

  out <- list(
    mu_u = BXu %*% fit$beta,
    mu = BX %*% fit$beta,
    pi = fit$pi,
    var = (fit$sigma)^2
  )

  return(out)
}

#is used to fit mixture of the linear regression
#assuming the error is normal
mixlin_smnr <- function(x, y, k = NULL, ini = NULL, addintercept = TRUE) {
  n <- length(y)
  x <- as.matrix(x)

  if (dim(x)[1] < dim(x)[2]) {
    x <- t(x)
    y <- t(y)
  }

  # Set default values if not provided
  if (is.null(k)) {
    k <- 2
  }

  #SK-- 9/4/2023
  if (addintercept) {
    X <- cbind(rep(1, n), x)
  } else {
    X <- x
  }
  #if (is.null(intercept)) {
  #  intercept <- 1
  #}
  #
  #if (intercept == 1) {
  #  X <- cbind(rep(1, n), x)
  #} else {
  #  X <- x
  #
  #}

  if (is.null(ini)) {
    ini <- mixlin_smnrveq(x, y, k)
  }

  p <- ini$p

  stop <- 0
  numiter <- 0
  dif <- 10
  difml <- 10
  ml <- 10^8
  dif1 <- 1
  beta <- matrix(rep(0, dim(X)[2] * k), ncol = k)
  e <- matrix(rep(0, n * k), nrow = n)
  sig <- numeric()

  for (j in 1:k) {
    temp <- t(X) %*% diag(p[, j])
    beta[, j] <- MASS::ginv(temp %*% X + 10^(-5) * diag(dim(X)[2])) %*% temp %*% y
    e[, j] <- y - X %*% beta[, j]
    sig[j] <- sqrt(p[, j] %*% (e[, j]^2) / sum(p[, j]))
  }

  prop <- apply(p, 2, sum) / n
  acc <- 10^(-3) * mean(mean(abs(beta)))

  while (stop == 0) {
    denm <- matrix(rep(0, n * k), nrow = n)

    for (j in 1:k) {
      denm[, j] <- stats::dnorm(e[, j], 0, sig[j])
    }

    f <- apply(prop * denm, 1, sum)
    prebeta <- beta

    numiter <- numiter + 1
    lh[numiter] <- sum(log(f))

    if (numiter > 2) {
      temp <- lh[numiter - 1] - lh[numiter - 2]

      if (temp == 0) {
        dif <- 0
      } else {
        c <- (lh[numiter] - lh[numiter - 1]) / temp
        preml <- ml
        ml <- lh[numiter - 1] + (lh[numiter] - lh[numiter - 1]) / (1 - c)
        dif <- ml - lh[numiter]
        difml <- abs(preml - ml)
      }
    }

    if (dif < 0.001 && difml < 10^(-6) || numiter > 5000 || dif1 < acc) {
      stop = 1
    } else {
      p <- prop * denm / matrix(rep(f, k), ncol = k)

      for (j in 1:k) {
        temp <- t(X) %*% diag(p[, j])
        beta[, j] <- MASS::ginv(temp %*% X) %*% temp %*% y
        e[, j] <- y - X %*% beta[, j]
        sig[j] <- sqrt(t(p[, j]) %*% (e[, j]^2) / sum(p[, j]))
      }

      prop <- apply(p, 2, sum) / n
      dif1 <- max(abs(beta - prebeta))
    }

    if (min(sig) / max(sig) < 0.001 || min(prop) < 1 / n || is.nan(min(beta[, 1]))) {
      ini <- list(beta = beta, sig = sig, prop = prop)
      res <- mixlin_smnrveq(x, y, k, ini, FALSE)
      out <- list(beta = res$beta, sig = res$sig, pi = res$pi, p = p, id = 0, lh = lh[numiter])
    } else {
      out <- list(beta = beta, sigma = sig, pi = prop, p = p, id = 1, lh = lh[numiter])
    }
  }

  return(out)
}

#is used to fit mixture of the linear regression
#assuming the error is normal and the variance of the errors are equal
mixlin_smnrveq <- function(x, y, k = NULL, ini = NULL, addintercept = TRUE) {
  n <- length(y)

  if (dim(x)[1] < dim(x)[2]) {
    x <- t(x)
    y <- t(y)
  }

  # Set default values if not provided
  if (is.null(k)) {
    k <- 2
  }

  #SK-- 9/4/2023
  if (addintercept) {
    X <- cbind(rep(1, n), x)
  } else {
    X <- x
  }
  #if (is.null(intercept)) {
  #  intercept <- 1
  #}
  #
  #if (intercept == 1) {
  #  X <- cbind(rep(1, n), x)
  #} else {
  #  X <- x
  #
  #}

  if (is.null(ini)) {
    ini <- mixmnorm(cbind(x, y), k, 0)
    prop <- ini$pi
    p <- t(ini$p)
    beta <- matrix(rep(0, dim(X)[2] * k), ncol = k)
    e <- matrix(rep(0, n * k), nrow = n)
    sig <- numeric()

    for (j in 1:k) {
      temp <- t(X) %*% diag(p[, j])
      beta[, j] <- MASS::ginv(temp %*% X) %*% temp %*% y
      e[, j] <- y - X %*% beta[, j]
      sig[j] <- sqrt(t(p[, j]) %*% (e[, j]^2) / sum(p[, j]))
    }

    sig <- rep(sqrt(sum(e^2 * p) / n), k)
    ini$beta <- beta
    ini$sigma <- sig
  } else {
    beta <- ini$beta
    sig <- ini$sig
    prop <- ini$prop
    denm <- matrix(rep(0, n * k), nrow = n)

    for (j in 1:k) {
      e[, j] <- y - X %*% beta[, j]
      denm[, j] <- stats::dnorm(e[, j], 0, sig[j])
    }

    f <- apply(prop * denm, 1, sum)
    p <- prop * denm / matrix(rep(f, k), ncol = k)
  }

  stop <- 0
  numiter <- 0
  dif <- 10
  difml <- 10
  ml <- 10^8

  while (stop == 0) {
    denm <- matrix(rep(0, n * k), nrow = n)

    for (j in 1:k) {
      denm[, j] <- stats::dnorm(e[, j], 0, sig[j])
    }

    f <- apply(prop * denm, 1, sum)
    numiter <- numiter + 1
    lh[numiter] <- sum(log(f))

    if (numiter > 2) {
      temp <- lh[numiter - 1] - lh[numiter - 2]

      if (temp == 0) {
        dif <- 0
      } else {
        c <- (lh[numiter] - lh[numiter - 1]) / temp
        preml <- ml
        ml <- lh[numiter - 1] + (lh[numiter] - lh[numiter - 1]) / (1 - c)
        dif <- ml - lh[numiter]
        difml <- abs(preml - ml)
      }
    }

    if (dif < 0.001 && difml < 10^(-6) || numiter > 5000) {
      stop = 1
    } else {
      p <- prop * denm / matrix(rep(f, k), ncol = k)

      for (j in 1:k) {
        temp <- t(X) %*% diag(p[, j])
        beta[, j] <- MASS::ginv(temp %*% X) %*% temp %*% y
        e[, j] <- y - X %*% beta[, j]
      }

      prop <- apply(p, 2, sum) / n
      sig <- rep(sqrt(sum(e^2 * p / n)), k)
    }

    if (min(prop) < 1 / n || is.nan(min(beta[, 1]))) {
      out <- ini
    } else {
      out <- list(beta = beta, sigma = sig, pi = prop, p = p, ind = 1)
    }
  }

  return(out)
}

bspline_basis <- function(i, m, t, x) {
  x <- as.matrix(x, nrow = length(x))
  B <- matrix(rep(0, dim(x)[1] * dim(x)[2]), nrow = dim(x)[1])

  if (m > 1) {
    b1 <- bspline_basis(i, m - 1, t, x)
    dn1 <- x - t[i]
    dd1 <- t[i + m - 1] - t[i]

    if (dd1 != 0) {
      B <- B + b1 * (dn1 / dd1)
    }

    b2 <- bspline_basis(i + 1, m - 1, t, x)
    dn2 <- t[i + m] - x
    dd2 <- t[i + m] - t[i + 1]

    if (dd2 != 0) {
      B <- B + b2 * (dn2 / dd2)
    }
  } else {
    # B = (t[i] <= x[1] && x[1] < t[i+1])
    B <- (t[i] <= x) & (x < t[i + 1])  # in Shen updated on sep23 2022 after checking with Yan Ge
  }

  return(B)
}

kdebw <- function(y, n = NULL, MIN = NULL, MAX = NULL) {
  # Check and set default values for n, MIN, and MAX
  if (is.null(n)) {
    n = 2^12
  }
  n = 2^ceiling(log2(n))

  if (is.null(MIN) || is.null(MAX)) {
    minimum = min(y)
    maximum = max(y)
    Range = maximum - minimum
    MIN = minimum - Range / 10
    MAX = maximum + Range / 10
  }

  # Set up the grid over which the density estimate is computed
  R = MAX - MIN
  dx = R / (n - 1)
  xmesh = MIN + seq(from = 0, to = R, by = dx)
  N = length(y)

  # Bin the data uniformly using the grid defined above
  initial_data = graphics::hist(y, xmesh, plot = FALSE)$counts / N
  a = dct1d(initial_data)  # Discrete cosine transform of initial data
  I = as.matrix(c(1:(n - 1))^2)
  a2 = as.matrix((a[2:length(a)] / 2)^2)

  # Define and find bandwidth using optimization
  ft2 <- function(t) {
    y = abs(fixed_point(t, I, a2, N) - t)
  }
  t_star = stats::optimize(ft2, c(0, 0.1))$minimum

  bandwidth = sqrt(t_star) * R

  return(bandwidth)
}

fixed_point <- function(t, I, a2, N) {

  # Define a nested function for the functional to find the fixed point
  functional <- function(df, s) {
    K0 = prod(seq(1, 2 * s - 1, 2)) / sqrt(2 * pi)
    const = (1 + (1/2)^(s + 1/2)) / 3
    t = (2 * const * K0 / N / df)^(2 / (3 + 2 * s))
    f = 2 * pi^(2 * s) * sum(I^s * a2 * exp(-I * pi^2 * t))
    return(f)
  }

  # Initialize an empty vector f
  f = numeric()

  # Compute f for s = 5 using a specific formula
  f[5] = 2 * pi^10 * sum(I^5 * a2 * exp(-I * pi^2 * t))

  # Compute f for s from 4 down to 2 using the functional
  for (s in seq(4, 2, -1)) {
    f[s] = functional(f[s + 1], s)
  }

  # Calculate time and find the fixed point
  time = (2 * N * sqrt(pi) * f[2])^(-2/5)
  out = (t - time) / time

  return(out)
}

dct1d <- function(data) {

  # Append a zero to the end of the data
  data = c(data, 0)

  # Get the length of the data
  n = length(data)

  # Define the weight vector
  weight = c(1, 2 * exp(-1i * seq(1, (n - 1), 1) * pi / (2 * n)))

  # Reorder the data
  data = c(data[seq(1, n, 2)], data[seq(n, 2, -2)])

  # Compute
  out = Re(weight * stats::fft(data))

  return(out)
}
