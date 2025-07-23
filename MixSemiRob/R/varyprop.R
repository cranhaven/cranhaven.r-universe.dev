#' Varying Proportion Mixture Data Generator
#'
#' `mixregPvaryGen' is used to generate a mixture of normal distributions with varying proportions:
#' \deqn{Y|_{\boldsymbol{x},Z=z} \sim \sum_{c=1}^C\pi_c(z)N(\boldsymbol{x}^{\top}\boldsymbol{\beta}_c,\sigma_c^2).}
#' See \code{\link{mixregPvary}} for details.
#'
#' @usage
#' mixregPvaryGen(n, C = 2)
#'
#' @param n number of observations.
#' @param C number of mixture components. Default is 2.
#'
#' @return A list containing the following elements:
#'   \item{x}{vector of length n.}
#'   \item{y}{vector of length n.}
#'   \item{true_p}{n by C matrix of probabilities of an observations belonging to each component.}
#'
#' @seealso \code{\link{mixregPvary}}
#'
#' @export
#' @examples
#' mixregPvaryGen(n = 100, C = 2)
mixregPvaryGen <- function(n, C = 2) {

  # Set the number of components
  numc = C

  # Generate random 'x' values
  x = stats::runif(n, 0, 1)

  # True probabilities for each component
  true_p = matrix(c(0.1 + 0.8 * sin(pi * x), 1 - 0.1 - 0.8 * sin(pi * x)), ncol = 2)

  # Cumulative probabilities for each component
  cum_p = matrix(c(0.1 + 0.8 * sin(pi * x), rep(1, n)), ncol = 2)

  # Generate random 'p' values
  gen_p = stats::runif(n, 0, 1)

  # Create a matrix to represent Bernoulli samples for each component
  bernulli = matrix(rep(0, n * numc), nrow = n)
  bernulli[, 1] = (gen_p < cum_p[, 1])

  # Generate Bernoulli samples for each component
  for (i in 2:numc) {
    bernulli[, i] = (cum_p[, i - 1] < gen_p) * (gen_p < cum_p[, i])
  }

  # Define error terms for each component
  e1 = 0.3
  e2 = 0.4

  # Generate component-specific functions with error
  func1 = 4 - 2 * x + e1 * stats::rnorm(n, 0, 1)
  func2 = 3 * x + e2 * stats::rnorm(n, 0, 1)

  # Combine component functions based on Bernoulli samples
  func = matrix(c(func1, func2), ncol = 2)

  # Calculate 'y' as the sum of component functions
  y = apply(func * bernulli, 1, sum)

  # Create the output list
  out = list(x = x, y = y, true_p = true_p)

  return(out)
}

#' Mixture of Regression Models with Varying Mixing Proportions
#'
#' `mixregPvary' is used to estimate a mixture of regression models with varying proportions:
#' \deqn{Y|_{\boldsymbol{x},Z=z} \sim \sum_{c=1}^C\pi_c(z)N(\boldsymbol{x}^{\top}\boldsymbol{\beta}_c,\sigma_c^2).}
#' The varying proportions are estimated using a local constant regression method (kernel regression).
#'
#' @usage
#' mixregPvary(x, y, C = 2, z = NULL, u = NULL, h = NULL,
#'              kernel = c("Gaussian", "Epanechnikov"), ini = NULL)
#'
#' @param x an n by p matrix of explanatory variables. The intercept will be automatically added to x.
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param z a vector of a variable with varying-proportions. It can be any of the variables in \code{x}.
#'   Default is NULL, and the first variable in \code{x} will be used.
#' @param u a vector of grid points for the local constant regression method to estimate the proportions.
#'   If NULL (default), 100 equally spaced grid points are automatically generated
#'   between the minimum and maximum values of z.
#' @param h bandwidth for kernel density estimation. If NULL (default), the bandwidth is
#'   calculated based on the method of Botev et al. (2010).
#' @param kernel character, determining the kernel function used in local constant method:
#'   \code{Gaussian} or \code{Epanechnikov}. Default is \code{Gaussian}.
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values
#'   using the \code{\link{regmixEM}} function of the `mixtools' package.
#'   If specified, it can be a list with the form of
#'   \code{list(pi, beta, var)}, where
#'   \code{pi} is a vector of length C of mixing proportions,
#'   \code{beta} is a (p + 1) by C matrix for component regression coefficients, and
#'   \code{var} is a vector of length C of component variances.
#'
#' @return A list containing the following elements:
#'   \item{pi_u}{length(u) by C matrix of estimated mixing proportions at grid points.}
#'   \item{pi_z}{n by C matrix of estimated mixing proportions at z.}
#'   \item{beta}{(p + 1) by C matrix of estimated component regression coefficients.}
#'   \item{var}{C-dimensional vector of estimated component variances.}
#'   \item{loglik}{final log-likelihood.}
#'
#' @seealso \code{\link{mixregPvaryGen}}
#'
#' @references
#'   Huang, M. and Yao, W. (2012). Mixture of regression models with varying mixing proportions:
#'   a semiparametric approach. Journal of the American Statistical Association, 107(498), 711-724.
#'
#'   Botev, Z. I., Grotowski, J. F., and Kroese, D. P. (2010). Kernel density estimation via diffusion.
#'   The Annals of Statistics, 38(5), 2916-2957.
#'
#' @importFrom mixtools regmixEM
#' @export
#' @examples
#' n = 100
#' C = 2
#' u = seq(from = 0, to = 1, length = 100)
#' true_beta = cbind(c(4, - 2), c(0, 3))
#' true_var = c(0.09, 0.16)
#' data = mixregPvaryGen(n, C)
#' x = data$x
#' y = data$y
#' est = mixregPvary(x, y, C, z = x, u, h = 0.08)
mixregPvary <- function(x, y, C = 2, z = NULL, u = NULL, h = NULL,
                         kernel = c("Gaussian", "Epanechnikov"), ini = NULL) {

  # Match arguments
  kernel <- match.arg(kernel)

  n = length(y)
  X = cbind(rep(1, n), x)
  numc = C

  # Set default values for missing arguments
  if (is.null(numc)) {
    numc = 2
  }
  if (is.null(z)) {
    z = X[, 2]
  }
  if (is.null(u)) {
    u = seq(from = min(z), to = max(z), length = 100)
  }
  if (is.null(h)) {
    h = kdebw(x, 2^14)
  }
  #if (is.null(kernel)) {
  #  kernel = 1
  #}

  # Initialize parameters if 'ini' is not provided
  if (is.null(ini)) {
    ini = mixtools::regmixEM(y, x)
    est_p_z = ini$lambda
    est_p_z = cbind(rep(est_p_z[1], n), rep(est_p_z[2], n))
    est_beta = ini$beta
    est_var = ini$sigma
    est_var = est_var^2
  } else {
    est_p_z = ini$pi
    est_p_z = cbind(rep(est_p_z[1], n), rep(est_p_z[2], n))
    est_beta = ini$beta
    est_var = ini$var
  }

  m = length(u)
  est_p_u = matrix(rep(0, m * numc), nrow = m)
  f = matrix(rep(0, n * numc), nrow = n)
  r = f

  rsd = 1
  likelihood = 1
  iter = 0
  acc = 10^(-5)

  repeat {
    iter = iter + 1

    # E-step
    for (i in 1:numc) {
      f[, i] = stats::dnorm(y, X %*% est_beta[, i], est_var[i]^0.5 * rep(1, n))
    }

    # M-step
    for (i in 1:numc) {
      r[, i] = est_p_z[, i] * f[, i] / apply(f * est_p_z, 1, sum)
      s = sqrt(r[, i])
      yy = y * s
      xx = X * matrix(rep(s, 2), nrow = n)
      b = stats::coefficients(stats::lm(yy ~ xx))
      est_beta[, i] = b[-1]
      est_var[i] = sum(r[, i] * (y - X %*% est_beta[, i])^2) / sum(r[, i])

      est_p_u[, i] = local_constant(r[, i], z, u, h, kernel)
      est_p_z[, i] = stats::approx(u, est_p_u[, i], z)$y
    }

    rsd = likelihood - sum(log(apply(f * est_p_z, 1, sum)))
    likelihood = sum(log(apply(f * est_p_z, 1, sum)))

    # Check for convergence
    if (abs(rsd) < acc | iter > 200) {
      break
    }
  }

  out = list(pi_u = est_p_u, pi_z = est_p_z, beta = est_beta, var = est_var, loglik = likelihood)
  return(out)
}


#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
#y is the response; x is the preditor; u is the grid points where the unknow function is evaluated; h is the bandwidth
#mu is the output. we then run: plot(u,mu,'-')
#kernel, 1 means gaussian, 2 means epanechnikov
local_constant <- function(y, x, u, h, kernel) {
  len1 = length(x)
  len2 = length(u)

  # Create matrices for u and x for computations
  U1 = matrix(rep(u, len1), ncol = len1)
  U2 = matrix(rep(x, len2), ncol = len2)
  U = U1 - t(U2)
  t = U / h

  Y = t(matrix(rep(y, len2), ncol = len2))

  if (kernel == "Gaussian") {
    # Gaussian Kernel
    W = h^-1 * (2 * pi)^-0.5 * exp(-0.5 * t^2)
  }
  if (kernel == "Epanechnikov") {
    # Epanechnikov Kernel
    W = h^-1 * 0.75 * (1 - t^2)
    W[W < 0] = 0
  }

  # Compute the local constant estimate
  p_u = apply(W * Y, 1, sum) / apply(W, 1, sum)

  return(p_u)
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


#-------------------------------------------------------------------------------
# Keep unused functions (SK, 09/04/2023)
#-------------------------------------------------------------------------------
gen_mixreg1_boot <- function(X, beta, var, p, numc) {
  n = dim(X)[1]
  d = dim(X)[2]

  # Calculate cumulative probabilities for each component
  cum_p = c(p[, 1], rep(1, n))
  cum_p = matrix(cum_p, ncol = 2)

  # Generate random values to determine component membership
  gen_p = stats::runif(n, 0, 1)
  bernulli = matrix(rep(0, n * numc), nrow = n)
  bernulli[, 1] = (gen_p < cum_p[, 1])

  for (i in 2:numc) {
    bernulli[, i] = (cum_p[, i - 1] < gen_p) * (gen_p < cum_p[, i])
  }

  # Generate random errors for each component
  e1 = sqrt(var[1])
  e2 = sqrt(var[2])
  func1 = X %*% beta[, 1] + e1 * stats::rnorm(n, 0, 1)
  func2 = X %*% beta[, 2] + e2 * stats::rnorm(n, 0, 1)
  func = matrix(c(func1, func2), ncol = 2)

  # Calculate the mixture response variable
  y_boot = apply(func * bernulli, 1, sum)

  jieguo_boot = list(y = y_boot)
  return(jieguo_boot)
}

emest_linear <- function(x, y, numc) {
  n = length(y)
  X = cbind(rep(1, n), x)
  d = dim(X)[2]

  # Initialize estimates for mixture proportions, variances, and coefficients
  est_p = (1 / numc) * matrix(rep(1, numc), nrow = 1)
  est_var = 0.1 * matrix(rep(1, numc), nrow = 1)
  est_beta = matrix(stats::rnorm(d * numc, 0, 1), ncol = numc)

  n = length(y)
  f = matrix(numeric(n * numc), nrow = n)
  r = f

  rsd = 1
  likelihood = 1
  iter = 0
  acc = 10^(-3)

  repeat {
    iter = iter + 1

    # Calculate the likelihood of each data point belonging to a component
    for (i in 1:numc) {
      f[, i] = stats::dnorm(y, X %*% est_beta[, i], est_var[i]^0.5 * rep(1, n))
    }

    # Update component responsibilities and parameters
    for (i in 1:numc) {
      r[, i] = est_p[i] * f[, i] / (f %*% t(est_p))
      s = sqrt(r[, i])
      yy = y * s
      xx = X * matrix(rep(s, 2), nrow = n)
      b = stats::coefficients(stats::lm(yy ~ xx))
      est_beta[, i] = b[-1]
      est_var[i] = sum(r[, i] * (y - X %*% est_beta[, i])^2) / sum(r[, i])
    }

    # Update mixture proportions
    est_p = cbind(mean(r[, 1]), mean(r[, 2]))

    # Calculate residual and likelihood
    rsd = likelihood - sum(log(f %*% t(est_p)))
    likelihood = sum(log(f %*% t(est_p)))

    # Check for convergence
    if (abs(rsd) < acc | iter > 20) {
      break
    }
  }

  out = list(est_p = est_p, est_beta = est_beta, est_var = est_var)
  return(out)
}
