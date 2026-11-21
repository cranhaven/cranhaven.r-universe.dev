obs_mat <- function(nobs = 1000, p, sd = 1) {
  obs_mat <- matrix(nrow = nobs, ncol = p)
  for (i in seq(p)) {
    obs_mat[, i] <- stats::rnorm(nobs, sd = sd)
  }
  obs_mat
}

cali_Z <- function(gamma, W, e) {
  cali_Z <- W %*% gamma + e
  cali_Z
}

Z <- function(kappa, J = 3, cali_Z) {
  assert <- length(kappa) == J - 1
  if (!assert) {
    stop("kappa must be of length ", J - 1)
  }
  kappa_vec <- c(-Inf, kappa, Inf)
  Z <- findInterval(cali_Z, kappa_vec)
  Z
}

## potentially different equation for each j
y_j <- function(X_j, b_j, eta_j) {
  y_j <- X_j %*% b_j + eta_j
  y_j
}

errors <- function(Sigma, nobs = 1000) {
  assert <- Sigma[1, 1] == 1
  if (!assert) {
    stop("Sigma[1, 1] must be 1 but is ", Sigma[1, 1])
  }
  mu <- numeric(length = nrow(Sigma))
  errors <- mvtnorm::rmvnorm(n = nobs, mean = mu, sigma = Sigma)
  class(errors) <- c("errors", class(errors))
  errors
}


#' Simulate Data from an OPSR Process
#'
#' Simulates data from an ordered probit process and separate (for each regime)
#' OLS process where the errors follow a multivariate normal distribution.
#'
#' @param nobs number of observations to simulate.
#' @param sigma the covariance matrix of the multivariate normal.
#' @param seed a single value, interpreted as an integer, or `NULL` passed to
#'   [`set.seed`].
#'
#' @return Named list:
#' \item{params}{ground truth parameters.}
#' \item{data}{simulated data (as observed by the researcher). See also 'Details' section.}
#' \item{errors}{error draws from the multivariate normal (as used in the latent
#'   process).}
#' \item{sigma}{assumed covariance matrix (to generate `errors`).}
#'
#' @details
#' Three ordinal outcomes are simulated and the distinct design matrices (`W` and
#' `X`) are used (if `W == X` the model is poorely identified). Variables `ys` and
#' `xs` in `data` correspond to the selection process and `yo`, `xo` to the outcome
#' process.
#'
#' @export
opsr_simulate <- function(nobs = 1000, sigma = NULL, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ## same regressors in selection and outcome might lead to identification issues
  X <- obs_mat(nobs, p = 2, sd = 1)
  colnames(X) <- paste0("xo", 1:ncol(X))
  W <- obs_mat(nobs, p = 2, sd = 1)
  colnames(W) <- paste0("xs", 1:ncol(X))

  if (is.null(sigma)) {
    sigma <- matrix(c(
      1.0, 0.2, 0.3, 0.4,
      0.2, 1.1, 0.4, 0.5,
      0.3, 0.4, 1.2, 0.6,
      0.4, 0.5, 0.6, 1.3), ncol = 4)  # J = 3
  }
  err <- errors(sigma, nobs)

  e <- err[, 1]
  eta1 <- err[, 2]
  eta2 <- err[, 3]
  eta3 <- err[, 4]
  gamma <- c(1, 1.5)
  cali_z <- cali_Z(gamma, W, e)
  kappa <- c(-2, 1)
  z <- Z(kappa, J = 3, cali_z)

  b1 <- c(1, 2, 1.1)
  b2 <- c(1, -1, 1.5)
  b3 <- c(1, 1.6, -2)
  X_ <- cbind(1, X)
  y1 <- y_j(X_, b1, eta1)
  y2 <- y_j(X_, b2, eta2)
  y3 <- y_j(X_, b3, eta3)

  params <- list(
    gamma = gamma,
    kappa = kappa,
    beta1 = b1,
    beta2 = b2,
    beta3 = b3
  )

  data <- data.frame(
    ys = z,
    yo = ifelse(z == 1, y1, ifelse(z == 2, y2, y3)),
    W,
    X
  )

  out <- list(
    params = params,
    data = data,
    errors = err,
    sigma = sigma
  )

  out
}
