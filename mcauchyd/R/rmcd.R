rmcd <- function(n, mu, Sigma, tol = 1e-6) {
  #' Simulate from a Multivariate Cauchy Distribution
  #'
  #' Produces one or more samples from the multivariate (\eqn{p} variables) Cauchy distribution (MCD)
  #' with location parameter \code{mu} and scatter matrix \code{Sigma}.
  #'
  #' @aliases rmcd
  #'
  #' @usage rmcd(n, mu, Sigma, tol = 1e-6)
  #' @param n integer. Number of observations.
  #' @param mu length \eqn{p} numeric vector. The location parameter.
  #' @param Sigma symmetric, positive-definite square matrix of order \eqn{p}. The scatter matrix.
  #' @param tol tolerance for numerical lack of positive-definiteness in Sigma (for \code{mvrnorm}, see Details).
  #' @return A matrix with \eqn{p} columns and \eqn{n} rows.
  #' 
  #' @details A sample from a MCD with parameters \eqn{\boldsymbol{\mu}} and \eqn{\Sigma}
  #' can be generated using:
  #' \deqn{\displaystyle{\mathbf{X} = \boldsymbol{\mu} + \frac{\mathbf{Y}}{\sqrt{u}}}}
  #' where \eqn{\mathbf{Y}} is a random vector distributed among a centered Gaussian density
  #' with covariance matrix \eqn{\Sigma} (generated using \code{\link[MASS]{mvrnorm}})
  #' and \eqn{u} is distributed among a Chi-squared distribution with 1 degree of freedom.
  #'
  #' @author Pierre Santagostini, Nizar Bouhlel
  #' 
  #' @seealso \code{\link{dmcd}}: probability density of a MCD.
  #'
  #' @examples
  #' mu <- c(0, 1, 4)
  #' sigma <- matrix(c(1, 0.6, 0.2, 0.6, 1, 0.3, 0.2, 0.3, 1), nrow = 3)
  #' x <- rmcd(100, mu, sigma)
  #' x
  #' apply(x, 2, median)
  #'
  #' @importFrom MASS mvrnorm
  #' @importFrom stats rchisq
  #' @export

  # Number of variables
  p <- length(mu)
  
  # Sigma must be a matrix
  if (is.numeric(Sigma) & !is.matrix(Sigma))
    Sigma <- as.matrix(Sigma)
  
  # Sigma must be a square matricx with p rows and p columns
  if (nrow(Sigma) != p | ncol(Sigma) != p)
    stop("Sigma must be a square matrix with size equal to length(mu).")

  # IS Sigma symmetric?
  if (!isSymmetric(Sigma))
    stop("Sigma must be a symmetric, positive-definite matrix.")

  # Eigenvalues and eigenvectors of Sigma
  eig <- eigen(Sigma, symmetric = TRUE)
  lambda <- eig$values

  # Is Sigma positive-definite?
  if (any(lambda < tol * max(abs(lambda))))
    stop("Sigma must be a symmetric, positive-definite matrix.")
  
  # Computation of the density
  x <- rchisq(n, 1)
  result <- mvrnorm(n, rep(0, p), Sigma, tol = tol)
  result <- matrix(mu, nrow = n, ncol = p, byrow = TRUE) + result/sqrt(x)
  
  return(result)
}
