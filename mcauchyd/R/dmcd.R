dmcd <- function(x, mu, Sigma, tol = 1e-6) {
  #' Density of a Multivariate Cauchy Distribution
  #'
  #' Density of the multivariate (\eqn{p} variables) Cauchy distribution (MCD)
  #' with location parameter \code{mu} and scatter matrix \code{Sigma}.
  #'
  #' @aliases dmcd
  #'
  #' @usage dmcd(x, mu, Sigma, tol = 1e-6)
  #' @param x length \eqn{p} numeric vector.
  #' @param mu length \eqn{p} numeric vector. The location parameter.
  #' @param Sigma symmetric, positive-definite square matrix of order \eqn{p}. The scatter matrix.
  #' @param tol tolerance (relative to largest eigenvalue) for numerical lack of positive-definiteness in Sigma.
  #' @return The value of the density.
  #' 
  #' @details The density function of a multivariate Cauchy distribution is given by:
  #' \deqn{ \displaystyle{ f(\mathbf{x}|\boldsymbol{\mu}, \Sigma) = \frac{\Gamma\left(\frac{1+p}{2}\right)}{\pi^{p/2} \Gamma\left(\frac{1}{2}\right) |\Sigma|^\frac{1}{2} \left[ 1 + (\mathbf{x}-\boldsymbol{\mu})^T \Sigma^{-1} (\mathbf{x}-\boldsymbol{\mu}) \right]^\frac{1+p}{2}} } }
  #'
  #' @author Pierre Santagostini, Nizar Bouhlel
  #'
  #' @seealso \code{\link{rmcd}}: random generation from a MCD.
  #' 
  #' \code{\link{plotmcd}}, \code{\link{contourmcd}}: plot of a bivariate Cauchy density.
  #'
  #' @examples
  #' mu <- c(0, 1, 4)
  #' sigma <- matrix(c(1, 0.6, 0.2, 0.6, 1, 0.3, 0.2, 0.3, 1), nrow = 3)
  #' dmcd(c(0, 1, 4), mu, sigma)
  #' dmcd(c(1, 2, 3), mu, sigma)
  #'
  #' @export

  # Number of variables
  p <- length(mu)
  
  # Sigma must be a matrix
  if (is.numeric(Sigma) & !is.matrix(Sigma))
    Sigma <- as.matrix(Sigma)
  
  # x must have the same length as mu
  if (length(x) != p)
    stop(paste("x does not have", p, "elements.\n x and mu must have the same length."))

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
  
  # Inverse of matrix Sigma
  invSigma <- solve(Sigma)
  
  xcent <- cbind(x - mu)

  # Computation of the density
  result <- gamma((1+p)/2) / ( pi^(p/2)*gamma(0.5))
  result <- result / sqrt(det(Sigma))
  result <- result / ( 1 + t(xcent) %*% invSigma %*% xcent )^((1+p)/2)
  
  return(as.numeric(result))
}
