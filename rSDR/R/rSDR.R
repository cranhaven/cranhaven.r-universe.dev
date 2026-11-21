#' Robust Sufficient Dimension Reduction
#' @description
#' Robust Sufficient Dimension Reduction with alpha-Distance Covariance and Stiefel Manifold Learning for supervised dimension reduction.
#' @usage rSDR(X, Y, d, alpha=0.5,maxiter=1000,tol=1e-7)
#' @param X an \eqn{n \times p} numeric matrix, where \eqn{n} is the number of observations and \eqn{p} is the number of variable.
#' @param Y an \eqn{n \times k} numeric response matrix, where \eqn{k (\geq 1)} is the number of response variables.
#' @param d the number of reduced dimension.
#' @param alpha this parameter represents the exponent applied to the Euclidean distance in the computation of distance covariance. When alpha=1, it corresponds to the classical distance covariance. When 0 < alpha < 1, it is a more robust version by reducing the influence of large values in the distance matrices.
#' @param maxiter maxiter is the maximum number of iterations allowed for the solver (a non-negative integer). See the Max_Iteration parameter in \code{\link[ManifoldOptim]{get.solver.params}} for details.
#' @param tol tol is used to assess convergence, see the Tolerance parameter in \code{\link[ManifoldOptim]{get.solver.params}} for details.
#'
#'
#' @return The returned value is an object of class "rSDR", containing the following components:
#'  \describe{
#'   \item{projected_data}{an \eqn{n \times d} matrix representing the projected data using the rSDR method.}
#'   \item{beta}{a \eqn{p \times d} matrix.Solve \eqn{\boldsymbol{\beta}} by \eqn{\textbf{C}=\Sigma_x^{1/2} \boldsymbol{\beta}}}
#'   \item{C_value}{an optimal of C is obtained by maximizing the target function using ManifoldOptim method.}
#'   \item{f_value}{The value of cost function f is defined as the negative of the target function.}
#' }
#'
#' @export
#' @references
#'  Hsin-Hsiung Huang, Feng Yu & Teng Zhang (19 Feb 2024): Robust sufficient dimension reduction via alpha-distance covariance, Journal of Nonparametric Statistics, DOI:10.1080/10485252.2024.2313137
#'
#' @importFrom stats cov dist
#' @examples
#'
#' library(ManifoldOptim)
#' library(rSDR)
#' utils::data("ionosphere", package = "fdm2id")
#' X<-as.matrix(ionosphere[,c(1:33)])
#' Y<-ifelse(ionosphere[,34]=='b',0,1)
#' Y<-matrix(Y,length(Y),1)
#' set.seed(2435)
#' \donttest{
#' sdr_result<-rSDR(X=X, Y=Y, d=3, alpha=0.3,maxiter=1000,tol=1e-7)
#' }
rSDR <- function(X, Y, d, alpha=0.5,maxiter=1000,tol=1e-7) {

  if (!is.matrix(X)){
    stop('X should be a matrix.')
  }

  if (!(is.matrix(Y)&is.numeric(Y))){
    stop('Y should be a matrix.')
  }

  # Extract dimensions
  n <- nrow(X)
  p <- ncol(X)

  # Initial matrix C for the problem
  # Initial C on Stiefel(p, d)
  # init is of size p x d
  init <- rstiefel::rustiefel(p, d)  # Generate a random orthonormal matrix (C'C=CC'=I) for initialization

  # Ensure n >= p
  if (n < p) {
    stop("The dimension n must be larger than the dimension p.")
  }

  # Pairwise distances for Y, then take alpha power
  PDY <- as.matrix(stats::dist(Y, method = "euclidean"))
  #update the power of euclidean distance
  KY <- PDY^alpha  # alpha = 1 for simplicity

  # FIX: double centering via H K H
  H <- diag(n) - matrix(1, n, n) / n
  B<-H%*%KY%*%H

  # X covariance and its matrix sqrt and inverse sqrt, with ridge for stability
  N <- stats::cov(X)
  eig <- eigen(N, symmetric = TRUE)
  lam <- pmax(eig$values, 1e-10)
  Nhalf     <- eig$vectors %*% (diag(sqrt(lam), p, p)) %*% t(eig$vectors)
  Nhalf_inv <- eig$vectors %*% (diag(1 / sqrt(lam), p, p)) %*% t(eig$vectors)

  # Standardize X
  Z <- X %*% Nhalf_inv   # n x p

  # Define cost function and gradient
  # Cost function
  cost_store <- function(C, Z, B, alpha, store = list()) {
    ZC <- Z %*% C # n x d
    #ZC pairwise distance
    PDZC <- as.matrix(stats::dist(ZC)) # pairwise distances
    f <- -mean((PDZC^alpha) * B)
    store$PDZC <- PDZC
    store$ZC   <- ZC
    return(list(f = f, store = store))
  }

  #gradient
  eGrad_store <- function(C, Z, B, alpha, store) {
    PDZC <- store$PDZC
    # derivative of ||x||^alpha wrt x uses (||x||^2 + eps)^((alpha-2)/2)
    eps <- 1e-10
    coef <- alpha * (PDZC^2 + eps)^((alpha - 2) / 2) * B   # n x n
    # Graph Laplacian style term: L = diag(colSums(coef)) - coef
    L <- diag(colSums(coef)) - coef
    G <- -2 * t(Z) %*% (L %*% Z %*% C) / (n^2)
    return(list(G = G, store = store))
  }

  # Modify the cost and gradient functions to accept vector inputs
  f <- function(c_vec) {
    C <- matrix(c_vec, nrow = p, ncol = d)
    cost_store(C, Z, B, alpha)$f  # Assuming alpha = 1
  }

  g <- function(c_vec) {
    C <- matrix(c_vec, nrow = p, ncol = d)
    G <- eGrad_store(C, Z, B, alpha, cost_store(C, Z, B, alpha)$store)$G
    as.numeric(G)  # Flatten the gradient matrix to a vector
  }

  # Create the problem object using RProblem class
  mod <- Rcpp::Module("ManifoldOptim_module", PACKAGE = "ManifoldOptim")
  prob <- methods::new(mod$RProblem, f, g)

  # Initial point for optimization (flattened to a vector)
  x0 <- as.numeric(init)  # initial point

  # Define the manifold using stiefel_factory (if necessary)
  mani.defn <- ManifoldOptim::get.stiefel.defn(p, d)

  # Set solver parameters
  solver.params <- ManifoldOptim::get.solver.params()
  solver.params$Max_Iteration <- maxiter
  solver.params$Tolerance <- tol

  # Perform optimization
  result <- ManifoldOptim::manifold.optim(
    prob = prob,
    mani.defn = mani.defn,
    method = "RCG", # RCG for Riemannian Conjugate Gradient
    x0 = x0,
    solver.params = solver.params
  )

  # Extract C (pxd) from results
  C_value <- matrix(result$xopt, nrow = p, ncol = d)
  #Extract a value of f from results
  #f_value<-f(C_value)
  f_value<-result$fval

  ##Since C=sigma_x^(0.5)*beta, beta=sigma_x^(-0.5)%*%C from equation (4)
  beta <- Nhalf_inv %*% C_value

# This projects X onto the estimated SDR directions
  projected_data<-as.data.frame(X %*% beta )

  # Return the result
  return(list(projected_data=projected_data,beta = beta,C_value=C_value,f_value=f_value))

}
