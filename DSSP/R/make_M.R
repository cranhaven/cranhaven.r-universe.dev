#' TPS radial basis function
#'
#' Function to compute the thin-plate splines radial basis function for internal use by the function make.M().
#' @param x is a Euclidean distance between two points.
#' @param is.even is a logical argument indicating TRUE if the dimension of the space where the thin-plate spline smoother is being fitted is even.
#' @return The resulting value of the thin-plate spline radial basis function.
#' @details This function computes the thin-plate spline radial basis function depending on the if d is odd or even.
#' @export
#' @examples
#' ## Use the Meuse River dataset from the package 'gstat'
#'
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#' X <- scale(coordinates(meuse.all))
#' D <- as.matrix(dist(X))
#' K <- tps.rbf(D, TRUE)
tps.rbf <- function(x, is.even) {
  if (is.even == FALSE) {
    x^2
  } else {
    log(x) * x^2
  }
}

#' Precision Matrix Function
#'
#' This function creates the precision matrix for the spatial prior based on thin-plate splines
#' and returns the matrix M, and its eigenvalues and eigenvectors
#' @param X a matrix of spatial coordinates. It is recommended that the coordinates be scaled and centred.
#' @param covariates the observed values for the covariates (including intercept).
#' @return A list containing the precision matrix M and the object M.eigen containing
#' eigenvalues and eigenvectors for the matrix M.
#' @details The M matrix is the precision matrix for the
#'  spatial effects from the direct sampling spatial prior (DSSP) model. M is based on
#'  thin plate splines basis functions, see White et. al. 2019 for more details on how the
#'  matrix M is constructed.
#' @export
#' @examples
#' ## Use the Meuse River dataset from the package 'gstat'
#'
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#' X <- scale(coordinates(meuse.all))
#' make.M(X)
make.M <- function(X, covariates) {
  if (missing(covariates)) {
    covariates <- matrix(rep(1, nrow(X)), ncol = 1)
  }
  X <- as.matrix(X)
  n <- nrow(X)
  dimX <- ncol(X)
  even <- dimX %% 2 == 0
  deg <- trunc(dimX / 2 + 1) - 1
  Tmat <- cbind(covariates, stats::poly(X, degree = deg, raw = TRUE))
  d <- ncol(Tmat)
  D <- as.matrix(stats::dist(X))
  ind0 <- D != 0
  K <- D
  K[ind0] <- tps.rbf(D[ind0], even)
  TT <- tcrossprod(Tmat, Tmat)
  F.mat <- eigen(TT, symmetric = TRUE)
  F2 <- F.mat$vectors[, -c(1:d)]
  KF2 <- crossprod(K, F2)
  G <- cbind(Tmat, KF2)
  H <- matrix(0, n, n)
  H[-c(1:d), -c(1:d)] <- crossprod(KF2, F2)
  G.inv <- qr.solve(G)
  HG <- crossprod(H, G.inv)
  M <- crossprod(HG, G.inv)
  M.eigen <- eigen(M, symmetric = TRUE)
  list(M = M, M.eigen = M.eigen, G.inv = G.inv, G = G)
}
