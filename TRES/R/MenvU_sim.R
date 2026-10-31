#' Generate matrices \eqn{M} and \eqn{U}
#'
#' This function generates the matrices \eqn{M} and \eqn{U} with envelope structure.
#'
#' @param p Dimension of \eqn{p}-by-\eqn{p} matrix \eqn{M}.
#' @param u The envelope dimension. An integer between 0 and \eqn{p}.
#' @param Omega The positive definite matrix \eqn{\Omega} in \eqn{M=\Gamma\Omega\Gamma^T+\Gamma_0\Omega_0\Gamma_0^T}. The default is \eqn{\Omega=AA^T} where the elements in \eqn{A} are generated from Uniform(0,1) distribution.
#' @param Omega0 The positive definite matrix \eqn{\Omega_0} in \eqn{M=\Gamma\Omega\Gamma^T+\Gamma_0\Omega_0\Gamma_0^T}. The default is \eqn{\Omega_0=AA^T} where the elements in \eqn{A} are generated from Uniform(0,1) distribution.
#' @param Phi The positive definite matrix \eqn{\Phi} in \eqn{U=\Gamma\Phi\Gamma^T}. The default is \eqn{\Phi=AA^T} where the elements in \eqn{A} are generated from Uniform(0,1) distribution.
#' @param jitter Logical or numeric. If it is numeric, the diagonal matrix \code{diag(jitter, nrow(M), ncol(M))} is added to matrix \eqn{M} to ensure the positive definiteness of \eqn{M}. If it is \code{TRUE}, then it is set as \code{1e-5} and the jitter is added. If it is \code{FALSE} (default), no jitter is added.
#' @param wishart Logical. If it is \code{TRUE}, the sample estimator from Wishart distribution \eqn{W_p(M/n, n)} and \eqn{W_p(U/n, n)} are generated as the output matrices \code{M} and \code{U}.
#' @param n The sample size. If \code{wishart} is \code{FALSE}, then \code{n} is ignored.
#'
#' @details The matrices \eqn{M} and \eqn{U} are in forms of
#' \deqn{M = \Gamma \Omega \Gamma^T + \Gamma_0\Omega_0\Gamma_0^T, U = \Gamma \Phi \Gamma^T.}
#'
#' The envelope basis \eqn{\Gamma} is randomly generated from the Uniform (0, 1) distribution elementwise and then transformed to a semi-orthogonal matrix. \eqn{\Gamma_0} is the orthogonal completion of \eqn{\Gamma}.
#'
#' In some cases, to guarantee that \eqn{M} is positive definite which is required by the definition of envelope, a \code{jitter} should be added to \eqn{M}.
#'
#' If \code{wishart} is \code{TRUE}, after the matrices \eqn{M} and \eqn{U} are generated, the samples from Wishart distribution \eqn{W_p(M/n, n)} and \eqn{W_p(U/n, n)} are output as matrices \eqn{M} and \eqn{U}. If so, \code{n} is required.
#'
#' @return
#' \item{M}{The \eqn{p}-by-\eqn{p} matrix \code{M}.}
#' \item{U}{The \eqn{p}-by-\eqn{p} matrix \code{U}.}
#' \item{Gamma}{The \eqn{p}-by-\eqn{u} envelope basis.}
#'
#' @references Cook, R.D. and Zhang, X., 2018. Fast envelope algorithms. Statistica Sinica, 28(3), pp.1179-1197.
#'
#' @examples
#' data1 <- MenvU_sim(p = 20, u = 5)
#' M1 <- data1$M
#' U1 <- data1$U
#'
#' # Sample version from Wishart distribution
#' data2 <- MenvU_sim(p = 20, u = 5, wishart = TRUE, n = 200)
#' M2 <- data2$M
#' U2 <- data2$U
#'
#' @export
#' @importFrom stats runif
#'
MenvU_sim <- function(p, u, Omega=NULL, Omega0=NULL, Phi=NULL, jitter = FALSE, wishart = FALSE, n = NULL){
  ##randomly generate a semi-orthogonal p-by-u basis matrix (Gamma) for the
  ##envelope and its orthogonal completion (Gamma0) of dimension p-by-(p-u)
  Gamma <- matrix(runif(p*u), p, u)

  ###make Gamma semi-orthogonal
  Gamma <- qr.Q(qr(Gamma))
  Gamma0 <- qr.Q(qr(Gamma),complete=TRUE)[,(u+1):p]

  ## randomly generated symmetric positive definite matrices, M and U, to have
  ## an exact u-dimensional envelope structure

  if (is.null(Omega)) {
    Omega <- matrix(runif(u^2), u, u)
    Omega <- tcrossprod(Omega)
  }
  if (is.null(Omega0)) {
    Omega0 <- matrix(runif((p-u)^2),p-u,p-u)
    Omega0 <- tcrossprod(Omega0)
  }
  if (is.null(Phi)) {
    Phi <- matrix(runif(u^2), u, u)
    Phi <- tcrossprod(Phi)
  }
  M <- Gamma %*% Omega %*% t(Gamma) + Gamma0 %*% Omega0 %*% t(Gamma0)
  U <- Gamma %*% Phi %*% t(Gamma)

  if(!is.logical(jitter) && !is.numeric(jitter)) stop("jitter must be logical or numerical.")
  if(isTRUE(jitter)){jitter <- 1e-5}
  # If is not FALSE, add jitter.
  if(is.numeric(jitter)){
    M <- M + jitter * diag(1, nrow(M), ncol(M))
  }

  if(isTRUE(wishart)){
    if(is.null(n)) stop("n is missing.")
    X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = M)
    Y <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = U)
    M <- (crossprod(X))/n
    U <- (crossprod(Y))/n
  }

  # Ensure the symmetry.
  if(!isSymmetric(M)) M <- (M + t(M))/2
  if(!isSymmetric(U)) U <- (U + t(U))/2

  return(list(M=M, U=U, Gamma=Gamma))
}
