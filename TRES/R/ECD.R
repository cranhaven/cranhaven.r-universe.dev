##################################################
#         ECD objective function                 #
##################################################
objF_ECD <- function(A, B, w) {
  fk <- log(t(w) %*% A %*% w) + log(t(w) %*% B%*% w) - 2*log(crossprod(w))
  return(fk)
}

##################################################
#    get initial value for ECD algorithm         #
##################################################
ECDini <- function(M, U) {
  p <- dim(U)[2]
  eigM <- eigen(M)
  eigMU <- eigen(M + U)
  v1 <- eigM$vectors
  v2 <- eigMU$vectors
  v <- cbind(v1, v2)
  W0 <- Re(v[, 1])
  Fw0 <- log(t(W0) %*% chol2inv(chol(M+U)) %*% W0) + log(t(W0) %*% M %*% W0)
  for (i in 2:(2*p)) {
    W <- Re(v[, i])
    Fw <- log(t(W) %*% chol2inv(chol(M+U)) %*% W) + log(t(W) %*% M %*% W)
    if (Fw < Fw0){
      W0 <- W
      Fw0 <- Fw
    }
  }
  return(W0)
}

##################################################
#     optimECD algorithm for solving fk          #
##################################################
optimECD <- function(A, B, w0, maxiter=500, tol=1e-08) {
  p <- length(w0)
  eigA <- eigen (A + t(A));
  ## already in descending order###
  Gp <- eigA$vectors
  dn <- eigA$values
  dn <- diag(dn/2)

  v0 <- crossprod(Gp, w0)
  GBG <- t(Gp) %*% B %*% Gp
  fk0 <- objF_ECD(dn, GBG, v0)
  v1 <- v0
  for (iter in 1:maxiter) {
    flg <- 0
    alpha <- 1/(t(v1) %*% dn %*% v1)
    beta <- 1/(t(v1) %*% GBG %*% v1)
    delta <- 1/crossprod(v1)
    A1 <- as.numeric(alpha)*dn
    B1 <- as.numeric(beta)*GBG
    for (j in 1:p) {
      AB1 <- A1[j, j] + B1[j, j]
      if ((2*delta - AB1) != 0) {
        v1[j] <- (crossprod(v1, A1[,j]) + crossprod(v1, B1[, j]) - AB1*v1[j])/(2*delta - AB1)
        flg <- flg + 1
      }
      if (objF_ECD(dn, GBG, v1) > (objF_ECD(dn, GBG, v0)) + tol) {
        v1[j] <- v0[j]
        flg <- flg - 1
      }
    }
    fk1 <- objF_ECD(dn, GBG, v1)
    if ((abs (fk0 - fk1)) < tol) break;
    fk0 <- fk1
    v0 <- v1
  }
  w <- Gp %*% v1

  w <- w/sqrt(sum(w^2))
  return(w)
}

##################################################
#           ECD algorithm                        #
##################################################
#' ECD algorithm for estimating the envelope subspace
#'
#' Estimate the envelope subspace with specified dimension based on ECD algorithm proposed by Cook, R. D., & Zhang, X. (2018).
#'
#' @param M The \eqn{p}-by-\eqn{p} positive definite matrix \eqn{M} in the envelope objective function.
#' @param U The \eqn{p}-by-\eqn{p} positive semi-definite matrix \eqn{U} in the envelope objective function.
#' @param u An integer between 0 and \eqn{n} representing the envelope dimension.
#' @param ... Additional user-defined arguments:
#' \itemize{
#'   \item{\code{maxiter}: The maximal number of iterations.}
#'   \item{\code{tol}: The tolerance used to assess convergence. See the ECD algorithm in Cook, R. D., & Zhang, X. (2018).}
#' }
#' The default values are: \code{maxiter=500; tol=1e-08}.
#'
#' @details Estimate \code{M}-envelope of \code{span(U)}. The dimension of the envelope is \code{u}.
#'
#' See \code{\link{FGfun}} for the generic objective function.
#'
#' The ECD algorithm is similar to 1D algorithm proposed by Cook, R. D., & Zhang, X. (2016). A fast and stable algorithm is used for solving each individual objective function.
#'
#' @return Return the orthogonal basis of the envelope subspace with each column represent the sequential direction. For example, the first column is the most informative direction.
#'
#' @references Cook, R.D. and Zhang, X., 2018. Fast envelope algorithms. Statistica Sinica, 28(3), pp.1179-1197.
#'
#' @examples
#' ##simulate two matrices M and U with an envelope structure#
#' data <- MenvU_sim(p = 20, u = 5, wishart = TRUE, n = 200)
#' M <- data$M
#' U <- data$U
#' G <- data$Gamma
#' Gamma_ECD <- ECD(M, U, u=5)
#' subspace(Gamma_ECD, G)
#'
#'@export
ECD <- function(M, U, u, ...){
  # estimating M-envelope contains span(U)
  # where M>0 and is symmetric
  # dimension of the envelope is u
  # based on inv(M+U) and (M)
  opts <- list(...)
  if(is.null(opts$maxiter) || opts$maxiter < 0 || opts$maxiter > 2^20) opts$maxiter <- 500
  if(is.null(opts$tol) || opts$tol < 0 || opts$tol > 1) opts$tol <- 1e-08
  maxiter <- opts$maxiter
  tol <- opts$tol

  p <- dim(M)[2]
  Mnew <- M
  Unew <- U
  G <- matrix(0, p, u)
  G0 <- diag(p)
  for (k in 1:u) {
    init <- ECDini(Mnew, Unew)
    gk <- optimECD(Mnew, chol2inv(chol(Mnew + Unew)), w0 = init, maxiter, tol)
    # gk <- ECD1st(Mnew, Unew, maxiter, tol)
    G[, k]<- G0 %*% gk
    G0 <- qr.Q(qr(G[, 1:k]),complete=TRUE)[,(k+1):p]
    Mnew <- t(G0) %*% M %*% G0
    Unew <- t(G0) %*% U %*% G0
  }
  Gamma <- G
  return(Gamma)
}
