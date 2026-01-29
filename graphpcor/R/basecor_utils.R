#' Internal functions used by basecor
#' @name basecor-utils
NULL
#> NULL

#' @describeIn basecor-utils
#' Cholesky parametrization for a correlation matrix
#' @inheritParams basecor
#' @param theta numeric parameter vector.
#' @returns matrix with lower triangle as the Cholesky factor
#' of a correlation matrix if parametrization is
#' "cpc" or "sap" and of a precision matrix
#' (of a correlation matrix) if parametrization is
#' 'itp' (with 'd0' as the diagonal elements).
#' @export
cholcor <- function(
    theta,
    p,
    itheta,
    parametrization = "cpc") {
  parametrization <- match.arg(
    arg = tolower(parametrization),
    choices = c("sap", "cpc")
  )
  stopifnot((m <- length(theta))>0)
  if(missing(p)) {
    p <- (1 + sqrt(1+8*m))/2
  }
  stopifnot(floor(p)==ceiling(p))
  stopifnot(p>1)
  ith0 <- which(lower.tri(
    diag(x = rep(1, p), nrow = p, ncol = p)))
  if(missing(itheta)) {
    itheta <- ith0
  } else {
    stopifnot(all(itheta %in% ith0))
  }
  stopifnot(length(theta)==length(itheta))
  B <- A <- diag(p)
  if(parametrization == 'cpc') {
    A[itheta] <- tanh(theta)
    B[ith0] <- sqrt(1-A[ith0]^2)
  } else {
    theta <- pi/(1+exp(-theta))
    A[itheta] <- cos(theta)
    B[ith0] <- 1.0
    B[itheta] <- sin(theta)
  }
  if(p>2) {
    for(j in 2:(p-1)) {
      B[, j] <- B[, j] * B[, j-1]
    }
  }
  L <- A * cbind(1, B[, 1:(p-1)])
  attr(L, "parametrization") <- parametrization
  attr(L, "theta") <- theta
  attr(L, "itheta") <- itheta
  attr(L, "determinant") <- exp(sum(diag(L))*2)
  return(L)
}

#' Compute the KLD between two multivariate Gaussian
#' distributions, assuming equal mean vector
#' @param C1 is a correlation matrix.
#' @param C0 is a correlation matrix of the base model.
#' @param L1 is the Cholesky of `C1`.
#' @param L0 is the Cholesky of `C0`.
#' @details
#' By assuming equal mean vector we have
#'  \deqn{KLD = 0.5( tr(C0^{-1}C1) -p - log(|C1|) + log(|C0|) )}
KLD10 <- function(C1, C0, L1, L0) {
  ### input: C1, C0 or, alternatively, L1, L0 (upper triangles)
  ### output: KLD
  if(missing(L1)) {
    if(missing(C1)) {
      stop("Please provide either 'C1' or 'L1'!")
    }
    L1 <- chol(C1)
  }
  if(missing(C1)) {
    C1 <- crossprod(L1)
  }
  p <- nrow(L1)
  hld1 <- sum(log(diag(L1)))
  if(missing(C0)) {
    if(missing(L0)) {
      warning("Missing C0,L0: using 'I'!")
      L0 <- diag(x = rep(1, p), nrow = p, ncol = p)
    }
  } else {
    if(missing(L0)) {
      L0 <- chol(C0)
    }
  }
  hld0 <- sum(log(diag(L0)))
  tr <- sum(diag(chol2inv(L0) %*% C1))
  return(0.5*(tr -p) + hld0 - hld1)
}
#' @describeIn basecor-utils
#' Evaluate the hessian of the KLD for a
#' correlation model around a base model.
#' @param C0 base correlation matrix.
#' @param decomposition character to inform
#' which decomposition is to be applied to the
#' hessian. The options are "eigen", "svd" and "chol".
#' @return list containing the hessian,
#' its 'square root', inverse 'square root' along
#' with the decomposition used
#' @importFrom stats cov2cor
#' @importFrom numDeriv hessian
Hcorrel <- function(
    theta,
    p,
    parametrization,
    itheta,
    C0,
    decomposition,
    ...) {

  theta2correl <- function(th) {
    L <- cholcor(
      theta = th,
      p = p,
      parametrization = parametrization,
      itheta = itheta
    )
    if(parametrization == 'itp') {
      return(cov2cor(chol2inv(t(L))))
    } else {
      return(tcrossprod(L))
    }
  }
  decomposition <- match.arg(
    decomposition, c("svd", "eigen", "chol"))

  if(missing(C0)) {
    C0 <- theta2correl(theta)
  }
  L0 <- chol(C0)
  H <- hessian(
    func = function(x)
      KLD10(C1 = theta2correl(x), L0 = L0),
    x = theta,
    ...)
  ## next bit follows mvtnorm:::rmvnorm()
  t0 <- sqrt(.Machine$double.eps)
  if(decomposition == "eigen") {
    Hd <- eigen(H)
    tol1 <- t0 * abs(Hd$values[1])
    if(!all(Hd$values >= tol1))
      warning("'H' is numerically not positive semidefinite")
    Hdet <- prod(Hd$values)
    s <- sqrt(pmax(Hd$values, 0.0))
    h.5 <- t(Hd$vectors %*% (t(Hd$vectors) * s))
    hneg.5 <- t(Hd$vectors %*% (t(Hd$vectors) / s))
  }
  if(decomposition == "svd") {
    Hd <- svd(H)
    tol1 <- t0 * abs(Hd$d[1])
    if(any(Hd$d<tol1))
      warning("'H' is numerically not positive semidefinite")
    Hdet <- prod(Hd$d)
    s <- sqrt(pmax(Hd$d, 0.0))
    h.5 <- t(Hd$v %*% (t(Hd$u) * s))
    hneg.5 <- t(Hd$v %*% (t(Hd$u) / s))
  }
  if(decomposition == "chol") {
    Hd <- chol(H, pivot = TRUE)
    if(any(diag(Hd)<t0))
      warning("'H' is numerically not positive semidefinite")
    tol1 <- pmin(t0 * 10, diag(Hd))
    hdet <- exp(sum(diag(Hd)*2))
    h.5 <- matrix(Hd[, order(attr(Hd, "pivot")), ], nrow(H))
    hn <- chol2inv(chol(H))
    hn.5 <- chol(hn, pivot = TRUE)
    hneg.5 <- matrix(hn.5[, order(attr(hn.5, "pivot"))], nrow(H))
  }
  stopifnot(all.equal(H, crossprod(h.5), tolerance = tol1))
##  attr(H, "theta") <- theta
  attr(Hd, "decomposition") <- decomposition
  attr(H, "decomposition") <- Hd
  attr(H, "determinant") <- Hdet
  attr(H, "h.5") <- h.5
  attr(H, "hneg.5") <- hneg.5
  return(H)
}
