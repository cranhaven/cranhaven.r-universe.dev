#' Compute the (lower triangle) Cholesky of the initial precision `Q0`.
#' @inheritParams basepcor
#' @param theta numeric parameter vector.
#' @returns lower triangular matrix
#' @details The (lower triangle) Cholesky factor
#' of the initial precision for a correlation matrix contains
#' the parameters in the non-zero elements of the lower triangle side
#' of the precision matrix.
#' The filled-in elements are computed from them using [fillLprec()].
Lprec0 <- function(
    theta,
    p,
    itheta,
    d0) {
  stopifnot(p>1)
  stopifnot((m <- length(theta))>0)
  ith0 <- which(lower.tri(
    diag(x = rep(1, p), nrow = p, ncol = p)))
  if(missing(itheta)) {
    itheta <- ith0
  } else {
    stopifnot(all(itheta %in% ith0))
  }
  stopifnot(length(itheta)==m)
  if(missing(d0)) {
    warning("Using 'd0 = p:1'!")
    d0 <- p:1
  }
  L <- diag(x = d0, nrow = p, ncol = p)
  L[itheta] <- theta
  L <- fillLprec(L)
  return(L)
}

#' Function to fill-in a Cholesky matrix
#' @param L matrix as the lower triangle
#' containing the Cholesky decomposition of
#' a initial precision matrix whose non-zeros are
#' only at the position where the lower triangle
#' side of the precision matrix is also non-zero
#' @param lfi integer vector used as indicator of the
#' position in the lower matrix where are the
#' fill-in elements. Must be col then row ordered.
#' @return lower triangular matrix with the filled-in
#' elements thus `Q0` can be computed.
fillLprec <- function(L, lfi) {
  L <- as.matrix(L)
  p <- nrow(L)
  if(missing(lfi)) {
    i0 <- is.zero(L)
    G <- i0 - 1.0
    G <- G + t(G)
    diag(G) <- 1 - colSums(G)
    lG <- t(chol(G))
    lfi <- which(i0 & (!is.zero(lG)))
  }
  if(length(lfi)>0) {
    if(length(lfi)>1)
      stopifnot(all(diff(lfi)>0))
    ii <- row(L)[lfi]
    jj <- col(L)[lfi]
    for(v in 1:length(ii)) {
      i <- ii[v]
      j <- jj[v]
      if(j==1) {
        warning("j = 1!\n")
        L[i,1] <- 0.0
      } else {
        stopifnot((i>1) & (j>1)) ## L_{11} not allowed
        stopifnot(j>1) ## j=1 is not allowed
        k <- 1:(j-1)
        L[i, j] <- -sum(L[i, k] * L[j, k]) / L[j, j]
      }
    }
  }
  return(L)
}
