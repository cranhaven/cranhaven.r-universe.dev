
Normalize <- function(v, tol=1e-10) {
  v <- as.numeric(v)
  d <- length(v)
  vNorm <- as.numeric(sqrt(crossprod(v)))
  res <- if (!is.na(vNorm) && vNorm <= tol) {
    rep(0, d)
  } else {
    v / vNorm
  }

  res
}


# #' Riemannian exponential map at a point
ExpSp <- function(p, V) {

  tol <- 1e-10

  # p needs to be on a unit sphere
  stopifnot(abs(sum(p^2) - 1) <= tol)

  if (!is.matrix(V)) {
    V <- matrix(V)
    stopifnot(length(V) == length(p))
  } else {
    stopifnot(ncol(V) == length(p))
  }

  # each col of V needs to be orthogonal to p
  stopifnot(all(abs(apply(na.omit(V), 1, crossprod, y=p) - 0) <= tol))

  res <- t(apply(V, 1, Exp1, mu=p, tol=tol))
  res
}


Exp1 <- function(v, mu, tol=1e-10) {
  vNorm <- as.numeric(sqrt(crossprod(v)))
  if (!is.na(vNorm) && vNorm <= tol) {
    mu
  } else {
    cos(vNorm) * mu + sin(vNorm) * v / vNorm
  }
}


