## L2: The same as Euclidean manifold, except for the normalizing factor (assume at each time t the densities/functions are observed over S=[0, 1] and is regular). Dens: Extrinsic density manifold; the same as L2 except that the projection is onto densities. HS: Hilbert sphere for square-root densities.
# TODO: Implement basisTan.*

# The intrinsic and ambient dimension, and geometric parameter are all the same and equal to the number of grids.

# #' Riemannian metric of tangent vectors
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p The base point which could be NULL if the metric does not rely on the base points
# #' @param U A D*n matrix, each column represents a tangent vector at \emph{p}
# #' @param V A D*n matrix, each column represents a tangent vector at \emph{p}
# #' 
# #' @details The tangent vectors can be represented in a coordinate frame, or in ambient space
# #' @export

#' @export
metric.L2 <- function(mfd, p, U, V) {
  U <- as.matrix(U)
  V <- as.matrix(V)
  ns <- nrow(U)
  metric.default(mfd, p, U, V) / (ns - 1)

  # U <- as.matrix(U)
  # V <- as.matrix(V)
  # ns <- nrow(U)
  # # return(colSums(U * V) / (ns - 1))
  # as.numeric(crossprod(U, V) / (ns - 1))
}


# #' The norm induced by the Riemannian metric tensor on tangent spaces
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p The base point which could be NULL if the norm does not rely on it
# #' @param U A D*n matrix, where each column represents a tangent vector at \emph{p}

#' @export
#' @describeIn norm Method
norm.L2 <- function(mfd, p, U) {
  U <- as.matrix(U)
  ns <- nrow(U)
  norm.default(mfd, p, U) / sqrt(ns - 1)
}

# #' Geodesic distance of points on the manifold
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param X A D*n matrix. Each column represents a point on manifold
# #' @param Y A D*n matrix. Each column represents a point on manifold
# #' @returns A 1*n vector. The \emph{i}th element is the geodesic distance of \code{X[, i]} and \code{Y[, i]}

#' @export
#' @describeIn distance Method
distance.L2 <- function(mfd, X, Y, ...)
{
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  ns <- nrow(X)

  if (ncol(X) == 1) {
    X <- matrix(X, nrow(X), ncol(Y))
  } else if (ncol(Y) == 1) {
    Y <- matrix(Y, nrow(Y), ncol(X))
  }

  sqrt(colSums((X - Y)^2 / (ns - 1)))
}

# #' Riemannian exponential map at a point p

#' @export
#' @describeIn rieExp Method
rieExp.L2 <- function(mfd, p, V, ...) {

  V <- as.matrix(V)

  res <- V + matrix(p, nrow=nrow(V), ncol=ncol(V))
  res
}


# #' Riemannian log map at a point
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p A matrix. Each column represents a base point of the log map. If only one base point is supplied, then it is replicated to match the number of points in \emph{X}
# #' @param X A matrix. Each column represents a point on the manifold
# #' @returns A matrix with the \emph{i}th column being the log map of the \emph{i}th point


#' @export
#' @describeIn rieLog Method
rieLog.L2 <- function(mfd, p, X, ...) {

  X <- as.matrix(X)
  Z <- X - matrix(p, nrow(X), ncol(X))

  return(Z)
}


#' @export
#' @describeIn project Method
project.L2 <- function(mfd, p) {
  as.matrix(p)
}


#' @export
calcGeomPar.L2 <- function(mfd, dimAmbient, dimIntrinsic, dimTangent) {

  if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent)
  }
}


#' @export
calcIntDim.L2 <- function(mfd, geomPar, dimAmbient, dimTangent) {
  
  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent)
  }
}


#' @export
calcTanDim.L2 <- function(mfd, geomPar, dimAmbient, dimIntrinsic) {

  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic)
  }
}


#' @export
calcAmbDim.L2 <- function(mfd, geomPar, dimIntrinsic, dimTangent) {
  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent)
  }
}


# Project ambient space data onto the tangent space at p

#' @export
#' @describeIn projectTangent Method
projectTangent.L2 <- function(mfd, p, X, projMatOnly=FALSE, ...) {

  if (projMatOnly) {
    return(diag(nrow=length(p)))
  } else {
    return(as.matrix(X))
  }
}


#' @export
#' @describeIn origin Method
origin.L2 <- function(mfd, dimIntrinsic, ...) {
  nGrid <- calcGeomPar.Dens(mfd, dimIntrinsic=dimIntrinsic)
  matrix(rep(0, nGrid))
}


#' @export
#' @describeIn metric Method
metric.HS <- function(mfd, p, U, V) {

  metric.L2(mfd, p, U, V)
  # U <- as.matrix(U)
  # V <- as.matrix(V)
  # ns <- nrow(U)
  # # return(colSums(U * V) / (ns - 1))
  # as.numeric(crossprod(U, V) / (ns - 1))

}

# #' The norm induced by the Riemannian metric tensor on tangent spaces
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p The base point which could be NULL if the norm does not rely on it
# #' @param U A D*n matrix, where each column represents a tangent vector at \emph{p}

#' @export
#' @describeIn norm Method
norm.HS <- function(mfd, p, U) {
  norm.L2(mfd, p, U)
  # ns <- nrow(U)
  # return(sqrt(colSums(U*U) / (ns - 1)))
}


# #' Geodesic distance of points on the manifold
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param X A D*n matrix. Each column represents a point on manifold
# #' @param Y A D*n matrix. Each column represents a point on manifold
# #' @returns A 1*n vector. The \emph{i}th element is the geodesic distance of \code{X[, i]} and \code{Y[, i]}

#' @export
#' @describeIn distance Method
distance.HS <- function(mfd, X, Y, ...) {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  ns <- nrow(X)

  val <- colSums(X * Y / (ns - 1))
  val[val > 1] <- 1
  val[val < -1] <- -1
  acos(val)
}


# #' Geodesic curve stating at a point
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p The starting point of the geodesic curve
# #' @param h A matrix with each column representing a tangent vector. If there is only one tangent vector is supplied, then it is replicated to match the length of \emph{t}
# #' @param t A vector, the time points where the curve is evaluated.
# #' 
# #' @details The curve is \eqn{\gamma(t)=\mathrm{Exp}_p(th)}
# #' 
# #' @returns A matrix with each column representing a point on the manifold

#' @export
#' @describeIn geodesicCurve Method
geodesicCurve.HS <- function(mfd, p, h, t)
{
    if(!is.matrix(p)) p <- as.matrix(p)
    if(!is.matrix(h)) h <- as.matrix(h)
    ns <- nrow(p)
    stopifnot(all(abs(crossprod(p, h) / (ns - 1) - 0) < 1e-8))
    n <- dim(h)[2]
    d <- dim(h)[1]
    m <- length(t)

    if (n==1) {
      h <- h %*% matrix(t, nrow=1)
    } else if (m != n) {
      stop('If there is more than one tangent vectors, the number of tangent vectors must be equal to the number of time points')
    }

    rieExp.HS(mfd, p, h)
    
}

# #' Riemannian exponential map at a point

#' @export
#' @describeIn rieExp Method
rieExp.HS <- function(mfd, p, V, ...) {

  tol <- 1e-10
  p <- as.matrix(p)
  ns <- nrow(p)

  # p needs to be on a unit sphere
  stopifnot(abs(sum(p^2) / (ns - 1) - 1) <= tol)

  if (!is.matrix(V)) {
    V <- matrix(V)
    stopifnot(length(V) == length(p))
  } else {
    stopifnot(nrow(V) == length(p))
  }

  # each col of V needs to be orthogonal to p
  stopifnot(all(abs(apply(V, 2, crossprod, y=p) / (ns - 1) - 0) <= tol))

  res <- apply(V, 2, Exp1HS, mu=p, tol=tol)
  matrix(res, ncol=ncol(V))
}


# #' Riemannian log map at a point
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p A matrix. Each column represents a base point of the log map. If only one base point is supplied, then it is replicated to match the number of points in \emph{X}
# #' @param X A matrix. Each column represents a point on the manifold
# #' @returns A matrix with the \emph{i}th column being the log map of the \emph{i}th point

#' @export
#' @describeIn rieLog Method
rieLog.HS <- function(mfd, p, X, tol=1e-7, ...) {

    if (!is.matrix(X)) X <- as.matrix(X)
    p <- as.matrix(p)
    m <- dim(p)[2]
    n <- dim(X)[2]
    d <- dim(p)[1]
    if (m == 1 && n != 1) {
      p <- matrix(p, d, n)
    }

    # # p needs to be on a unit sphere
    # stopifnot(all(abs(colSums(p^2) - 1) <= tol))

    # # each column of X needs to be on a unit sphere
    # stopifnot(all(abs(apply(X, 2, function(x) sum(x^2)) - 1) <= tol))

    Z <- Log2HS(X, p, tol)

    return(Z)
}


Log2HS <- function(X, Mu, tol=1e-7) {
  ns <- nrow(X)
  N <- ncol(X)
  cprod <- colSums(X * Mu) / (ns - 1)
  U <- X - matrix(cprod, ns, N, byrow=TRUE) * Mu
  uNorm <- sqrt(colSums(U^2) / (ns - 1))
  res <- matrix(0, ns, N)
  ind <- uNorm > tol
  distS <- acos(ifelse(cprod > 1, 1, ifelse(cprod < -1, -1, cprod)))
  res[, ind] <- (U * matrix(distS / uNorm, ns, N, byrow=TRUE))[, ind, drop=FALSE]
  if (any(!ind)) {
    for (i in which(!ind)) {
      res[, i] <- projectTangent.HS(111, Mu[, i], X[, i] - Mu[, i])
    }
  }
  res
}


#' @export
#' @describeIn project Method
project.HS <- function(mfd, p) {
  p <- as.matrix(p)
  ns <- nrow(p)
  n <- ncol(p)
  p <- apply(p, 2, Normalize, tol=1e-13) * sqrt(ns - 1)
  matrix(p, ncol=n)
}


#' @export
#' @describeIn projectTangent Method
# Project ambient space data onto the tangent space at p
projectTangent.HS <- function(mfd, p, X, projMatOnly=FALSE, ...) {

  dd <- length(p)
  stopifnot(abs(sum(p^2) / (dd - 1) - 1) < 1e-14)
  projMat <- diag(nrow=dd) - c(tcrossprod(p) / (dd - 1))

  if (projMatOnly) {
    return(projMat)
  } else {
    return(crossprod(projMat, X))
  }
}


Exp1HS <- function(v, mu, tol=1e-10) {
  ns <- length(v)
  vNorm <- as.numeric(sqrt(crossprod(v) / (ns - 1)))
  if (!is.na(vNorm) && vNorm <= tol) {
    Normalize(mu + v) * sqrt(ns - 1)
  } else {
    cos(vNorm) * mu + sin(vNorm) * v / vNorm
  }
}


#' @export
calcGeomPar.HS <- function(mfd, dimAmbient, dimIntrinsic, dimTangent) {

  if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient - 1)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent - 1)
  }
}


#' @export
calcIntDim.HS <- function(mfd, geomPar, dimAmbient, dimTangent) {

  if (!missing(geomPar)) {
    as.integer(geomPar - 1)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient - 1)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent - 1)
  }

}


#' @export
calcTanDim.HS <- function(mfd, geomPar, dimAmbient, dimIntrinsic) {

  if (!missing(geomPar)) {
    as.integer(geomPar + 1)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic + 1)
  }

}


#' @export
calcAmbDim.HS <- function(mfd, geomPar, dimIntrinsic, dimTangent) {
  if (!missing(geomPar)) {
    as.integer(geomPar + 1)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic + 1)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent)
  }
}


# The root uniform density

#' @export
#' @describeIn origin Method
origin.HS <- function(mfd, dimIntrinsic, ...) {
  nGrid <- calcGeomPar.HS(mfd, dimIntrinsic=dimIntrinsic)
  as.matrix(rep(sqrt((nGrid - 1) / nGrid), nGrid))
}


# TODO: don't use NextMethod()

#' @export
#' @describeIn metric Method
metric.Dens <- function(mfd, p, U, V) NextMethod()


#' @export
#' @describeIn norm Method
norm.Dens <- function(mfd, p, U) NextMethod()


#' @export
#' @describeIn distance Method
distance.Dens <- function(mfd, X, Y, ...) NextMethod()


#' @export
#' @describeIn rieExp Method
rieExp.Dens <- function(mfd, p, V, ...) {

  V <- as.matrix(V)

  res <- V + matrix(p, nrow=nrow(V), ncol=ncol(V))
  project.Dens(p=res)
}


#' @export
#' @describeIn rieLog Method
rieLog.Dens <- function(mfd, p, X, ...) NextMethod()


#' @export
#' @describeIn project Method
project.Dens <- function(mfd, p) {
  A <- as.matrix(p)
  ns <- nrow(A)
  res <- apply(A, 2, function(x) {
    x[x < 0] <- 0
    x / sum(x) * (ns - 1)
  })
  matrix(res, ncol=ncol(A))
}


#' @export
#' @describeIn projectTangent Method
projectTangent.Dens <- function(mfd, p, X, projMatOnly, ...) NextMethod()


#' @export
calcGeomPar.Dens <- function(mfd, dimAmbient, dimIntrinsic, dimTangent) NextMethod()


#' @export
calcIntDim.Dens <- function(mfd, geomPar, dimAmbient, dimTangent) NextMethod()


#' @export
calcTanDim.Dens <- function(mfd, geomPar, dimAmbient, dimIntrinsic) NextMethod()


#' @export
#' @describeIn origin The uniform density
origin.Dens <- function(mfd, dimIntrinsic, ...) {
  nGrid <- calcGeomPar.Dens(mfd, dimIntrinsic=dimIntrinsic)
  as.matrix(rep((nGrid - 1) / nGrid, nGrid))
}


