## The Riemannian manifold of flat tori ( S^1 )^d. Each S^1 is identified with [0, 1], and thus the torus itself is identified with [0, 1]^d with opposite sides glued together.

#' @export
metric.FlatTorus <- function(mfd, p, U, V) {
  NextMethod('metric')
}


#' @export
norm.FlatTorus <- function(mfd, p, U) {
  NextMethod('norm')
}


#' @export
distance.FlatTorus <- function(mfd, X, Y, ...) {

  X <- as.matrix(X)
  Y <- as.matrix(Y)

  if (length(X) == 0 || length(Y) == 0) {
    return(numeric(0))
  }

  if (ncol(X) == 1) {
    X <- matrix(X, nrow(Y), ncol(Y))
  } else if (ncol(Y) == 1) {
    Y <- matrix(Y, nrow(X), ncol(X))
  }

  d2xy <- pmin(abs(X - Y), 1 - abs(X - Y))
  sqrt(colSums(d2xy^2))

}


# #' Riemannian exponential map at a point p

#' @export
rieExp.FlatTorus <- function(mfd, p, V, ...) {

  p <- as.matrix(p)
  V <- as.matrix(V)

  if (length(p) == 0) {
    return(p)
  }

  if (length(V) == 0) {
    return(V)
  }

  if (ncol(p) == 1 && ncol(V) > 1) {
    p <- matrix(p, nrow=nrow(V), ncol=ncol(V))
  } 
  if (ncol(V) == 1 && ncol(p) > 1) {
    V <- matrix(V, nrow=nrow(p), ncol=ncol(p))
  }

  res <- (V + p) %% 1
  res
}


# #' Riemannian log map at a point
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p A matrix. Each column represents a base point of the log map. If only one base point is supplied, then it is replicated to match the number of points in \emph{X}
# #' @param X A matrix. Each column represents a point on the manifold
# #' @returns A matrix with the \emph{i}th column being the log map of the \emph{i}th point


#' @export
rieLog.FlatTorus <- function(mfd, p, X, ...) {
  p <- as.matrix(p)
  X <- as.matrix(X)

  if (length(p) == 0) {
    return(p)
  } else if (length(X) == 0) {
    return(X)
  }

  if (ncol(p) == 1 && ncol(X) > 1) {
    p <- matrix(p, nrow=nrow(X), ncol=ncol(X))
  } 
  if (ncol(X) == 1 && ncol(p) > 1) {
    X <- matrix(X, nrow=nrow(p), ncol=ncol(p))
  }

  Z <- X - p
  -0.5 + (Z + 0.5) %% 1

}


#' @export
project.FlatTorus <- function(mfd, p) {
  as.matrix(p) %% 1
}


#' @export
calcGeomPar.FlatTorus <- function(mfd, dimAmbient, dimIntrinsic, dimTangent) {

  if (!missing(dimAmbient)) {
    dimAmbient
  } else if (!missing(dimIntrinsic)) {
    dimIntrinsic
  } else if (!missing(dimTangent)) {
    dimTangent
  }
}


#' @export
calcIntDim.FlatTorus <- function(mfd, geomPar, dimAmbient, dimTangent) {
  
  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent)
  }
}


#' @export
calcTanDim.FlatTorus <- function(mfd, geomPar, dimAmbient, dimIntrinsic) {

  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic)
  }
}


#' @export
calcAmbDim.FlatTorus <- function(mfd, geomPar, dimIntrinsic, dimTangent) {
  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent)
  }
}


#' @export
projectTangent.FlatTorus <- function(mfd, p, X, projMatOnly=FALSE, ...) {

  if (projMatOnly) {
    return(diag(length(p)))
  } else {
    -0.5 + (as.matrix(X) + 0.5) %% 1
  }
}


#' @export
origin.FlatTorus <- function(mfd, dimIntrinsic, ...) {
  as.matrix(rep(1/2, dimIntrinsic))
}


#' @export
#' @describeIn basisTan An identity matrix
basisTan.FlatTorus <- function(mfd, p) {
  diag(nrow=length(p))
}


