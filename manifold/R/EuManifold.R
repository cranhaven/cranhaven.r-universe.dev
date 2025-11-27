# #' Riemannian metric of tangent vectors
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p The base point which could be NULL if the metric does not rely on the base points
# #' @param U A D*n matrix, each column represents a tangent vector at \emph{p}
# #' @param V A D*n matrix, each column represents a tangent vector at \emph{p}
# #' 
# #' @details The tangent vectors can be represented in a coordinate frame, or in ambient space


#' @export
#' @describeIn metric Method
metric.Euclidean <- function(mfd, p, U, V) {
  NextMethod('metric')
}


#' @export
#' @describeIn norm Method
norm.Euclidean <- function(mfd, p, U) {
  NextMethod('norm')
}

# #' Geodesic distance of points on the manifold
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param X A D*n matrix. Each column represents a point on manifold
# #' @param Y A D*n matrix. Each column represents a point on manifold
# #' @returns A 1*n vector. The \emph{i}th element is the geodesic distance of \code{X[, i]} and \code{Y[, i]}

#' @export
#' @describeIn distance Method
distance.Euclidean <- function(mfd, X, Y, ...) {

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

  sqrt(colSums((X - Y)^2))
}

# #' Riemannian exponential map at a point p

#' @export
#' @describeIn rieExp Method
rieExp.Euclidean <- function(mfd, p, V, ...) {

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

  res <- V + p
  res
}


# #' Riemannian log map at a point
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p A matrix. Each column represents a base point of the log map. If only one base point is supplied, then it is replicated to match the number of points in \emph{X}
# #' @param X A matrix. Each column represents a point on the manifold
# #' @returns A matrix with the \emph{i}th column being the log map of the \emph{i}th point


#' @export
#' @describeIn rieLog Method
rieLog.Euclidean <- function(mfd, p, X, ...) {
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

  return(Z)
}


#' @export
#' @describeIn project Method
project.Euclidean <- function(mfd, p) {
  as.matrix(p)
}


#' @export
calcGeomPar.Euclidean <- function(mfd, dimAmbient, dimIntrinsic, dimTangent) {

  if (!missing(dimAmbient)) {
    dimAmbient
  } else if (!missing(dimIntrinsic)) {
    dimIntrinsic
  } else if (!missing(dimTangent)) {
    dimTangent
  }
}


#' @export
calcIntDim.Euclidean <- function(mfd, geomPar, dimAmbient, dimTangent) {
  
  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent)
  }
}


#' @export
calcTanDim.Euclidean <- function(mfd, geomPar, dimAmbient, dimIntrinsic) {

  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic)
  }
}


#' @export
calcAmbDim.Euclidean <- function(mfd, geomPar, dimIntrinsic, dimTangent) {
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
projectTangent.Euclidean <- function(mfd, p, X, projMatOnly=FALSE, ...) {

  if (projMatOnly) {
    return(diag(length(p)))
  } else {
    return(as.matrix(X))
  }
}


#' @export
#' @describeIn origin Method
origin.Euclidean <- function(mfd, dimIntrinsic, ...) {
  as.matrix(rep(0, dimIntrinsic))
}


#' @export
#' @describeIn basisTan An identity matrix
basisTan.Euclidean <- function(mfd, p) {
  diag(nrow=length(p))
}


