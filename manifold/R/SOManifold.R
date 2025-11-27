## Class for general SO(n) manifolds. In this implementation, we follow the convention that
## we use only the lower triangle of the skew-symmetric matrices on the tangent spaces to 
## denote the tangent vectors. 



#' @export
#' @describeIn metric Method
metric.SO <- function(mfd, p, U, V) {
  NextMethod('metric')
}


#' @export
#' @describeIn norm Method
norm.SO <- function(mfd, p, U) {
  norm.default(mfd, p, U)
}


# #' Geodesic distance of points on the manifold
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param X A D*n matrix. Each column represents a point on manifold
# #' @param Y A D*n matrix. Each column represents a point on manifold
# #' @returns A 1*n vector. The \emph{i}th element is the geodesic distance of \code{X[, i]} and \code{Y[, i]}

#' @export
#' @describeIn distance Method
distance.SO <- function(mfd, X, Y, ...) {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  distSO(X, Y)
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

# geodesicCurve.SO <- function(mfd, p, h, t) {
    # if(!is.matrix(p)) p <- as.matrix(p)
    # if(!is.matrix(h)) h <- as.matrix(h)
    # stopifnot(all(abs(crossprod(p, h) - 0) < 1e-8))
    # n <- dim(h)[2]
    # d <- dim(h)[1]
    # m <- length(t)

    # if (n==1) {
      # h <- h %*% matrix(t, nrow=1)
    # } else if (m != n) {
      # stop('If there is more than one tangent vectors, the number of tangent vectors must be equal to the number of time points')
    # }

    # rieExp.SO(mfd, p, h)
    
# }


# #' Riemannian exponential map at a point

#' @param tol Tolerance for `rieExp.SO`
#' @export
#' @describeIn rieExp Method
rieExp.SO <- function(mfd, p, V, tol=1e-10, ...) {

  V <- as.matrix(V)

  n <- ncol(V)
  d <- calcGeomPar.SO(dimTangent=nrow(V))

  if (length(V) == 0 || (!missing(p) && length(p) == 0) ) {
    return(matrix(0, nrow(p), 0))
  }

  if (missing(p)) {
    p <- matrix(diag(d), d^2, ncol(V))
  } else {
    if (is.matrix(p)) {
      stopifnot(nrow(p) == d^2)
    } else {
      p <- matrix(p)
    }
    if (ncol(p) == 1) {
      p <- matrix(p, nrow(p), n)
    }
  }

  # each col of V needs to be orthogonal to p

  res <- sapply(seq_len(n), function(i) {
                  vMat <- MakeSkewSym(V[, i])
                  muMat <- matrix(p[, i], d, d)
                  stopifnot(isSkewSym(vMat))

                  if (d == 3) {
                    ExpMSO3(vMat, tol) %*% muMat
                  } else {
                    # seems only 'Eigen' works but not 'Higham08'
                    ExpM(vMat) %*% muMat
                  }
               })

  matrix(res, ncol=n)
}


# #' Riemannian log map at a point
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p A matrix. Each column represents a base point of the log map. If only one base point is supplied, then it is replicated to match the number of points in \emph{X}. If missing, the basepoint is set to be the identity matrix.
# #' @param X A matrix. Each column represents a point on the manifold
# #' @returns A matrix with the \emph{i}th column being the log map of the \emph{i}th point


#' @param tol Tolerance for `rieLog.SO`
#' @export
#' @describeIn rieLog Method
rieLog.SO <- function(mfd, p, X, tol=1e-10, ...) {

  X <- as.matrix(X)
  if (missing(p)) {
    p <- matrix(diag(nrow=sqrt(nrow(X))))
  } else {
    p <- as.matrix(p)
  }
  logSO(p, X)
  # X <- as.matrix(X)
  # d <- round(sqrt(nrow(X)))

  # if (missing(p)) {
    # p <- c(diag(nrow=d))
  # }
  # p <- as.matrix(p)
  # if (length(p) == 0 || length(X) == 0) {
    # return(matrix(0, calcTanDim.SO(dimAmbient=nrow(p)), 0))
  # }

  # if (ncol(p) == 1) {
    # p <- matrix(p, nrow(p), ncol(X))
  # } else if (ncol(X) == 1) {
    # X <- matrix(X, nrow(X), ncol(p))
  # }
  # n <- ncol(X)

  # res <- sapply(seq_len(n), function(i) {
                  # xMat <- matrix(X[, i], d, d)
                  # muMat <- matrix(p[, i], d, d)
                  # stopifnot(isSO(xMat, tol))
                  # if (d == 3) {
                    # LogMSO3(tcrossprod(xMat, muMat))
                  # } else {
                    # # seems only 'Eigen' works but not 'Higham08'
                    # tryCatch(
                      # LogM(tcrossprod(xMat, muMat)),
                      # error = function(e) 
                        # LogM(tcrossprod(xMat, muMat))
                      # )
                  # }
                 # })
  # res <- matrix(res, ncol=n)

  # res[c(lower.tri(diag(d))), , drop=FALSE]
}

# # Matrix logarithm
# # mat A matrix
# LogM_old <- function(mat, method='Higham08') {

#   LogM(mat, method)

# }

# Works only for d = 3
LogMSO3 <- function(mat, tol=1e-10) {

  if (nrow(mat) == 3) {
    val <- (sum(diag(mat)) - 1) / 2

    if (val > 1) {
      val <- 1
    } else if (val < -1) {
      val <- -1
    }
    theta <- acos(val)

    if (abs(theta) < tol) {
      res <- matrix(0, 3, 3)
    } else {
      res <- theta / (2 * sin(theta)) * (mat - t(mat))
    }

  } else {
    stop()
  }
  res
}


# ExpM_old <- function(mat, method='R_Eigen') {
#   ExpM(mat, method)
# }


ExpMSO3 <- function(mat, tol=1e-10) {

  if (nrow(mat) == 3) {
    a <- sqrt(0.5 * sum(mat^2))
    if (abs(a) < tol) {
      res <- diag(3)
    } else {
      res <- diag(3) + sin(a) / a * mat + (1 - cos(a)) / a^2 * mat %*% mat
    }
  } else {
    stop()
  }
  res
}


# Project extrinsic tangent space data onto the tangent space at p
# p must be a single point on the manifold

#' @export
#' @describeIn projectTangent Method
projectTangent.SO <- function(mfd, p, X, projMatOnly=FALSE, ...) {

  d <- round(sqrt(length(p)))
  stopifnot(isSO(matrix(p, d, d)))
  projMat <- diag(nrow=d)

  if (projMatOnly) {
    return(projMat)
  } else {
    return(as.matrix(X))
  }
}


## Helper functions from sphere/R/func.R
## Functions for rotational groups SO(n). c.f. Rahman et al 2006

isOrth <- function(x, tol=1e-14) {
  if (length(x) == 1) {
    x <- matrix(x, 1, 1)
  }

  d <- nrow(x)
  max(abs(crossprod(x) - diag(d))) <= tol
}


isSO <- function(x, tol=1e-14) {
  if (length(x) == 1) {
    x <- matrix(x, 1, 1)
  }

  d <- nrow(x)
  max(abs(crossprod(x) - diag(d))) <= tol && abs(det(x) - 1) <= tol
}


isSkewSym <- function(x, tol=1e-14) {
  max(abs(x + t(x) - 0)) <= tol
}


# Make a skew-symmetric matrix by specifying the lower triangular elements.
MakeSkewSym <- function(v) {

  p <- length(v)
  d <- calcGeomPar.SO(dimTangent=p)

  A <- matrix(0, d, d)
  A[lower.tri(A)] <- v
  A <- A - t(A)

  A
}


# Make a skew-symmetric matrix of functions by specifying the lower triangular functions.
MakeSkewSymFunc <- function(fList) {

    p <- length(fList)
    d <- calcGeomPar.SO(dimIntrinsic=p)

    outList <- matrix(list(), d, d)
    outList[] <- lapply(outList, function(l) {function(x) rep(0, length(x))})
    outList[lower.tri(outList)] <- lapply(fList, function(f) {
                                          function(x) -1 * f(x)
                                        }) 
    outList <- t(outList)
    outList[lower.tri(outList)] <- fList

    outList

}


# # Deprecated. Use LogM and distSO instead
# # Make an orthogonal matrix by specifying the lower triangular elements of the skew-symmetric logarithm matrix.  
# DistSO <- function(x1, x2) {

#   if (!is.matrix(x1)) {
#     d1 <- round(sqrt(length(x1)))
#     x1 <- matrix(x1, d1, d1)
#   }
#   if (!is.matrix(x2)) {
#     d2 <- round(sqrt(length(x2)))
#     x2 <- matrix(x2, d2, d2)
#   }

#   if (nrow(x1) == 3) {
#     R <- crossprod(x1, x2)
#     x <- (sum(diag(R)) - 1) / 2
#     if (x > 1) {
#       x <- 1
#     } else if (x < -1) {
#       x <- -1
#     }
#     sqrt(2) * abs(acos(x))
#   } else {
#     drop(sqrt(crossprod(as.numeric(LogM(crossprod(x1, x2), method='Eigen')))))
#   }
# }


# ExpMapSO <- function(V, mu, ...) {

  # if (is.matrix(mu)) {
    # stopifnot(dim(mu)[1] == dim(mu)[2])
    # d <- nrow(mu)
  # } else {
    # d <- round(sqrt(length(mu)))
    # stopifnot(identical(d, sqrt(length(mu))))
  # }

  # if (!is.matrix(V)) {
    # V <- matrix(V, nrow=1)
  # }

  # stopifnot(ncol(V) == length(mu))

  # muMat <- matrix(mu, d, d)
  # stopifnot(all.equal(crossprod(muMat), diag(d)))

  # res <- t(apply(V, 1, function(v) {
                   # vMat <- matrix(v, d, d)
                   # ExpM(vMat, ...) %*% muMat
                 # }))

  # res
# }


# ExpMap1 <- function(v, mu, type, ...) {

  # # stopifnot(length(v) == length(mu))

  # if (type == 'SO') {
    # d <- round(sqrt(length(mu)))

    # vMat <- matrix(v, d, d)
    # muMat <- matrix(mu, d, d)
    # res <- as.numeric(ExpM(vMat, ...) %*% muMat)
  # }

  # res
# }


# # works only for SO(3). c.f. Rahman et al 2006 and https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
# ExpMap2 <- function(v, mu, type, ...) {

  # # stopifnot(length(v) == length(mu))
  # # stopifnot(length(v) == 9)

  # if (type == 'SO') {
    # d <- round(sqrt(length(mu)))

    # vMat <- matrix(v, d, d)
    # muMat <- matrix(mu, d, d)
    # vNorm <- sqrt(sum(v^2) / 2)
    # res <- diag(3) + sin(vNorm) / vNorm * vMat + (1 - cos(vNorm)) / vNorm^2 * vMat %*% vMat
    # res <- as.numeric(res %*% muMat)
  # }

  # res
# }



# Get the nearest orthogonal matrix
NearestOrth <- function(A) {
  if (length(A) == 1) {
    A <- matrix(A)
  }
  stopifnot(is.matrix(A))
  stopifnot(nrow(A) == ncol(A))

  s <- svd(A)

  tcrossprod(s[['u']], s[['v']])
}


# Get the nearest SO matrix
#' @export

#' @export
#' @describeIn project Method
project.SO <- function(mfd, p) {

  A <- as.matrix(p)
  d <- round(sqrt(nrow(A)))

  res <- apply(A, 2, function(a) {
    a <- matrix(a, d, d)
    N <- NearestOrth(a)
    d <- nrow(N)
    if (!isSO(N)) {
      N[, d] <- -N[, d]
    }
    c(N)
  })
  matrix(res, nrow(A), ncol(A))
}


# Calculate the parameter n of SO(n) given the length of the lower triangle

#' @export
calcGeomPar.SO <- function(mfd, dimAmbient, dimIntrinsic, dimTangent) {

  if (!missing(dimIntrinsic)) {
    dimTangent <- dimIntrinsic
  } else if (!missing(dimAmbient)) {
    return(as.integer(sqrt(dimAmbient)))
  } else if (!missing(dimTangent)) {
  }

  d0 <- sqrt(2 * dimTangent + 1/4) + 1/2 # Root of a quadratic equation
  n <- round(d0)

  if (n != d0) {
    stop('The input is not the dimTangent of a lower triangular matrix')
  }

  as.integer(n)
}


#' @export
calcIntDim.SO <- function(mfd, geomPar, dimAmbient, dimTangent) {

  if (!missing(geomPar)) {
    n <- geomPar
  } else if (!missing(dimAmbient)) {
    n <- round(sqrt(dimAmbient))
  } else if (!missing(dimTangent)) {
    return(as.integer(dimTangent))
  }
  as.integer(n * (n - 1) / 2)
 
}


#' @export
calcTanDim.SO <- function(mfd, geomPar, dimAmbient, dimIntrinsic) {

  if (!missing(geomPar)) {
    as.integer(geomPar * (geomPar - 1) / 2)
  } else if (!missing(dimAmbient)) {
    as.integer(calcIntDim.SO(dimAmbient=dimAmbient))
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic)
  }

}


#' @export
calcAmbDim.SO <- function(mfd, geomPar, dimIntrinsic, dimTangent) {
  if (!missing(geomPar)) {
    as.integer(geomPar ^ 2)
  } else if (!missing(dimIntrinsic)) {
    as.integer(calcGeomPar.SO(mfd, dimIntrinsic=dimIntrinsic)^2)
  } else if (!missing(dimTangent)) {
    as.integer(calcGeomPar.SO(mfd, dimTangent=dimTangent)^2)
  }
}


#' @export
#' @describeIn origin The origin has 1 in the first ambient coordinate and 0 otherwise.
origin.SO <- function(mfd, dimIntrinsic, ...) {
  matrix(diag(calcGeomPar(mfd, dimIntrinsic=dimIntrinsic)))
}


#' @export
#' @describeIn basisTan An identity matrix
basisTan.SO <- function(mfd, p) {
  diag(nrow=calcIntDim(mfd, dimAmbient=length(p)))
}


#' Returns the angle representation of SO(3) matrices
#' c.f. \url{https://en.wikipedia.org/wiki/Axis-angle_representation}
#' @param mfd A manifold object created by \code{\link{createM}}
#' @param X A matrix holding a vectorized SO(3) matrix in each column
#' @returns A matrix with 4 rows and the same number of columns as X. The first row contains the angles in rads (theta), and the last three rows correspond to the the axes with respect to which the rotations are performed.
#' @export
axisAngleRep <- function(mfd, X) {
  
  X <- as.matrix(X)

  if (!inherits(mfd, 'SO') || nrow(X) != 9) {
    stop('Axis-angle representation only works for SO(3)')
  }

  res <- apply(X, 2, function(x) {
                 # browser()
    R <- matrix(x, 3, 3)
    t1 <- (sum(diag(R)) - 1) / 2 
    theta <- acos(t1)
    tol <- 1e-12
    if (abs(theta) < tol) {
      omega <- c(1, 0, 0)
    } else if (abs(theta) > pi - tol) {
      B <- (R + diag(3)) / 2
      omega <- sqrt(diag(B))
    } else {
      omega <- 1 / (2 * sin(theta)) * 
        c(R[3, 2] - R[2, 3],
          R[1, 3] - R[3, 1],
          R[2, 1] - R[1, 2])
    }

    c(theta, omega)
  })

  matrix(res, 4, ncol(X))
}
