

#' @export
#' @describeIn metric Method
metric.Sphere <- function(mfd, p, U, V) {
  NextMethod()
}


#' @export
#' @describeIn norm Method
norm.Sphere <- function(mfd, p, U) {
  NextMethod()
}

# #' The Euclidean norm of tangent vectors. Only meaningful for Euclidean submanifolds
# #' 
# #' @param U A D*n matrix. Each column represents a tangent vector in the ambient space

# #' Geodesic distance of points on the manifold
# #' 
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param X A D*n matrix. Each column represents a point on manifold
# #' @param Y A D*n matrix. Each column represents a point on manifold
# #' @returns A 1*n vector. The \emph{i}th element is the geodesic distance of \code{X[, i]} and \code{Y[, i]}

#' @export
#' @describeIn distance Method
distance.Sphere <- function(mfd, X, Y, ...) {

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

  val <- unname(colSums(X * Y))
  val[val > 1] <- 1
  val[val < -1] <- -1
  acos(val)
}

#' @param mfd A class instance that represents the Riemannian manifold
#' @param p The starting point of the geodesic curve
#' @param h A matrix with each column representing a tangent vector. If there is only one tangent vector is supplied, then it is replicated to match the length of \emph{t}
#' @param t A vector, the time points where the curve is evaluated.
#' 
#' @details The curve is \eqn{\gamma(t)=\mathrm{Exp}_p(th)}
#' 
#' @returns A matrix with each column representing a point on the manifold
#' @export
#' @describeIn geodesicCurve Geodesic curve stating at a point
geodesicCurve.Sphere <- function(mfd, p, h, t)
{
    if(!is.matrix(p)) p <- as.matrix(p)
    if(!is.matrix(h)) h <- as.matrix(h)
    stopifnot(all(abs(crossprod(p, h) - 0) < 1e-8))
    n <- dim(h)[2]
    d <- dim(h)[1]
    m <- length(t)

    if (n==1) {
      h <- h %*% matrix(t, nrow=1)
    } else if (m != n) {
      stop('If there is more than one tangent vectors, the number of tangent vectors must be equal to the number of time points')
    }

    rieExp.Sphere(mfd, p, h)
    
}


#' @export
#' @describeIn rieExp Method
rieExp.Sphere <- function(mfd, p, V, ...) {

  tol <- 1e-10

  V <- as.matrix(V)
  p <- as.matrix(p)
  m <- dim(p)[2]
  n <- dim(V)[2]
  d <- dim(V)[1]

  if (length(p) == 0 || length(V) == 0) {
    return(matrix(0, nrow(p), 0))
  }

  if (m == 1 && n != 1) {
    p <- matrix(p, d, n)
  } else if (m > 1 && n == 1) {
    V <- matrix(V, d, m)
  }
  mn <- max(m, n)

  # p needs to be on a unit sphere
  # stopifnot(abs(sum(p^2) - 1) <= tol)

  stopifnot(nrow(V) == nrow(p))

  # each col of V needs to be orthogonal to p
  stopifnot(all(abs(colSums(p * V)) <= tol))

  res <- vapply(seq_len(mn), function(i) {
    Exp1(V[, i], p[, i], tol=tol)
  }, rep(0, d))
  if (!is.matrix(res)) {
    res <- matrix(res, nrow=1)
  }
  res
}


# #' Riemannian log map at a point
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param p A matrix. Each column represents a base point of the log map. If only one base point is supplied, then it is replicated to match the number of points in \emph{X}
# #' @param X A matrix. Each column represents a point on the manifold
# #' @returns A matrix with the \emph{i}th column being the log map of the \emph{i}th point


#' @export
#' @describeIn rieLog Method
rieLog.Sphere <- function(mfd, p, X,  tol=1e-10, ...) {

  X <- as.matrix(X)
  p <- as.matrix(p)
  m <- dim(p)[2]
  n <- dim(X)[2]
  d <- dim(X)[1]

  if (length(p) == 0 || length(X) == 0) {
    return(matrix(0, calcTanDim.Sphere(dimAmbient=d), 0))
  }

  if (m == 1 && n != 1) {
    p <- matrix(p, d, n)
  } else if (m > 1 && n == 1) {
    X <- matrix(X, d, m)
  }
  mn <- max(m, n)

  # # p needs to be on a unit sphere
  # stopifnot(all(abs(colSums(p^2) - 1) <= tol))

  # # each column of X needs to be on a unit sphere
  # stopifnot(all(abs(apply(X, 2, function(x) sum(x^2)) - 1) <= tol))

  # Z <- vapply(seq_len(n), function(i) {
  # Log1(X[, i], p[, i], tol)
  # }, rep(0, d))
  Z <- Log2(X, p, tol)

  return(Z)
}


Log2 <- function(X, Mu, tol=1e-10) {
  dimAmbient <- nrow(X)
  N <- ncol(X)
  cprod <- colSums(X * Mu)
  U <- X - matrix(cprod, dimAmbient, N, byrow=TRUE) * Mu
  uNorm <- sqrt(colSums(U^2))
  res <- matrix(0, dimAmbient, N)
  ind <- uNorm > tol
  distS <- acos(ifelse(cprod > 1, 1, ifelse(cprod < -1, -1, cprod)))
  res[, ind] <- (U * matrix(distS / uNorm, dimAmbient, N, byrow=TRUE))[, ind, drop=FALSE]
  res
}

# Log1 <- function(x, mu, tol=1e-10) {
  # u <- x - c(crossprod(x, mu)) * mu
  # uNorm <- sqrt(c(crossprod(u)))
  # if (!is.na(uNorm) && uNorm <= tol) {
    # rep(0, length(x))
  # } else {
    # u / uNorm * DistS(x, mu)
  # }
# }

DistS <- function(x1, x2) {
  val <- crossprod(x1, x2)[1]
  val[val > 1] <- 1
  val[val < -1] <- -1
  acos(val)
}




# #' @export
# frechetMean.Sphere <- function(mfd, X, weight=NULL, tol=1e-9, maxit=1000) {

  # X <- as.matrix(X)
  # d <- nrow(X)
  
  # if (is.null(weight)) {
    # n <- dim(X)[2]
    # weight <- rep(1 / n, n)
    # mu0 <- apply(X, 1, mean)
  # } else {
    # weight <- weight / sum(weight)
    # ind <- weight > 1e-15
    # n <- sum(ind)
    # X <- X[, ind, drop=FALSE]
    # weight <- weight[ind]
    # mu0 <- apply(X*t(replicate(d, weight)), 1, sum)
  # }

  # mu0 <- as.matrix(mu0)
  # mu0 <- mu0 / norm.default(U=mu0) # maybe zero?

  # it <- 0
  # dif <- Inf
  # SS <- Inf

  # # browser()
  # while(dif > tol && it <= maxit) {
    # it <- it + 1
    # V <- rieLog(mfd, mu0, X)
    # vNew <- as.numeric(rowSums(V * matrix(weight, d, n, byrow=TRUE)))
    # muNew <- as.numeric(rieExp(mfd, mu0, vNew))
    # SSNew <- sum(scale(t(V), scale=FALSE)^2)
    # dif <- abs(SS - SSNew)

    # mu0 <- Normalize(muNew)
    # SS <- SSNew
  # }

  # if (it == maxit) {
    # stop('Maximum iteration reached!')
  # }

  # matrix(mu0)
# }


# #' Frechet mean of a set of points
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param X A matrix. Each column is a point on the manifold
# #' @param weight A vector. The weight for each point in \emph{X}
# #' @param opt.control Controls for optimization procedure of Rsolnp
# #' 
# #' @returns A vector representing the Frechet mean of \emph{X}
# # NOTE by XD: The returned point is not quite on the sphere - numerical error is large
# frechetMean.Sphere2D <- function(mfd, X, weight=NULL,
                                 # opt.control=list(outer.iter=40, tol=1e-5, trace=0))
# {
    # X <- as.matrix(X)
    # n <- dim(X)[2]
    # objFunc <- function(p){
        # P <- matrix(p, 3, n)
        # if(is.null(weight))
            # return(sum(distance(mfd, X, P)))
        # else
            # return(sum(distance(mfd, X, P)*weight))
    # }
    # equalFunc <- function(p)
    # {
        # p[1]^2+p[2]^2+p[3]^2
    # }
    
    # if(is.null(weight)) p0 <- apply(X, 1, mean)
    # else p0 <- apply(X*t(replicate(3, weight)), 1, mean)

    # p0 <- as.matrix(p0)
    # p0 <- p0 / euclideanNorm(p0) # maybe zero?
    
    # res <- Rsolnp::solnp(p0,
                  # fun=objFunc,
                  # eqfun=equalFunc,
                  # eqB=1,
                  # LB=c(-1,-1,-1),
                  # UB=c(1, 1, 1),
                  # control=opt.control)
    # if(res$convergence==0) return(res$pars)
    # else
    # {
        # opt.control$outer.iter <- which(res$values==min(res$values))
        
        # # not efficient, maybe we need to hack the solnp ...
        # res1 <- Rsolnp::solnp(p0,
                             # fun=objFunc,
                             # eqfun=equalFunc,
                             # eqB=1,
                             # LB=c(-1,-1,-1),
                             # UB=c(1, 1, 1),
                             # control=opt.control)
        # return(project(mfd, res1$pars))
    # }
# }

# #' Frechet mean curve of a set of sparsely observed curves on the manifold
# #' @param mfd A class instance that represents the Riemannian manifold
# #' @param bw The bandwidth
# #' @param kernel_type The type of kernel for smoothing
# #' @param yin A list of \emph{n} matrices containing the observed values for each individual. Missing values specified by \code{NA}s are supported for dense case (\code{dataType='dense'}).
# #' @param xin A list of \emph{n} vectors containing the observation time points for each individual corresponding to y. Each vector should be sorted in ascending order.
# #' @param xout The time points where the mean curve shall be evaluated
# #' @param npoly The order of the local polynomial smoother
# #' @param win A vector \emph{n} numbers. The weight of each individual.
# #' 
# #' @returns A matrix with \code{length(xout)} columns. The \emph{i}th column is the estimated mean curve at \code{xout[i]} 


# #' @export
# frechetMeanCurve.Sphere <- function(mfd, bw, kernel_type, xin, 
                                    # yin, xout, npoly = 1L, 
                                    # win=rep(1L, length(xin))){

  # if (!is.matrix(yin)) {
    # yin <- matrix(yin, nrow=1)
  # }

  # if(is.unsorted(xout)){
    # stop('`xout` needs to be sorted in increasing order')
  # }

  # if(all(is.na(win)) || all(is.na(xin)) || all(is.na(yin))){
    # stop(' win, xin or yin contain only NAs!')
  # }

  # # Deal with NA/NaN measurement values
  # NAinY = is.na(xin) | is.na(yin) | is.na(win)
  # if(any(NAinY)){
    # win = win[!NAinY]
    # xin = xin[!NAinY]
    # yin = yin[!NAinY]
  # } 

  # # kw <- getKernelWeight(kernel_type, bw, xin, xout, win)
  # # browser()
  # kw <- getKernelWeight1(kernel_type, bw, xin, xout, win, npoly)

  # # if(npoly == 0) # constant kernel smoothing
  # # {
  # # could be faster if local compact kernel by drop points with zero weight
  # meanCurve <- vapply(seq_along(xout), function(i) frechetMean(mfd, yin, weight=kw[, i]), rep(0, nrow(yin)))
  # # }
  # # else # local linear smoothing
  # # {
  # # stop('npoly >= 1 not supported yet')
  # # #meanCurve <- matrix(0, 3, nout)
  # # }

  # if (!is.matrix(meanCurve)) {
    # matrix(meanCurve, nrow=1)
  # }

  # return(meanCurve)
# }


#' @export
#' @describeIn project Method
project.Sphere <- function(mfd, p) {
  p <- as.matrix(p)
  res <- apply(p, 2, Normalize, tol=1e-13)
  matrix(res, nrow=nrow(p))
}



#' @export
#' @describeIn projectTangent Method
# Project ambient space data onto the tangent space at p
projectTangent.Sphere <- function(mfd, p, X, projMatOnly=FALSE, ...) {

  dd <- length(p)
  stopifnot(abs(sum(p^2) - 1) < 1e-14)
  projMat <- diag(nrow=dd) - c(tcrossprod(p))

  if (projMatOnly) {
    return(projMat)
  } else {
    return(crossprod(projMat, X))
  }
}

# vector cross product
# u, v: 3 by n vectors
# xprod <- function(u, v) {
    # return( rbind(u[2,]*v[3,]-u[3,]*v[2,], 
              # u[3,]*v[1,]-u[1,]*v[3,], 
              # u[1,]*v[2,]-u[2,]*v[1,]) )
# }


Exp1 <- function(v, mu, tol=1e-10) {
  vNorm <- as.numeric(sqrt(crossprod(v)))
  if (!is.na(vNorm) && vNorm <= tol) {
    mu
  } else {
    cos(vNorm) * mu + sin(vNorm) * v / vNorm
  }
}


#' @export
calcGeomPar.Sphere <- function(mfd, dimAmbient, dimIntrinsic, dimTangent) {

  if (!missing(dimIntrinsic)) {
    dimIntrinsic
  } else if (!missing(dimAmbient)) {
    dimAmbient - 1
  } else if (!missing(dimTangent)) {
    dimTangent - 1
  }
}


#' @export
calcIntDim.Sphere <- function(mfd, geomPar, dimAmbient, dimTangent) {

  if (!missing(geomPar)) {
    as.integer(geomPar)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient - 1)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent - 1)
  }

}


#' @export
calcTanDim.Sphere <- function(mfd, geomPar, dimAmbient, dimIntrinsic) {

  if (!missing(geomPar)) {
    as.integer(geomPar + 1)
  } else if (!missing(dimAmbient)) {
    as.integer(dimAmbient)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic + 1)
  }

}


#' @export
calcAmbDim.Sphere <- function(mfd, geomPar, dimIntrinsic, dimTangent) {
  if (!missing(geomPar)) {
    as.integer(geomPar + 1)
  } else if (!missing(dimIntrinsic)) {
    as.integer(dimIntrinsic + 1)
  } else if (!missing(dimTangent)) {
    as.integer(dimTangent)
  }
}


#' @export
#' @describeIn origin The origin has 1 in the first ambient coordinate and 0 otherwise.
origin.Sphere <- function(mfd, dimIntrinsic, ...) {
  matrix(c(1, rep(0, dimIntrinsic)))
}



#' @export 
#' @describeIn basisTan The basis at the north pole is [0, ..., 1, ..., 0] where the 1 is at the j = 2, ..., dAmbth location. The basis at a point p is obtained through rotating the basis from the north pole to p along the shortest geodesic. 
basisTan.Sphere <- function(mfd, p) {

  p <- matrix(p, ncol=1)
  d <- calcIntDim.Sphere(mfd, dimAmbient=length(p))

  tmp <- diag(nrow = d + 1)[, -1, drop=FALSE]
  D <- MakeRotMat(origin(mfd, d), p) %*% tmp
  D

}


#' Generate uniform random variables on the unit sphere
#' @param n Sample size
#' @param dimAmbient The dimension of the ambient space
#' @returns A `dimAmbient` by `n` matrix. Each column is a random observation
#' @export
runifSphere <- function(n, dimAmbient) {
  X <- matrix(stats::rnorm(n * dimAmbient), dimAmbient, n)
  matrix(apply(X, 2, Normalize), dimAmbient, n)
}
