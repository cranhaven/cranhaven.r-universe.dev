#' Calculate the Metric Halfspace Depth
#'
#' Apply the metric halfspace depth algorithm (Dai and Lopez-Pintado (2022) <doi:10.1080/01621459.2021.2011298>) to calculate the metric halfspace depth at a set of evaluation points with respect to a data cloud. Also calculate the deepest points both in- and out-of-sample.
#'
#' @param mfd The manifold where the data is supported on. For example, the Euclidean space is generated using `manifold::createM('Euclidean')`, and the (hyper)sphere is `manifold::createM('Sphere')`. See `manifold` package for more details.
#' @param data A data frame giving the data points. Each row is an observation.
#' @param anchors A data frame for the anchor points for evaluating the halfspace probabilities. If missing, default to the data points together with possibly the jiggled points. 
#' @param XEval A data frame for additional points at which the depth should be evaluated. Defaults to nothing.
#' @param theta0 A vector for the initial value for searching for the deepest out-of-sample point.
#' @param depthOnly Calculate the depth values only if `TRUE`, or both the depth values and the out-of-sample deepest point if `FALSE` (default). The calculation of the deepest point can be time consuming.
#' @param optGlo,optLoc Lists of user specified option for the global and the local optimization steps, respectively. Follows the specification of nloptr::nloptr. For a list of options, use `nloptr.print.options()`, and for the list of algorithms see https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/  One should apply a derivative-free optimizers, and in particular one of `c('NLOPT_GN_DIRECT_NOSCAL', 'NLOPT_GN_DIRECT', 'NLOPT_GN_CRS2_LM', 'NLOPT_GN_MLSL_LDS')` in the global step and one of `c('NLOPT_LN_NELDERMEAD', 'NLOPT_LN_SBPLX')` for the local step. 
#' @param jiggle An interger. The number of jiggled points per data point to add into the dataset. This is for making the approximated MHD depth more precise. 
#' @param jiggleQuantile A numeric scalar. The amount of jiggling is determined by the `jiggleQuantile` quantile of the nonzero pairwise distances between the data and the anchor points.
#' @return A list containing the following fields:
#' \item{sampDeepest}{The in-sample deepest point} 
#' \item{depthSamp}{The depth of the evaluation points}   
#' \item{depthDeepest}{The depth at the deepest out-of-sample point}
#' \item{xDeepest}{The deepest out-of-sample point}    
#' \item{theta0}{Initial value for the search of the deepest point}      
#' \item{optGlo}{Options used for the global search}      
#' \item{optLoc}{Options used for the local search}      
#' \item{nloptTime}{Time used by the optimization procedure}   
#' @examples
#' mfd <- manifold::createM('Euclidean')
#' 
#' n <- 100
#' d <- 4
#' data <- matrix(rnorm(n * d), n, d)
#' anchors <- matrix(rnorm(n * d), 2 * n, d)
#' 
#' # The default
#' depthObj1 <- MHD(mfd, data)
#' 
#' # more precise, but slower
#' \donttest{
#' depthObj2 <- MHD(mfd, data, anchors) 
#' }
#' 
#' # Do not search for the deepest point. Faster
#' depthObj3 <- MHD(mfd, data, depthOnly=TRUE) 

# Note that in the returned nloptr objects, the optimal solutions are the coordinates of a tangent vector, but not the points on the manifold themselves. Should be a small number.

#' @export
MHD <- function(mfd, data, anchors, XEval, theta0, 
                depthOnly      = FALSE,
                optGlo         = list(),
                optLoc         = list(),
                jiggle         = 0,
                jiggleQuantile = 0.01) {

  tol <- 1e-14

  Dist <- function(x, Y) {
    manifold::distance(mfd, x, t(Y))
  }
  dInt <- manifold::calcIntDim(mfd, dimAmbient=ncol(data))

  if (missing(anchors)) {
    anchors <- data
  } 

  datAnc <- PairwiseDistance2(data, anchors, Dist)

  if (jiggle >= 1) {
    nAnchors <- nrow(anchors)
    multJig <- quantile(datAnc[datAnc > tol], jiggleQuantile)
    anchorsJig <- lapply(seq_len(nAnchors), function(i) {
      x <- anchors[i, , drop=TRUE]
      if (dInt >= 1) {
        V0Jig <- manifold::runifSphere(jiggle, dimAmbient=dInt) * multJig
      }
      VJig <- manifold::coordToTanV(mfd, x, V0Jig)
      XJig <- rieExp(mfd, x, VJig)
      t(XJig)
    })
    anchorsJig <- do.call(rbind, anchorsJig)
    anchors <- rbind(anchors, anchorsJig)
    datAnc <- cbind(datAnc, PairwiseDistance2(data, anchorsJig, Dist))
  }

  ancX <- t(datAnc)

  if (!missing(XEval)) {
    if (!is.matrix(XEval)) {
      XEval <- matrix(XEval, nrow=1)
    }
    ancX <- cbind(ancX, PairwiseDistance2(anchors, XEval, Dist))
  }

  tmp <- MHD3_cpp(datAnc, ancX)
  prop <- tmp$prop
  depth <- tmp$depth[seq_len(nrow(data))]
  indSamp <- which.max(depth)
  sampDeepest <- data[indSamp, , drop=TRUE]

  res <- list(sampDeepest  = sampDeepest,
              depthSamp    = depth)

  if (!missing(XEval)) {
    depthXEval <- tmp$depth[-seq_len(nrow(data))]
    res <- c(res, list(depthX = depthXEval))
  }

  if (!depthOnly) {
    if (missing(theta0)) {
      theta0 <- sampDeepest
    }

    # orig <- manifold::origin(mfd, dInt)
    # origPar <- crossprod(basis, rieLog(mfd, theta0, orig))
    # origObjective <- -F(origPar)

    # Set the bounding box as the smallest one containing the 50% deepest samples
    V <- t(rieLog(mfd, theta0, t(data[depth >= median(depth), , drop=FALSE])))
    basis <- svd(V, nv=dInt)$v[, seq_len(dInt), drop=FALSE]

    F <- function(v0) {
      v <- basis %*% as.matrix(v0)
      expv <- t(rieExp(mfd, theta0, v))
      D <- PairwiseDistance2(anchors, expv, Dist)
      ind <- outer(D, D, '-') <= 0 + tol
      vec <- prop[ind]
      min(vec)
    }
    parData <- V %*% basis

    lb <- apply(parData, 2, min) - tol
    ub <- apply(parData, 2, max) + tol
    x0 <- rep(0, ncol(basis))

    # Default options for nloptr
    opt1 <- list(
      algorithm = 'NLOPT_GN_DIRECT_NOSCAL', 
      maxeval = 200 * dInt, 
      xtol_abs = 1e-04,
      print_level = 0# , 
      # local_opts = list(algorithm = 'NLOPT_LN_SBPLX',
                        # xtol_abs = 1e-04)
    )
    opt2 <- list(
      algorithm = 'NLOPT_LN_NELDERMEAD', 
      xtol_abs = 1e-04, 
      maxeval = 50 * dInt,
      print_level = 0)

    # Plug in user options
    opt1[names(optGlo)] <- optGlo
    opt2[names(optLoc)] <- optLoc

    # Optimize
    F0 <- F(x0)

    time <- system.time({
      resGlo <- nloptr::nloptr(x0, function(x) -F(x), lb=lb, ub=ub, opts=opt1)
      resGlo[c('eval_f', 'nloptr_environment')] <- NULL
      if (resGlo$objective > -F0) {
        x00 <- x0
      } else {
        x00 <- resGlo$solution
      }

      resLoc <- nloptr::nloptr(x00, function(x) -F(x), lb=lb, ub=ub, opts=opt2)
      resLoc[c('eval_f', 'nloptr_environment')] <- NULL
      if (resLoc$objective > -F0) {
        solution <- x0
        objective <- -F0
      } else {
        solution <- resLoc$solution
        objective <- resLoc$objective
      }
    })

    deepest <- c(rieExp(mfd, theta0, basis %*% solution))

    res <- c(res, 
             list(depthDeepest = -objective,
                  xDeepest     = deepest,
                  theta0       = theta0,
                  optGlo       = resGlo,
                  optLoc       = resLoc,
                  nloptTime    = time['user.self']))
  }

  class(res) <- 'MHD'
  res
}


MHD_tree <- function(data, jiggle = 0, jiggleQuantile = 0.01) {

  tol <- 1e-14

  if (jiggle >= 1) {

    dataDist <- distory::dist.multiPhylo(data, method = 'geodesic')
    multJig <- quantile(dataDist, jiggleQuantile)

    # Only jiggle within the same stratum
    jiggledData <- do.call(c, lapply(data, function(tree) {

      jigDim <- which(tree$edge.length > tol)
      dInt <- length(jigDim)

      XJig <- replicate(jiggle, {

        tmp <- tree

        if (dInt >= 1) {
          v <- c(manifold::runifSphere(1, dimAmbient=dInt) * multJig)
        } else if (dInt == 0) {
          return()
        }

        newlen <- tmp$edge.length
        newlen[jigDim] <- newlen[jigDim] + v
        newlen[newlen < 0] <- 0
        tmp$edge.length <- newlen
        tmp
      }, simplify=FALSE)

      XJig

    }))

    class(jiggledData) <- 'multiPhylo'
    allData <- c(data, jiggledData)
  } else {
    allData <- data
  }

  datInd <- seq_len(length(data))
  ancInd <- seq_len(length(allData))

  allDist <- as.matrix(distory::dist.multiPhylo(allData, method = 'geodesic'))

  datAnc <- allDist[datInd, ancInd, drop=FALSE]
  ancX <- t(datAnc)

  depth <- MHD2_cpp(datAnc, ancX)

  indSamp <- which.max(depth)
  sampDeepest <- data[[indSamp]]

  res <- list(sampDeepest  = sampDeepest,
              depthSamp    = depth)
  class(res) <- 'MHD'
  res
}


#TODO: make nicer
#' @export
print.MHD <- function(x, ...) {
  NextMethod()
}


#' @export
str.MHD <- function(object, ...) {
  NextMethod(max.level=1)
}


# Does not completely agree with the TUkey halfspace depth, because our algorithm does not make use of the underlying space, but just pairwise distances.
# Higher depth regions are better estimated than lower depth regions. For lower depth region there are not enough good surrounding data points that provide separating halfspaces. 
MHalfDepth <- function(distM, xInd, dataInd) {
  dataDist <- distM[dataInd, dataInd, drop=FALSE]
  nDat <- nrow(dataDist)

  a <- vapply(xInd, function(i) {
    xDataDist <- distM[i, dataInd, drop=TRUE]
    # Locate halfspaces H_{x_1,x_2} containing x
    ord <- order(xDataDist)
    tmp <- which(upper.tri(matrix(NA, nDat, nDat), diag=FALSE), arr.ind = TRUE)
    x1x2Ind <- cbind(ord[tmp[, 1]], ord[tmp[, 2]])
    HSprop <- apply(x1x2Ind, 1, function(x1x2i) {
      # Proportion of data that are closer to x1 than to x2
      mean(dataDist[x1x2i[1], ] <= dataDist[x1x2i[2], ])
    })
    min(HSprop)
  }, 0.1)
  a
}


# Need three sets of x: A query set where the depths are evaluated at; an anchor set for defining the collection of subspaces; a data set to evaluate the halfspace proportions.
# This implementation assumes the anchor set and the data set to be common.
MHD2 <- function(distM, xInd, dataInd) {
  dataDist <- distM[dataInd, dataInd, drop=FALSE]
  nDat <- nrow(dataDist)
  nAnchor <- nDat

  # Get the halfspace proportions for halfspaces specified by the anchor points
  Prop <- # matrix(NA, nAnchor, nAnchor)
    sapply(seq_len(nAnchor), function(j) {
    sapply(seq_len(nAnchor), function(i) {
      mean(dataDist[, i] <= dataDist[, j])
    })
    })

  a <- vapply(xInd, function(i) {
    xDataDist <- distM[dataInd, i, drop=TRUE]
    tmp <- expand.grid(xx1 = xDataDist, xx2 = xDataDist)
    ind <- matrix(tmp$xx1 <= tmp$xx2, nAnchor, nAnchor)
    min(Prop[ind])
  }, 0.1)
  a
}



