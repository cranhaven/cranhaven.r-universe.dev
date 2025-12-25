PVS <- function(X, kmax = 3, dist = "euclidean",
                        method = "gap", B = 1000,
                        gapMethod = "firstSEmax",
                        minSize = 0.05, rDist = runif,
                        SE.factor = 1, 
                        refDist = NULL) {
  # Computed the pooled scale estimator for scaling prior to cluster analysis
  #
  # Arguments:
  #   X: n x p dataset
  #   kmax: maximum number of clusters in one variable
  #   dist: "euclidean" for pooled standard deviation and "manhattan" for 
  #         pooled mean absolute deviation.
  #   method: either "gap" or "jump" to determine the number of clusters
  #   B: number of bootstrap samples for the reference distribution of the gap
  #      statistic
  #   gapMethod: method to define number of clusters in the gap statistic. See
  #           ?cluster::maxSE for more info
  #   minSize: minimum cluster size 
  #   rDist: reference distribution for the gap statistic
  #   SE.factor: factor for determining number of clusters. See 
  #              ?cluster::maxSE
  #   refDist: optional k x 2 matrix with de reference distribution of the 
  #            gap statistic to avoid bootstrapping
  #
  # Returns: 
  #   scales: vector with scale of each variable
  # 
  
  n <- dim(X)[1]
  p <- dim(X)[2]
  
  minSize <- round(minSize * n) # minimum cluster size
  
  scales <- matrix(1, nrow = length(SE.factor), ncol = p) # output vector
  
  if (dist == "euclidean") {
    clustMethod <- Ckmeans.1d.dp::Ckmeans.1d.dp
  } else if (dist == "manhattan") {
    if (gapMethod == "jump") {
      stop("no jump with manhattan")
    }
    clustMethod  <- Ckmeans.1d.dp::Ckmedian.1d.dp
  } else {
    stop("invalid dist")
  }
  
  if (method == "gap") {
    # bootstrap the reference distribution of W
    # We bootstrap for the uniform[0, 1] distribution
    # when comparing a kmeans-clustering for a variable with
    # this distribution, we have to make sure to divide de 
    # variable by its range. Note that it may not be so accurate
    # when we have loads of NAs, since then the bootstrapped distribution
    # uses n points.
    
    if (is.null(refDist)) {
      refDist <- matrix(0, kmax, 2)
      for (k in 1:kmax) {
        bs_samples <- replicate(B,
                                log(sum(clustMethod(x = rDist(n),
                                                    k = k)$withinss)))
        refDist[k, 1] <- mean(bs_samples)
        refDist[k, 2] <- sd(bs_samples) * sqrt(1 + 1 / B)
      }
    }
    # perform kmeans on each variable and compare with reference distribution
    for (i in 1:p) {
      v <- na.omit(X[, i])
      if (length(unique(v)) < kmax * 2) {
        scales[i] <- sd(v)
      } else {
        gapStat <- minClusSize <- rep(0, kmax)
        for (k in 1:kmax) {
          kmeans.out     <- clustMethod(x = v, k = k)
          gapStat[k]     <- log(sum(kmeans.out$withinss))
          minClusSize[k] <- min(kmeans.out$size)
        }
        sizeable <- which(minClusSize >= minSize)
        gapStat  <- gapStat[sizeable]
        for (j in 1:length(SE.factor)) {
          if (dist == "euclidean") {
            nbClus   <- cluster::maxSE(refDist[sizeable, 1] -
                                         (gapStat - 2 * log(diff(range(v)))),
                                       SE.f = refDist[sizeable, 2],
                                       method = gapMethod, SE.factor = SE.factor[j])
            
            scales[j, i] <- sqrt(exp(gapStat[nbClus]) / length(v))
          } else {
            nbClus   <- cluster::maxSE(refDist[sizeable, 1] -
                                         (gapStat - log(diff(range(v)))),
                                       SE.f = refDist[sizeable, 2],
                                       method = gapMethod, SE.factor = SE.factor[j])
            scales[j, i] <- exp(gapStat[nbClus]) / length(v) * sqrt(pi / 2)
          }
        }
      }
    }
  } else if (method == "jump") {
    for (i in 1:p) {
      v <- na.omit(X[, i])
      if (length(unique(v)) < kmax * 2) {
        scales[i] <- sd(v)
      } else {
        W <- minClusSize <- rep(0, kmax)
        for (k in 1:kmax) {
          kmeans.out     <- clustMethod(x = v, k = k)  
          W[k]           <- sum(kmeans.out$withinss)
          minClusSize[k] <- min(kmeans.out$size)
        }
        jumpstat  <- diff(c(0, W^{-1/2}))
        W         <- W[which(minClusSize >= minSize)]
        jumpstat  <- jumpstat[which(minClusSize >= minSize)]
        scales[i] <- sqrt(W[which.max(jumpstat)] / n)
      }
    }
  }
  return(scales)
}
