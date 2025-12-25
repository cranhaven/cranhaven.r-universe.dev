

KM <- function(
  X,
  centroid,
  Xw = rep(1, ncol(X)),
  minkP = 2,
  maxIter = 100L,
  maxCore = 7L,
  verbose = TRUE
)
{
  Xw = Xw + 0.0 # Realize language.
  if(minkP == "cosine") minkP = -1e308
  else if(minkP == "max") minkP = 0


  KMcpp(
    X = X,
    centroid = centroid,
    Xw = Xw,
    minkP = minkP,
    maxCore = maxCore,
    maxIter = maxIter,
    verbose = verbose)
}






KMconstrained <- function(
  X,
  centroid,
  Xw = rep(1, ncol(X)),
  clusterWeightUB = rep(ncol(X) + 1, ncol(centroid)),
  minkP = 2,
  convergenceTail = 5L,
  tailConvergedRelaErr = 1e-4,
  maxIter = 100L,
  maxCore = 7L,
  paraSortInplaceMerge = FALSE,
  verbose = TRUE)
{
  Xw = Xw + 0.0
  if(minkP == "cosine") minkP = -1e308
  else if(minkP == "max") minkP = 0


  clusterWeightUB = clusterWeightUB + 0.0
  if(sum(clusterWeightUB) < ncol(X))
  {
    cat("Sum of upper bounds of cluster weights < Number of data points.\n")
    return(list());
  }


  KMconstrainedCpp(
    X = X,
    centroids = centroid,
    Xw = Xw,
    clusterWeightUpperBound = clusterWeightUB,
    minkP = minkP,
    maxCore = maxCore,
    convergenceTail = convergenceTail,
    tailConvergedRelaErr = tailConvergedRelaErr,
    maxIter = maxIter,
    paraSortInplaceMerge = paraSortInplaceMerge,
    verbose = verbose)
}




KMsparse <- function(
  X,
  d,
  centroid,
  Xw = rep(1, length(X)),
  minkP = 2,
  maxIter = 100L,
  maxCore = 7L,
  verbose = TRUE
)
{
  Xw = Xw + 0.0
  if(minkP == "cosine") minkP = -1e308
  else if(minkP == "max") minkP = 0


  sparseKMcpp(
    X = X,
    d = d,
    centroid = centroid,
    Xw = Xw,
    minkP = minkP,
    maxCore = maxCore,
    maxIter = maxIter,
    verbose = verbose)
}


KMconstrainedSparse <- function(
  X,
  d,
  centroid,
  Xw = rep(1, length(X)),
  clusterWeightUB = rep(length(X) + 1, length(centroid)),
  minkP = 2,
  convergenceTail = 5L,
  tailConvergedRelaErr = 1e-4,
  maxIter = 100L,
  maxCore = 7L,
  paraSortInplaceMerge = FALSE,
  verbose = TRUE
)
{
  Xw = Xw + 0.0
  if(minkP == "cosine") minkP = -1e308
  else if(minkP == "max") minkP = 0


  clusterWeightUB = clusterWeightUB + 0.0
  if(sum(clusterWeightUB) < length(X))
  {
    cat("Sum of upper bounds of cluster weights < Number of data points.\n")
    return(list());
  }


  sparseKMconstrainedCpp(
    X = X,
    d = d,
    centroid = centroid,
    Xw = Xw,
    clusterWeightUpperBound = clusterWeightUB,
    minkP = minkP,
    maxCore = maxCore,
    convergenceTail = convergenceTail,
    tailConvergedRelaErr = tailConvergedRelaErr,
    maxIter = maxIter,
    paraSortInplaceMerge = paraSortInplaceMerge,
    verbose = verbose
  )
}


































