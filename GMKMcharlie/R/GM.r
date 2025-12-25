



GM <- function(
  X,
  Xw = rep(1.0, ncol(X)),
  alpha = numeric(0),
  mu = matrix(ncol = 0, nrow = 0),
  sigma = matrix(ncol = 0, nrow = 0),
  G = 5L,
  convergenceEPS = 1e-5,
  convergenceTail = 10,
  alphaEPS = 0,
  eigenRatioLim = Inf,
  embedNoise = 1e-6,
  maxIter = 1000L,
  maxCore = 7L,
  tlimit = 3600,
  verbose = TRUE,
  updateAlpha = TRUE,
  updateMean = TRUE,
  updateSigma = TRUE,
  checkInitialization = FALSE,
  KmeansFirst = TRUE,
  KmeansPPfirst = FALSE,
  KmeansRandomSeed = NULL,
  friendlyOutput = TRUE
  )
{


  if(KmeansFirst & length(alpha) <= 0)
  {
    if(!is.null(KmeansRandomSeed)) set.seed(KmeansRandomSeed)
    N = ncol(X)
    if(!KmeansPPfirst) centers = X[, sample(N, G)]
    else
    {
      iniInd = KMppIni(
        X,
        G,
        firstSelection = sample(N, 1),
        minkP = 2,
        stochastic = TRUE,
        seed = sample(2e9L, 1),
        maxCore = maxCore,
        verbose = verbose)
      centers = X[, iniInd]
    }
    rm(.Random.seed, envir = globalenv())


    kmrst = KM(X = X, centroid = centers, Xw = Xw, maxIter = maxIter, maxCore = maxCore, verbose = verbose)
    eligibleClusterInd = which(unlist(lapply(kmrst, function(x) length(x$clusterMember))) > 1)
    kmrst = kmrst[eligibleClusterInd]


    kmrstMu = as.matrix(as.data.frame(lapply(kmrst, function(x) x$centroid)))
    weightedCov = function(x, mu, w)
    {
      N = ncol(x)
      w = w * (N / sum(w))
      x_mu = x - mu
      x_mu %*% (w * t(x_mu)) / (N - 1)
    }
    covlist = list()
    for(i in 1:length(kmrst))
    {
      x = kmrst[[i]]
      subX = X[, x$clusterMember]
      ct = x$centroid
      subXw = Xw[x$clusterMember]
      # print(str(subX))
      # print(str(ct))
      # print(str(subXw))
      covlist[[i]] = as.numeric(weightedCov(subX, ct, subXw))
    }
    # print(str(covlist))
    covlist = as.matrix(as.data.frame(covlist))


    mu = kmrstMu
    sigma = covlist
  }


  # print(str(mu))
  # print(str(sigma))
  # return(list())


  if(length(sigma) > 0 & is.list(sigma))
  {
    dm = nrow(sigma[[1]])
    sigma = matrix(unlist(sigma), nrow = dm * dm)
  }


  if(!is.finite(eigenRatioLim)) eigenRatioLim = 0;
  rst = paraGmm(
    X,
    Xw,
    G,
    alpha,
    mu,
    sigma,
    eigenRatioLim,
    convergenceEPS,
    alphaEPS,
    maxIter,
    tlimit,
    verbose,
    maxCore,
    updateAlpha,
    updateMean,
    updateSigma,
    convergenceTail,
    embedNoise,
    checkInitialization
  )
  rst$clusterMember = aggregate(list(1L : ncol(X)), list(rst$clusterMember), function(x) x, simplify = F)[[2]]


  if(friendlyOutput)
  {
    tmp = lapply(as.data.frame(rst$sigma), function(x) matrix(x, nrow = sqrt(length(x))))
    names(tmp) = NULL
    rst$sigma = tmp
  }
  rst
}




GMcw <- function(
  X,
  Xw = rep(1.0, ncol(X)),
  alpha = numeric(0),
  mu = matrix(ncol = 0, nrow = 0),
  sigma = matrix(ncol = 0, nrow = 0),
  G = 5L,
  convergenceEPS = 1e-5,
  alphaEPS = 0,
  eigenRatioLim = Inf,
  maxIter = 1000L,
  maxCore = 7L,
  tlimit = 3600,
  verbose = TRUE)
{
  if(!is.finite(eigenRatioLim)) eigenRatioLim = 0;
  rst = paraGmmCW(
    X,
    Xw,
    G,
    alpha,
    mu,
    sigma,
    eigenRatioLim,
    convergenceEPS,
    alphaEPS,
    maxIter,
    tlimit,
    verbose,
    maxCore
  )
  rst$clusterMember = aggregate(list(1L : ncol(X)), list(rst$clusterMember), function(x) x, simplify = F)[[2]]
  rst
}




GMfj <- function(
  X,
  Xw = rep(1.0, ncol(X)),
  alpha = numeric(0),
  mu = matrix(ncol = 0, nrow = 0),
  sigma = matrix(ncol = 0, nrow = 0),
  G = 5L,
  Gmin = 2L,
  convergenceEPS = 1e-5,
  alphaEPS = 0,
  eigenRatioLim = Inf,
  maxIter = 1000L,
  maxCore = 7L,
  tlimit = 3600,
  verbose = TRUE)
{
  if(!is.finite(eigenRatioLim)) eigenRatioLim = 0;
  rst = paraGmmFJ(
    X,
    Xw,
    G,
    Gmin,
    alpha,
    mu,
    sigma,
    eigenRatioLim,
    convergenceEPS,
    alphaEPS,
    maxIter,
    tlimit,
    verbose,
    maxCore)
  # print(rst$clusterMember)
  rst$clusterMember = aggregate(list(1L : ncol(X)), list(rst$clusterMember), function(x) x, simplify = F)[[2]]
  rst
}








































































