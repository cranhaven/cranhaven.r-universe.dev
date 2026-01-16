distanceMatrix <- function(X, distance,...){
  rows <- dim(X)[1]
  cols <- dim(X)[2]
  Ds <- matrix(NA, nrow = rows, ncol = rows)
  for(i in 1:(rows-1)){
    for(j in (i+1):rows){
      Ds[j,i] <- distance(X[i,],X[j,],...)
    }
  }
  Ds <- as.dist(Ds)
  return(Ds)
}

calculateDistance <- function(X, type,...){

  distances_TSdist <- c("infnorm", "ccor", "sts", "lb.keogh", "edr", "erp", "lcss", "fourier", "tquest", "dissim", "acf",
                        "pacf", "ar.lpc.ceps", "ar.mah", "ar.mah.statistic", "ar.mah.pvalue", "ar.pic", "cdm", "cid", "cor",
                        "cort", "int.per", "per", "mindist.sax", "ncd", "pred",  "spec.glk", "spec.isd", "spec.llr", "pdc",
                        "frechet", "tam")

  distances_parallelDist <- c("bhjattacharyya", "bray", "canberra", "chord", "divergence", "dtw","euclidean", "fJaccard",
                              "geodesic", "hellinger", "kullback", "mahalanobis", "manhattan", "maximum", "minkowski",
                              "podani", "soergel", "wave", "whittaker") # DISTANCES WITH BINARY INPUT VARIABLES NOT USED

  if(type==toupper(type)) type <- tolower(type) # if type in uppercase --> lowercase

  if(type %in% distances_parallelDist) dist_type <- 'parDist'
  else if(type %in% distances_TSdist) dist_type <- 'TSdist'
  else if(exists(type)){
    if(is.function(eval(parse(text=type)))) dist_type <- 'r_custom'
    else dist_type <- 'parDist_custom'
  }
  else stop('Invalid distance type. See ?dbcsp for more information.')

  Ds <- switch(
    dist_type,
    'TSdist' = Ds <- plyr::llply(X, TSdist::TSDatabaseDistances, distance=type,...),
    'parDist' = Ds <- plyr::llply(X, parallelDist::parDist, method=type,...),
    'parDist_custom' = Ds <- plyr::llply(X, parallelDist::parDist, method="custom", func=eval(parse(text=type)),...),
    'r_custom' = Ds <- plyr::llply(X, distanceMatrix, distance=eval(parse(text=type)),...)
  )
  return(Ds)
}

compB <- function(X, mixture, type, w, eig.tol = 1e-06, getWarning=TRUE,...)
{
  is_warning = FALSE
  # Put together all de Bi-s
  #
  # EUCL
  if(type=='EUCL' || type=='euclidean') Bs <- plyr::llply(X, function(x){x%*%t(x)})
  else # DB and MIX
  {
    if(is.list(X)) n <- length(X)
    else n <- 1
    # DB
    Ds <- calculateDistance(X,type,...)
    # MIX
    if(mixture){
      Ds1 <- calculateDistance(X,'euclidean')
      Ds2 <- Ds
      Ds <- vector("list", n)
      for (r in 1:n)
      {
        aux <- list(Ds1[[r]], Ds2[[r]])
        Ds[[r]] <- Mixture.dist(aux, w=w)
      }
    }
    Bs <- vector("list", n)
    for (r in 1:n) Bs[[r]] <- Bd(D=Ds[[r]], X=X[[r]])
  }

  # pooled B matrix
  Bs <- plyr::laply(Bs, as.matrix)
  B <- plyr::aaply(Bs, c(2, 3), mean)

  # DB and MIX
  if(type!='EUCL'){
    # Check whether it is a positive definite matrix
    vp <- eigen(B)$values
    if (min(vp) < -eig.tol)
    {
      B <- Matrix::nearPD(B)$mat
      B <- as.matrix(B)
      if(getWarning){
        is_warning = TRUE
        #warning('Distance matrix was converted to be definite positive',immediate. = TRUE)
      }
    }
  }

  return(list(B=B,warn=is_warning))
}
