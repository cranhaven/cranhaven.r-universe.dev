ranking <- function(fit, full, p = 2, minCoef = 1, maxCoef = 10, quantiles = c(0, 0.25, 0.5, 0.75, 1), transform = function(x){x}) {
  
  # rankMethod   <- match.arg(rankMethod)
  # rankFun      <- match.fun(rankMethod)
  
  if (inherits(fit, "WpProj")) {
    coarse     <- fit$eta
    nzero      <- fit$nzero
    whichModel <- which(nzero >= minCoef & nzero <= maxCoef)
    keep_nzero <- nzero[whichModel]
    if (is.null(full)) full <- transform(fit$Y)
    
    # losses     <- sapply(coarse[whichModel], function(x) WpDist_individual(x, full, p, "rowwise"))
    # rankLoss   <- apply(losses, 1, rankFun )
    rankLoss   <- sapply(coarse[whichModel],
                         function(x)
                           sapply(1:nrow(x), function(y) WpProj::wasserstein(transform(x[y,]), 
                                                                   full[y,], p, p, "colwise", 
                                                        "univariate.approximation.pwr")))
  } else{
    if(is.list(fit)) stop("fit must be a WpProj output")
    # if(!is.list(fit)) stop("fit must be a sparse-posterior output or a list of outputs from the SparsePosterior package")
    # if(!(all(sapply(fit, inherits, "sparse-posterior")))) {
    #   stop("Must be a fitted model from the SparsePosterior package")
    # }
    # coarse     <- lapply(fit, function(ff) ff$eta)
    # nzero      <- lapply(fit, function(ff) ff$nzero)
    # whichModel <- lapply(nzero, function(nn) which(nn <= maxCoef))
    #
    # losses     <- mapply(function (cc,nn){
    #       return(sapply(cc[nn], function(x)
    #         WpDist_individual(transform(x), transform(full), p, "rowwise")))
    #       }, cc = coarse, nn = whichModel)
    #
    # # rankLosses <- sapply(losses, function(ll) apply(ll, 1, rankFun ))
    # # rankLoss   <- rowMeans(rankLosses)
    # extractRows<- function(losses, idx) {
    #   return(unlist(sapply(losses, function(ll) ll[idx,])))
    # }
    # keep_nzero <- lapply(nzero, function(nn) nn[which(nn<=maxCoef)])
    # rankLoss   <- t(sapply(1:n, function(i) tapply(extractRows(losses, i), unlist(keep_nzero), mean)))
    # rankLoss   <- rankLoss[,ncol(rankLoss)]
    
  }
  if(!is.matrix(rankLoss)) rankLoss <- matrix(rankLoss)
  
  rankLoss     <- rankLoss[,ncol(rankLoss)]
  ranks        <- rank(rankLoss, ties.method= "random")
  desiredRank  <- ceiling(quantiles * nrow(full))
  desiredRank  <- ifelse(desiredRank == 0, 1,  desiredRank)
  idx          <- which(ranks %in% desiredRank)
  idx          <- idx[order(rankLoss[idx])]
  
  return(list(index = idx, ncoef = max(unlist(keep_nzero)) ))
}
