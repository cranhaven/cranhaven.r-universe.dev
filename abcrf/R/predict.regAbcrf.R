predict.regAbcrf <- function(object, obs, training, quantiles=c(0.025,0.975),
                             paral = FALSE, ncores = if(paral) max(detectCores()-1,1) else 1, rf.weights = FALSE,
                             post.err.med = FALSE, ...)
{
  ### Checking arguments

  if (!inherits(obs, "data.frame"))
    stop("obs needs to be a data.frame object")
  if (!inherits(training, "data.frame"))
    stop("training needs to be a data.frame object")
  if (nrow(training) == 0L || is.null(nrow(training)))
    stop("no simulation in the training reference table (response, sumstat)")
  if ( (!is.logical(paral)) || (length(paral) != 1L) )
    stop("paral should be TRUE or FALSE")
  if ( (!is.logical(rf.weights)) || (length(rf.weights) != 1L) )
    stop("paral should be TRUE or FALSE")
  if(is.na(ncores)){
    warning("Unable to automatically detect the number of CPU cores, \n1 CPU core will be used or please specify ncores.")
    ncores <- 1
  }
  
  if(min(quantiles)<0 | max(quantiles)>1 )
    stop("quantiles must be in [0,1]")
  
  if ( (!is.logical(post.err.med)) || (length(post.err.med) != 1L) )
    stop("post.err.med should be TRUE or FALSE")
  
    # modindex and sumsta recovery
  
  mf <- match.call(expand.dots=FALSE)
  mf <- mf[1]
  mf$formula <- object$formula
  

  mf$data <- training
  
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame() )
  mt <- attr(mf, "terms")
  
  obj <- object$model.rf
  
  inbag <- simplify2array(obj$inbag.counts)
  
  obj[["origObs"]] <- model.response(mf)
  
  x <- obs
  if(!is.null(x)){
    if(is.vector(x)){
      x <- matrix(x,ncol=1)
    }
    if (nrow(x) == 0)
      stop("obs has 0 rows")
    if (any(is.na(x)))
      stop("missing values in obs")
  }

  
  ### prediction
  
  origObs <- obj$origObs

  nnew <- nrow(x)
  
  quant <- matrix(nrow=nnew,ncol=length(quantiles))
  mediane <- matrix(nrow=nnew, ncol=1)
  
  nodeIDTrain <- predict(obj, training, predict.all=TRUE, num.threads=ncores, type="terminalNodes")$predictions
  nodeIDObs <- predict(obj, x, predict.all=TRUE, num.threads=ncores, type="terminalNodes")$predictions
  
  if(is.null(dim(nodeIDObs))) nodeIDObs <- matrix(nodeIDObs, nrow=1)
  ntree <- obj$num.trees
  
  ntrain <- obj$num.samples

  weights <- findweights(nodeIDTrain, nodeIDObs, inbag, ntrain, nnew, ntree) # cpp function call

  weights.std <- weights/ntree
  
  esper <- sapply(1:nnew, function(x) weights.std[,x]%*%origObs)
  
  # Out of bag expectations
  
  predict.oob <- obj$predictions
  
  # squared residuals
  
  residus.oob.sq <- (origObs - predict.oob)^2
  
  # variance estimation
  
  variance <- sapply(1:nnew, function(x) weights.std[,x] %*% residus.oob.sq)
  
  ## Variance obtained using cdf
  
  variance.cdf <- sapply(1:nnew, function(x) weights.std[,x] %*% ( ( origObs - esper[x] )^2 ) ) 
  
  # Quantiles calculation
  
  ord <- order(origObs)
  origObs <- origObs[ord]
  weights <- weights[ord,,drop=FALSE]
  cumweights <- colCumsums(weights)
  cumweights <- sweep(cumweights,2,as.numeric(cumweights[ntrain,]),FUN="/")
  
  # quantiles (from Meins)
  
  for (qc in 1:length(quantiles)){
    larg <- cumweights<quantiles[qc]
    wc <- colSums(larg)+1
    ind1 <- which(wc<1.1)
    indn1 <- which(wc>1.1)
    quant[ind1,qc] <- rep(origObs[1],length(ind1))
    quantmax <- origObs[wc[indn1]]
    quantmin <- origObs[wc[indn1]-1]
    weightmax <- cumweights[cbind(wc[indn1],indn1)]
    weightmin <- cumweights[cbind(wc[indn1]-1,indn1)]
    factor <- numeric(length(indn1))
    indz <- weightmax-weightmin<10^(-10)
    factor[indz] <- 0.5
    factor[!indz] <- (quantiles[qc]-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
    quant[indn1,qc] <- quantmin + factor* (quantmax-quantmin)
  }
  
  colnames(quant) <- paste("quantile=",quantiles,sep = "")
  
  # mediane estimation
  
  larg <- cumweights< 0.5
  wc <- colSums(larg)+1
  ind1 <- which(wc<1.1)
  indn1 <- which(wc>1.1)
  mediane[ind1,1] <- rep(origObs[1],length(ind1))
  quantmax <- origObs[wc[indn1]]
  quantmin <- origObs[wc[indn1]-1]
  weightmax <- cumweights[cbind(wc[indn1],indn1)]
  weightmin <- cumweights[cbind(wc[indn1]-1,indn1)]
  factor <- numeric(length(indn1))
  indz <- weightmax-weightmin<10^(-10)
  factor[indz] <- 0.5
  factor[!indz] <- (0.5-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
  mediane[indn1,1] <- quantmin + factor* (quantmax-quantmin)
  
  
  #################
  
    #### Posterior error measures
    
    # Posterior normalized mean absolute error computed with the oob mean
    
    nmaeRatio.mean <- abs( (obj$origObs - predict.oob)/obj$origObs )
    
    post.NMAE.mean <- sapply(1:nnew, function(x) weights.std[,x] %*% nmaeRatio.mean)
    
    ## ! For the posterior errors using the median, we need to compute the oob median estimate (using predictOOB)
  
  
  if(post.err.med){
    
    pred.oob.training <- predictOOB(object, training, quantiles, paral, ncores)
  
    residus.oob.sq.median <- (obj$origObs - pred.oob.training$med)^2
    nmaeRatio.median <- abs( (obj$origObs - pred.oob.training$med)/obj$origObs )
    
    prior.MSE.mean  <- pred.oob.training$MSE
    
    prior.NMAE.mean <- pred.oob.training$NMAE
    
    prior.MSE.med   <- pred.oob.training$MSE.med
    
    prior.NMAE.med  <- pred.oob.training$NMAE.med
    
    prior.coverage <- NULL
    
    if(!is.null(pred.oob.training$coverage)) prior.coverage <- pred.oob.training$coverage
    
    post.MSE.med <- sapply(1:nnew, function(x) weights.std[,x] %*% residus.oob.sq.median)
    
    post.NMAE.med <- sapply(1:nnew, function(x) weights.std[,x] %*% nmaeRatio.median)
    
  }
  
  #################
  
  if(post.err.med){
    
    if(rf.weights){
      tmp <- list(expectation = esper, med = mediane, variance = variance, variance.cdf = variance.cdf, quantiles = quant, weights = weights.std,
                  post.NMAE.mean = post.NMAE.mean, post.MSE.med = post.MSE.med, post.NMAE.med = post.NMAE.med,
                  prior.NMAE.mean = prior.NMAE.mean, prior.MSE.mean = prior.MSE.mean,
                  prior.NMAE.med = prior.NMAE.med, prior.MSE.med = prior.MSE.med, prior.coverage = prior.coverage)
    } else {
      tmp <- list(expectation = esper, med = mediane, variance = variance, variance.cdf = variance.cdf, quantiles = quant,
                  post.NMAE.mean = post.NMAE.mean, post.MSE.med = post.MSE.med, post.NMAE.med = post.NMAE.med,
                  prior.NMAE.mean = prior.NMAE.mean, prior.MSE.mean = prior.MSE.mean,
                  prior.NMAE.med = prior.NMAE.med, prior.MSE.med = prior.MSE.med, prior.coverage = prior.coverage)
    }
  } else {
    
    if(rf.weights){
      tmp <- list(expectation = esper, med = mediane, variance = variance, variance.cdf = variance.cdf, quantiles = quant,
                  weights=weights.std, post.NMAE.mean = post.NMAE.mean)
    } else{
      tmp <- list(expectation = esper, med = mediane, variance = variance, variance.cdf = variance.cdf, quantiles = quant,
                  post.NMAE.mean = post.NMAE.mean)
    }
    
  }
  class(tmp) <- "regAbcrfpredict"
  tmp
  
}


print.regAbcrfpredict <-
  function(x, ...){
    ret <- cbind(x$expectation, x$med, x$variance, x$variance.cdf, x$quantiles, x$post.NMAE.mean)
    if( !is.null(x$post.MSE.med) ){
      ret <- cbind(ret, x$post.MSE.med, x$post.NMAE.med)
      colnames(ret) <- c("expectation", "median", "variance (post.MSE.mean)", "variance.cdf" , colnames(x$quantiles),
                         "post.NMAE.mean", "post.MSE.med", "post.NMAE.med")
    } else {
      colnames(ret) <- c("expectation", "median", "variance (post.MSE.mean)", "variance.cdf" , colnames(x$quantiles),
                         "post.NMAE.mean")
    }
    print(ret, ...)
    if( !is.null(x$prior.MSE.mean) ){
      cat("\nPrior out-of-bag mean squared error computed with mean: ", x$prior.MSE.mean, "\n")
      cat("Prior out-of-bag normalized mean absolute error computed with mean: ", x$prior.NMAE.mean, "\n")
      cat("\nPrior out-of-bag mean squared error computed with median: ", x$prior.MSE.med, "\n")
      cat("Prior out-of-bag normalized mean absolute error computed with median: ", x$prior.NMAE.med, "\n")
      if(!is.null(x$prior.coverage)) cat("\nPrior out-of-bag credible interval coverage: ", x$prior.coverage, "\n")
    }
    
  }

as.data.frame.regAbcrfpredict <-
  function(x, ...) {
    ret <- cbind(x$expectation, x$med, x$variance, x$variance.cdf, x$quantiles, x$post.NMAE.mean)
    if( !is.null(x$post.MSE.med) ){
      ret <- cbind(ret, x$post.MSE.med, x$post.NMAE.med)
      colnames(ret) <- c("expectation", "median", "variance (post.MSE.mean)", "variance.cdf" , colnames(x$quantiles),
                         "post.NMAE.mean", "post.MSE.med", "post.NMAE.med")
    } else {
      colnames(ret) <- c("expectation", "median", "variance (post.MSE.mean)", "variance.cdf" , colnames(x$quantiles),
                         "post.NMAE.mean")
    }
    as.data.frame(ret,  row.names=NULL, optional=FALSE, ...)
  }

as.matrix.regAbcrfpredict <-
  function(x, ...){
    ret <- cbind(x$expectation, x$med, x$variance, x$variance.cdf, x$quantiles, x$post.NMAE.mean)
    if( !is.null(x$post.MSE.med) ){
      ret <- cbind(ret, x$post.MSE.med, x$post.NMAE.med)
      colnames(ret) <- c("expectation", "median", "variance (post.MSE.mean)", "variance.cdf" , colnames(x$quantiles),
                         "post.NMAE.mean", "post.MSE.med", "post.NMAE.med")
    } else {
      colnames(ret) <- c("expectation", "median", "variance (post.MSE.mean)", "variance.cdf" , colnames(x$quantiles),
                         "post.NMAE.mean")
    }
    ret
  }

as.list.regAbcrfpredict <-
  function(x, ...){
    if(!is.null(x$post.MSE.med)){
      if(!is.null(x$weights)){
        list(expectation = x$expectation, med = x$med , variance = x$variance, variance.cdf = x$variance.cdf, quantiles=x$quantiles, weights=x$weights,
             post.NMAE.mean = x$post.NMAE.mean, post.MSE.med = x$post.MSE.med, post.NMAE.med = x$post.NMAE.med, ...)
      } else{
        list(expectation = x$expectation, med = x$med , variance = x$variance, variance.cdf = x$variance.cdf, quantiles=x$quantiles,
             post.NMAE.mean = x$post.NMAE.mean, post.MSE.med = x$post.MSE.med, post.NMAE.med = x$post.NMAE.med, ...)
      }
    } else if(!is.null(x$weights)){
      list(expectation = x$expectation, med = x$med , variance = x$variance, variance.cdf = x$variance.cdf, quantiles=x$quantiles, weights=x$weights,
           post.NMAE.mean = x$post.NMAE.mean, ...)
    } else{
      list(expectation = x$expectation, med = x$med , variance = x$variance, variance.cdf = x$variance.cdf, quantiles=x$quantiles,
           post.NMAE.mean = x$post.NMAE.mean, ...)
    }
  }