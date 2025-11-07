predictOOB.regAbcrf <- function(object, training, quantiles=c(0.025,0.975),
                             paral = FALSE, ncores = if(paral) max(detectCores()-1,1) else 1,...)
{
  ### Checking arguments
  
  if (!inherits(training, "data.frame"))
    stop("training needs to be a data.frame object")
  if (nrow(training) == 0L || is.null(nrow(training)))
    stop("no simulation in the training reference table (response, sumstat)")
  if ( (!is.logical(paral)) || (length(paral) != 1L) )
    stop("paral should be TRUE or FALSE")
  if(is.na(ncores)){
    warning("Unable to automatically detect the number of CPU cores, \n1 CPU core will be used or please specify ncores.")
    ncores <- 1
  }
  if(min(quantiles)<0 | max(quantiles)>1 )
    stop("quantiles must be in [0,1]")
  
  # modindex and sumsta recovery
  
  mf <- match.call(expand.dots=FALSE)
  mf <- mf[1]
  mf$formula <- object$formula
  
  mf$data <- training
  
  training <- mf$data
  
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame() )
  mt <- attr(mf, "terms")
  
  obj <- object$model.rf
  inbag <- matrix(unlist(obj$inbag.counts, use.names=FALSE), ncol=obj$num.trees, byrow=FALSE)
  
  trainingNodeID <- predict(object$model.rf, training, predict.all=TRUE, num.threads=ncores, type = 'terminalNodes')$predictions
  trainingResp <- model.response(mf)
  
  ### prediction
  
  nobs <- object$model.rf$num.samples
  
  quantFinal <- matrix(nrow=nobs,ncol=length(quantiles))
  medianeFinal <- matrix(nrow=nobs, ncol=1)
  
  variance <- rep(NA,nobs)
  variance.cdf <- rep(NA,nobs)
  esper <- rep(NA,nobs)
  
  ntree <- obj$num.trees
  
  # Out of bag expectations
  
  predict.oob <- object$model.rf$predictions
  
  # squared residuals
  
  residus.oob.sq <- (trainingResp - predict.oob)^2
  
  ### Parallelism update:
  
  # order: 1- esper, 2-med, 3-variance, 4-variance.cdf, 5- the quantiles

  if(ncores==1){
    
    for(idxLoop in 1:nobs){
      
      quant <- matrix(nrow=1,ncol=length(quantiles))
      mediane <- matrix(nrow=1, ncol=1)
      
      result <- findweights_train(trainingNodeID = trainingNodeID, inbag = inbag,
                                  ntrain = nobs, trainIdx = idxLoop-1, ntree = ntree)
      
      weights <- matrix(result, nrow = nobs) #weights are already standardised
      
      esper[idxLoop] <- weights[,1] %*% trainingResp
      
      # variance estimation
      
      variance[idxLoop] <- weights[,1] %*% residus.oob.sq
      
      
      ## Variance obtained using cdf
      
      variance.cdf[idxLoop] <- weights[,1] %*% ( ( trainingResp - predict.oob[idxLoop] )^2 )
      
      # Quantiles calculation
      
      ord <- order(trainingResp)
      trainingRespModif <- trainingResp[ord]
      weights <- weights[ord,,drop=FALSE]
      cumweights <- colCumsums(weights)
      cumweights <- sweep(cumweights,2,as.numeric(cumweights[nobs,]),FUN="/")
      
      # quantiles (from Meins)
      
      for (qc in 1:length(quantiles)){
        larg <- cumweights<quantiles[qc]
        wc <- colSums(larg)+1
        ind1 <- which(wc<1.1)
        indn1 <- which(wc>1.1)
        quant[ind1,qc] <- rep(trainingRespModif[1],length(ind1))
        quantmax <- trainingRespModif[wc[indn1]]
        quantmin <- trainingRespModif[wc[indn1]-1]
        weightmax <- cumweights[cbind(wc[indn1],indn1)]
        weightmin <- cumweights[cbind(wc[indn1]-1,indn1)]
        factor <- numeric(length(indn1))
        indz <- weightmax-weightmin<10^(-10)
        factor[indz] <- 0.5
        factor[!indz] <- (quantiles[qc]-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
        quant[indn1,qc] <- quantmin + factor* (quantmax-quantmin)
      }
      
      quantFinal[idxLoop,] <- quant[1,]
      
      
      # mediane estimation
      
      larg <- cumweights< 0.5
      wc <- colSums(larg)+1
      ind1 <- which(wc<1.1)
      indn1 <- which(wc>1.1)
      mediane[ind1,1] <- rep(trainingRespModif[1],length(ind1))
      quantmax <- trainingRespModif[wc[indn1]]
      quantmin <- trainingRespModif[wc[indn1]-1]
      weightmax <- cumweights[cbind(wc[indn1],indn1)]
      weightmin <- cumweights[cbind(wc[indn1]-1,indn1)]
      factor <- numeric(length(indn1))
      indz <- weightmax-weightmin<10^(-10)
      factor[indz] <- 0.5
      factor[!indz] <- (0.5-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
      mediane[indn1,1] <- quantmin + factor* (quantmax-quantmin)
      
      medianeFinal[idxLoop,] <- mediane 
      
    }
    
  } else {

  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
    info.store <- foreach(idxLoop=1:nobs, .combine='rbind') %dopar% {
      
      quant <- matrix(nrow=1,ncol=length(quantiles))
      mediane <- matrix(nrow=1, ncol=1)
      
      result <- findweights_train(trainingNodeID = trainingNodeID, inbag = inbag,
                                  ntrain = nobs, trainIdx = idxLoop-1, ntree = ntree)
      
      weights <- matrix(result, nrow = nobs) #weights are already standardised
      
      esperToReturn <- weights[,1] %*% trainingResp
      
      # variance estimation
      
      varianceToReturn <- weights[,1] %*% residus.oob.sq
      
      ## Variance obtained using cdf
      
      variance.cdfToReturn <- weights[,1] %*% ( ( trainingResp - predict.oob[idxLoop] )^2 )
      
      # Quantiles calculation
      
      ord <- order(trainingResp)
      trainingRespModif <- trainingResp[ord]
      weights <- weights[ord,,drop=FALSE]
      cumweights <- colCumsums(weights)
      cumweights <- sweep(cumweights,2,as.numeric(cumweights[nobs,]),FUN="/")
      
      # quantiles (from Meins)
      
      for (qc in 1:length(quantiles)){
        larg <- cumweights<quantiles[qc]
        wc <- colSums(larg)+1
        ind1 <- which(wc<1.1)
        indn1 <- which(wc>1.1)
        quant[ind1,qc] <- rep(trainingRespModif[1],length(ind1))
        quantmax <- trainingRespModif[wc[indn1]]
        quantmin <- trainingRespModif[wc[indn1]-1]
        weightmax <- cumweights[cbind(wc[indn1],indn1)]
        weightmin <- cumweights[cbind(wc[indn1]-1,indn1)]
        factor <- numeric(length(indn1))
        indz <- weightmax-weightmin<10^(-10)
        factor[indz] <- 0.5
        factor[!indz] <- (quantiles[qc]-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
        quant[indn1,qc] <- quantmin + factor* (quantmax-quantmin)
      }
      
      quantToReturn <- quant[1,]
      
      # mediane estimation
      
      larg <- cumweights< 0.5
      wc <- colSums(larg)+1
      ind1 <- which(wc<1.1)
      indn1 <- which(wc>1.1)
      mediane[ind1,1] <- rep(trainingRespModif[1],length(ind1))
      quantmax <- trainingRespModif[wc[indn1]]
      quantmin <- trainingRespModif[wc[indn1]-1]
      weightmax <- cumweights[cbind(wc[indn1],indn1)]
      weightmin <- cumweights[cbind(wc[indn1]-1,indn1)]
      factor <- numeric(length(indn1))
      indz <- weightmax-weightmin<10^(-10)
      factor[indz] <- 0.5
      factor[!indz] <- (0.5-weightmin[!indz])/(weightmax[!indz]-weightmin[!indz])
      mediane[indn1,1] <- quantmin + factor* (quantmax-quantmin)
      
      medianeToReturn <- mediane 
      
      return(c(esperToReturn, medianeToReturn, varianceToReturn, variance.cdfToReturn, quantToReturn))
      
    }
      
    stopCluster(cl)
    
    esper <- info.store[,1,drop=FALSE]
    medianeFinal <- info.store[,2,drop=FALSE]
    variance <- info.store[,3,drop=FALSE]
    variance.cdf <- info.store[,4,drop=FALSE]
    quantFinal <- info.store[,-c(1:4),drop=FALSE]
  
  }
  
  colnames(quantFinal) <- paste("quantile=",quantiles)
  
  MSE <- mean( (trainingResp - esper)^2)
  NMAE <- mean( abs((trainingResp - esper)/trainingResp) )
  
  MSE.med <- mean( (trainingResp - medianeFinal)^2)
  NMAE.med <- mean( abs((trainingResp - medianeFinal)/trainingResp) )
  
  coverage <- NULL
  if(length(quantiles)==2 & length(unique(quantiles))!=1){
    if(quantiles[1] < quantiles[2]){
      coverage <- mean( (quantFinal[,1] <= trainingResp) & (trainingResp <= quantFinal[,2]) )
    } else{
      coverage <- mean( (quantFinal[,2] <= trainingResp) & (trainingResp <= quantFinal[,1]) )
    }
  }

  tmp <- list(expectation = esper, med = medianeFinal, variance = variance, variance.cdf = variance.cdf, quantiles = quantFinal,
              MSE = MSE, NMAE = NMAE, MSE.med = MSE.med, NMAE.med = NMAE.med, coverage = coverage)
  
  class(tmp) <- "regAbcrfOOBpredict"
  tmp
}

predictOOB <-
  function(...) UseMethod("predictOOB")

print.regAbcrfOOBpredict <-
  function(x, ...){
    cat("\nOut-of-bag mean squared error computed with mean: ", x$MSE, "\n")
    cat("Out-of-bag normalized mean absolute error computed with mean: ", x$NMAE, "\n")
    cat("\nOut-of-bag mean squared error computed with median: ", x$MSE.med, "\n")
    cat("Out-of-bag normalized mean absolute error computed with median: ", x$NMAE.med, "\n")
    if(!is.null(x$coverage)) cat("\nOut-of-bag credible interval coverage: ", x$coverage, "\n")
  }

as.list.regAbcrfOOBpredict <-
  function(x, ...){
    list(expectation = x$expectation, med = x$med, variance = x$variance, variance.cdf = x$variance.cdf, quantiles=x$quantiles,
        MSE = x$MSE, NMAE = x$NMAE, MSE.med=x$MSE.med, NMAE.med=x$NMAE.med, coverage = x$coverage, ...)
  }