densityPlot.regAbcrf <- 
function(object, obs, training, add=TRUE, main="Posterior density", log="", xlim=NULL, ylim=NULL,
         xlab=NULL, ylab=NULL, paral=FALSE, ncores= if(paral) max(detectCores()-1,1) else 1, ...)
{
    ### Checking arguments
    if (!inherits(object, "regAbcrf")) 
      stop("object not of class regAbcrf")
  
    if (!inherits(training, "data.frame"))
      stop("training needs to be a data.frame object")
  
    if (!inherits(obs, "data.frame")) 
      stop("obs needs to be a data.frame object")
    if (nrow(obs) == 0L || is.null(nrow(obs)))
      stop("no data in obs")
    if (nrow(training) == 0L || is.null(nrow(training)))
      stop("no simulation in the training reference table (response, sumstat)")
    if ( (!is.logical(add)) || (length(add) != 1L) )
      stop("add should be TRUE or FALSE")
    if ( (!is.logical(paral)) || (length(paral) != 1L) )
      stop("paral should be TRUE or FALSE")
    if(is.na(ncores)){
      warning("Unable to automatically detect the number of CPU cores, \n1 CPU core will be used or please specify ncores.")
      ncores <- 1
    }

    if( !is.character(log) )
      stop("log needs to be a character string")
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
    
    # resp and sumsta recover
  
    mf <- match.call(expand.dots=FALSE)
    mf <- mf[1]
    mf$formula <- object$formula


    mf$data <- training

    
    training <- mf$data
    
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame() )
    mt <- attr(mf, "terms")
    resp <- model.response(mf)
    
    obj <- object$model.rf
    inbag <- matrix(unlist(obj$inbag.counts, use.names=FALSE), ncol=obj$num.trees, byrow=FALSE)
    
    obj[["origNodes"]] <- predict(obj, training, predict.all=TRUE, num.threads=ncores)$predictions
    obj[["origObs"]] <- model.response(mf)
    
    #####################

    origObs <- obj$origObs
    origNodes <- obj$origNodes
    
    nodes <- predict(obj, x, predict.all=TRUE, num.threads=ncores )$predictions
    if(is.null(dim(nodes))) nodes <- matrix(nodes, nrow=1)
    ntree <- obj$num.trees
    nobs <- object$model.rf$num.samples
    nnew <- nrow(x)

    weights <- findweights(origNodes, nodes, inbag, as.integer(nobs), as.integer(nnew), as.integer(ntree)) # cpp function call
    weights.std <- weights/ntree
    
    priorDensity <- density(resp)
    
    if(add){
      
      rangex <- range(priorDensity$x)
      rangey <- range(priorDensity$y)
      
      for(i in 1:nnew){
        
        postDensity <- density(resp, weights=weights.std[,i], ...)
        rangex <- range(rangex, postDensity$x)
        rangey <- range(rangey, postDensity$y)
        
      }
      
      plot(priorDensity$x, priorDensity$y, type="l", main=main, log=log,
           xlim=if(is.null(xlim)) rangex else xlim,
           ylim=if(is.null(ylim)) rangey else ylim,
           xlab=xlab, ylab=ylab, col="grey")
      
      for(i in 1:nnew){
        postDensity <- density(resp, weights=weights.std[,i], ...)
        points(postDensity$x, postDensity$y, type="l")
      }
      
    } else {
      
      for(i in 1:nnew){
        
        postDensity <- density(resp, weights=weights.std[,i], ...)
        
        plot(postDensity$x, postDensity$y, type="l", main=main, log=log,
             xlim= if(is.null(xlim)) range(postDensity$x, priorDensity$x) else xlim,
             ylim= if(is.null(ylim)) range(postDensity$y, priorDensity$y) else ylim,
             xlab=xlab, ylab=ylab)
        
        points(priorDensity$x, priorDensity$y, type="l", col="grey")
      
        if(nnew>1 && i<nnew) readline("Press <ENTER> to Continue")
      }
      
    }

}

densityPlot <-
  function(...) UseMethod("densityPlot")