covRegAbcrf.regAbcrf <-
function(regForest1, regForest2, obs, training1, training2, 
         ntree=500, mtry=max(floor((dim(training1)[2]-1)/3), 1),
         sampsize=min(1e5, dim(training1)[1]), paral = FALSE, 
         ncores = if(paral) max(detectCores()-1,1) else 1, paral.predict = FALSE,
         ncores.predict = if(paral.predict) max(detectCores()-1,1) else 1, ... ){
  
  nSumstat1 <- dim(training1)[2]-1
  nSumstat2 <- dim(training2)[2]-1
  
  if (!inherits(obs, "data.frame")) 
    stop("obs needs to be a data.frame object")
  if (!inherits(regForest1, "regAbcrf")) 
    stop("regForest1 not of class regAbcrf")
  if (!inherits(regForest2, "regAbcrf")) 
    stop("regForest2 not of class regAbcrf")
  if (!inherits(training1, "data.frame"))
    stop("training1 needs to be a data.frame object")
  if (!inherits(training2, "data.frame"))
    stop("training2 needs to be a data.frame object")
  if(any(regForest1$model.rf$forest$independent.variable.names[1:nSumstat1] != regForest1$model.rf$forest$independent.variable.names[1:nSumstat2]) )
    stop("variable names of the regAbcrf objects do not match")

  if(sampsize > nrow(training1) )
    stop("sampsize too large")
  
  if ( (!is.logical(paral)) || (length(paral) != 1L) )
    stop("paral should be TRUE or FALSE")
  if ( (!is.logical(paral.predict)) || (length(paral.predict) != 1L) )
    stop("paral.predict should be TRUE or FALSE")
  if(is.na(ncores)){
    warning("Unable to automatically detect the number of CPU cores, \n1 CPU core will be used or please specify ncores.")
    ncores <- 1
  }
  if(is.na(ncores.predict)){
    warning("Unable to automatically detect the number of CPU cores, \n1 CPU core will be used or please specify ncores.predict.")
    ncores.predict <- 1
  }
  
  # recover response
  
  mf1 <- match.call(expand.dots=FALSE)
  mf1 <- mf1[1]
  mf1$formula <- regForest1$formula


  mf1$data <- training1
  

  mf1[[1L]] <- as.name("model.frame")
  mf1 <- eval(mf1, parent.frame() )
  mt1 <- attr(mf1, "terms")
  resp1 <- model.response(mf1)
  
  sumsta1 <- training1[regForest1$model.rf$forest$independent.variable.names[1:nSumstat1]]
  
  mf2 <- match.call(expand.dots=FALSE)
  mf2 <- mf2[1]


  mf2$data <- training2
  
  
  mf2$data <- training2
  mf2[[1L]] <- as.name("model.frame")
  mf2 <- eval(mf2, parent.frame() )
  mt2 <- attr(mf2, "terms")
  resp2 <- model.response(mf2)
  
  sumsta2 <- training2[regForest2$model.rf$forest$independent.variable.names[1:nSumstat2]]
   
  if(any(dim(sumsta1) != dim(sumsta2) ))
    stop("regForest1 and regForest2 training data are not build on the same summaries")
  if(any(sumsta1 != sumsta2) )
    stop("regForest1 and regForest2 training data are not build on the same summaries")
  
  # residuals
  
  res1 <- resp1 - regForest1$model.rf$predictions
  res2 <- resp2 - regForest2$model.rf$predictions
  
  res12 <- res1*res2 # new response variable
  
  # construction du dataframe pour ranger
  
  data.ranger <- data.frame(res12, sumsta1)
  
  # forest construction

  m <- names(match.call(expand.dots=TRUE))
  
  model.rf <- ranger(res12~., data=data.ranger, num.trees = ntree, sample.fraction=sampsize/length(res12),
                           num.threads = ncores, mtry=mtry, ...)
  
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
  
  if(!is.null(x)){
    if (any(colnames(x) != regForest1$model.rf$forest$independent.variable.names[1:nSumstat1]))
      stop("names of predictor variables do not match")
  }
  
  result <- predict(model.rf, x, num.threads=ncores.predict)$predictions
  
  return(result)
  
}


covRegAbcrf <-
function(...) UseMethod("covRegAbcrf")
