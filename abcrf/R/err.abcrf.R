err.abcrf <- function(object, training, paral=FALSE, ncores= if(paral) max(detectCores()-1,1) else 1)
{
  if (!inherits(training, "data.frame"))
    stop("training needs to be a data.frame object")
  if ( (!is.logical(paral)) && (length(paral) != 1L) )
    stop("paral should be TRUE or FALSE")
  if(is.na(ncores)){
    warning("Unable to automatically detect the number of CPU cores, \n1 CPU core will be used or please specify ncores.")
    ncores <- 1
  }

  nmod <- length(object$model.rf$forest$levels)
  ntrain <- nrow(training)
  
  if (length(object$group)!=0)
  {
    ngroup <- length(object$group)
    varn <- object$formula[[2]]
    training[[as.character(varn)]] <- as.vector(training[[as.character(varn)]])
    allmod <- unique(training[[as.character(varn)]])
    for (k in 1:ngroup) for (l in 1:length(object$group[[k]])) 
      training[[as.character(varn)]][which(training[[as.character(varn)]]==object$group[[k]][l])] <- paste("g",k,sep="")
    if (!setequal(allmod,unlist(object$group)))
    {
      diffe <- setdiff(allmod,unlist(object$group))
      for (l in 1:length(diffe)) training <- training[-which(training[[as.character(varn)]]==diffe[l]),]
    }
    training[[as.character(varn)]] <- as.factor(training[[as.character(varn)]])
  }
  
  mf <- match.call(expand.dots=FALSE)
  mf <- mf[1]
  mf$formula <- object$formula
  mf$data <- training
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame() )
  mt <- attr(mf, "terms")
  modindex <- model.response(mf)
  
  if (object$lda) training <- cbind(training,predict(object$model.lda,training)$x)
  
  inbag <- matrix(unlist(object$model.rf$inbag.counts, use.names=FALSE), ncol=object$model.rf$num.trees, byrow=FALSE)
  
  mimi <- predict(object$model.rf, training, predict.all=TRUE, num.threads=ncores)$predictions
  if (object$model.rf$num.trees < 40) stop("the number of trees in the forest should be greater than 10")
  sequo <- seq(40,object$model.rf$num.trees, length.out = 20)

  res <- oobErrors(sequo = as.integer(floor(sequo)), ntrain = as.integer(ntrain), mod = as.integer(labels(modindex)),
                   ntree = object$model.rf$forest$num.trees, modindex = as.numeric(modindex), inbag = inbag, mimi = mimi)
  
  plot(floor(sequo),res,ylab="Prior error rate",xlab="Number of trees",type="l", ylim=range(res))
  cbind(ntree=floor(sequo), error.rate=res)
}