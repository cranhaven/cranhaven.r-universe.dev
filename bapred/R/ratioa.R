ratioa <-
function(x, batch) {

  if(any(is.na(x)))
	stop("Data contains missing values.")
  if(!is.factor(batch))
    stop("'batch' has to be of class 'factor'.")  
  if(!is.matrix(x))
    stop("'x' has to be of class 'matrix'.")

  batches = levels(batch)
  nbatches = length(batches)

  means = as.list(rep(0,nbatches))
  xadj = x  
  for (i in 1:nbatches) {
    means[[i]] <- colMeans(x[batch==batches[i],])
	meanabovezero <- which(means[[i]]!=0)
    xadj[batch==batches[i],meanabovezero] = scale(x[batch==batches[i],meanabovezero],center=rep(0,ncol(x[,meanabovezero])),scale=means[[i]][meanabovezero])
  }
  
  params <- list(xadj=xadj)
  params$nbatches <- nbatches
  params$batch <- batch  
  
  class(params) <- "ratioa"
   
  return(params)
    
}
