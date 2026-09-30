qunormaddon <- 
function(params, x) {

  if (!inherits(params, "qunormtrain")) 
    stop("Input parameter 'params' has to be of class 'qunormparam'.")
  xnorm <- x
  for (i in 1:nrow(x)) {
    xnorm[i,] <- normalizeqntadd2(x[i,], params$mqnts)
  }
  return(xnorm)
  
}
