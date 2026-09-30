qunormtrain <- 
function(x) {

  params <- normalizeqntval(t(x)) 
  params$x <- t(params$x)
  names(params)[names(params)=="x"] <- "xnorm"

  class(params) <- "qunormtrain"
  
  return(params)
  
}
