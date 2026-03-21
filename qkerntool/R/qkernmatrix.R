
setGeneric("as.qkernmatrix",function(x, center = FALSE) standardGeneric("as.qkernmatrix"))
setMethod("as.qkernmatrix", signature(x = "matrix"),
function(x, center = FALSE)
{

  if(center){
    N <- dim(x)[1]
    x <- t(t(x - colSums(x)/N) -  rowSums(x)/N) + sum(x)/N^2
    ## center onditionally negative definite kernel matrix
    x <- -x
    x <- pmax(x, t(x))
    x[which(is.na(x))] <- 0
    x[which(x == Inf)] <- 0
  }
  
  return(new("qkernmatrix",.Data = x))
})

##############################################################################################
setGeneric("as.cndkernmatrix",function(x, center = FALSE) standardGeneric("as.cndkernmatrix"))
setMethod("as.cndkernmatrix", signature(x = "matrix"),
function(x, center = FALSE)
{
            
  if(center){
    N <- dim(x)[1]
    x <- t(t(x - colSums(x)/N) -  rowSums(x)/N) + sum(x)/N^2
    ## center onditionally negative definite kernel matrix
    x <- -x
    x <- pmax(x, t(x))
    x[which(is.na(x))] <- 0
    x[which(x == Inf)] <- 0
   }
            
   return(new("cndkernmatrix",.Data = x))
})



