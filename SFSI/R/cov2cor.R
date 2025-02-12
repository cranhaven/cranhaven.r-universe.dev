
# Covariance matrix to correlation matrix

cov2cor2 <- function(A, a = 1, inplace = FALSE)
{
   dm <- dim(A)
   if((sum(dm)/2)^2 != length(A)){
      stop("Input 'A' must be a squared symmetric matrix")
   }
   n <- dm[1]

   #isBigMatrix <- bigmemory::is.big.matrix(A)
   isBigMatrix <- FALSE
   if(isBigMatrix){
     message(" Routine 'cov2cor' is not implemented yet for 'bigmatrix'")
     #stopifnot(bigmemory::typeof(A) == "double")

   }else{
     #dyn.load("c_cov2cor.so")
     if(inplace){
       tmp <- .Call('R_cov2cor', n, a, A)
       #return(tmp)
     }else{
       out <- A[]
       tmp <- .Call('R_cov2cor', n, a, out)
       return(out)
     }
     #dyn.unload("c_cov2cor.so")
   }
}
