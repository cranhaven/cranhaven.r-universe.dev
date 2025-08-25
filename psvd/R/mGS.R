
#############################################################################
## Modified Gram-Schmidt orthogonalization method
mGS <- function(A) {
   n <- ncol(A)
   m <- nrow(A)
   if (n>m) {
      V <- A[,1:m]; n <- m
   } else {
      V <- A
   }
   for (i in 1:n) {
       ri <- as.numeric(sqrt(crossprod(V[,i])))
       V[,i] <- V[,i]/ri
       if (i<n) {
          for (j in (i+1):n) {
              rj <- as.numeric(crossprod(V[,i],V[,j]))
              V[,j] <- V[,j] - rj*V[,i]
          }
       }
   }
   return(V)
}
#############################################################################
