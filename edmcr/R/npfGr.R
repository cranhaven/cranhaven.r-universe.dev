#Compute Gradient

npfGr <- function(PVector,A,H){
  
  P <- matrix(PVector, nrow=nrow(A),byrow=FALSE)
  
  n <- nrow(P)
  r <- ncol(P)
  
  Gradient <- c()
  
  for(i in 1:n){
    Ind <- rep(0,r)
    for(j in 1:n){
      if(i == j){
       Ind <- Ind + 0 
      }else{  
      Ind <- Ind + 2*(H[i,j]^2)*(2*(A[i,j] - sum((P[i,]-P[j,])^2))*(-2*(P[i,]-P[j,])))
      }
    }
   Gradient <- rbind(Gradient, Ind)
  }
  
  return(as.vector(Gradient))
}

##gradient wrapper for optimization
npfGrWrap <- function(P){
  A <- get("A")
  H <- get("H")
  
  Gradient <- npfGr(P,A,H)
  return(Gradient)
  
}