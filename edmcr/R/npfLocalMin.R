npfLocalMin <- function(PVector,A,H){
  
  P <- matrix(PVector, nrow=nrow(A),byrow=FALSE)
  fSol <- npffAH(A,H,P)
  
  return(fSol)
}