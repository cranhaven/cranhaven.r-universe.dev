npfConfigInit <- function(FHat,r){
  
  TauF <- edm2gram(FHat)
  EigenDecomp <- eigen(TauF,symmetric=TRUE)
  FirstRValues <- EigenDecomp$values[1:r]
  Ur <- EigenDecomp$vectors[,1:r]
  Lambdar <- diag(sapply(FirstRValues,max,0))
  P <- Ur %*% sqrt(Lambdar)
  
  return(P)
}