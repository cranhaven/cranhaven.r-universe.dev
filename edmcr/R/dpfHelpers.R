#Objective function for minimization in Trosset Algorithm
dpfFp <- function(TauD,p){ 
  
  Eigs <- eigen(TauD,symmetric=TRUE,only.values=TRUE)$values
  SortedEigs <- sort(Eigs,decreasing=TRUE)
  
  n <- length(Eigs)
  Fn <- function(Eig){
    return((Eig - max(Eig,0))^2)
  }
  
  R1 <- sum(sapply(SortedEigs[1:p], Fn))
  R2 <- sum((SortedEigs[(p+1):n])^2)
  R <- R1 + R2
  
  return(R)
}

#Helper function for lbfgs minimization
#Compute minimum function value given current iteration of UnknownEntries
dpfFn <- function(UnknownEntries, Index = Index, KnownEntries = KnownEntries, NRows=NRows,Dimension=Dimension){
  
  PMatrix <- buildMatrix(UnknownEntries,KnownEntries,Index,NRows)
  
  TauMatrix <- edm2gram(PMatrix)
  FpMatrix <- dpfFp(TauMatrix,Dimension)
  
  return(FpMatrix)
  
}