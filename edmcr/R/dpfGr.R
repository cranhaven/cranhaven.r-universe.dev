##Gradient Function for minimization of DPF algorithm
dpfGr <- function(CurrentSolution,KnownEntries=KnownEntries,Index=Index,NRows=NRows,Dimension=Dimension){
  
  p <- Dimension
    
  Rows <- Index %% NRows
  Columns <- ceiling(Index/NRows)
  
  Rows[which(Rows == 0)] <- NRows
  
  Row <- Rows[order(Columns)]
  Col <- sort(Columns)
  
  TMatrix <- buildMatrix(CurrentSolution,KnownEntries,Index,NRows)
  TauTMatrix <- edm2gram(TMatrix)
  Eigs <- eigen(TauTMatrix,symmetric=TRUE)
  LambdaHat <- sapply(Eigs$values,max,0)
  EigVec <- Eigs$vectors

  BT1 <- sapply(c(1:p),function(i,LH,EV){EV[,i]*LH[i]},LH=LambdaHat,EV=EigVec)
  BT <- BT1 %*% t(EigVec)[1:p,]
  
  S <- BT - TauTMatrix
  n <- nrow(S)
  
  SumRows <- apply(S,1,sum)
  SumS <- sum(SumRows)
  
  Grad <- sapply(c(1:length(Row)),dpfGrHelper,S=S,SumS=SumS,SumRows=SumRows,Row=Row,Col=Col,n=n)
  
  return(Grad)
  
}

##Gradient Helper
##Computes individual inputs for the gradient function
#used in dpfGr.

dpfGrHelper <- function(Ind,S,SumS,SumRows,Row,Col,n=n){
  Result <- (2/(n^2))*SumS - (2/n)*(SumRows[Row[Ind]] + SumRows[Col[Ind]]) + 2*S[Row[Ind],Col[Ind]]
  return(Result)
}
