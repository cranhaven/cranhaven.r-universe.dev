##
npfCreateF <- function(A, GraphA, SP){
    
  Index <- which(A == 0)
  
  A[Index] <- SP[Index]^2
  
  
  #MakeUnif <- function(i,SP){
  #  out <- runif(1,0,SP[i])^2
  #  return(out)
  #}
  
  #Sim <- sapply(Index,MakeUnif,SP=SP)
  #A[Index] <- Sim
  
  diag(A) <- 0
  
  return(A)
}

##
npfCreateFHat <- function(A, GraphA, SP){
  
  FTilde <- A
  
  IndexI <- c(2:nrow(A))
  
  F1 <- function(Indexi){
    IndexJ <- which(A[Indexi,1:(Indexi-1)] == 0)
    return(IndexJ)
  }
  
  IndexJList <- lapply(IndexI,F1)
  
  GetS <- function(Ind,IndexI,IndexJList,GraphA){
    i <- IndexI[Ind]
    j <- IndexJList[[Ind]]
    
    Result <- sapply(j,function(j,i,GraphA){rtruncnorm(1,a=1,b=length(get.shortest.paths(GraphA, from=i,to=j)$vpath[[1]])-1,1.5,3)}, i=i,GraphA=GraphA)
    return(Result)
  }
  
  s <- unlist(lapply(c(1:length(IndexI)), GetS, IndexI,IndexJList,GraphA))
  
  LengthIndI <- sapply(c(1:length(IndexI)), function(Ind,IndexJList){length(IndexJList[[Ind]])}, IndexJList)
  IndexIVec <- rep(IndexI, LengthIndI)
  IndexJVec <- unlist(IndexJList)
  
  FTilde[IndexIVec,IndexJVec] <- ((SP[IndexIVec,IndexJVec])^2)/s
  FTilde[IndexJVec,IndexIVec] <- FTilde[IndexIVec,IndexJVec]
  diag(FTilde) <- 0
  
  return(FTilde)
}
