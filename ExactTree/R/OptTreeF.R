OptTreeF <- function(Y,X,XType,measure,MaxSize,MaxDepth,MinNodeSize,MinH,
                     AlgoType,BoundH, Prior, LossM,LookAheadDepth){

  warn<-0
  MY<-NROW(Y)
  NY<-NCOL(Y)
  MX<-NROW(X)
  NX<-NCOL(X)


  if(MaxSize == 0){
    NMaxNodes = (2^MaxDepth) -1
    if(bitwAnd(AlgoType,4)>0){ #THEN #Trees for all sizes<=restrictions
      NTrees <- MaxDepth
    }else{
      NTrees <- 1
    }
  }else{
    MaxSize<-min(MaxSize,2^(MaxDepth-1)) #! Prevent redundant heap
    NMaxNodes <- 1+2*(MaxSize-1)
    if(bitwAnd(AlgoType,4)>0){ #THEN #!Trees for all sizes<=restrictions
      NTrees <- MaxSize
    }else{
      NTrees <- 1
    }
  }
  TreeTableSize <- NMaxNodes*5


  hAll<-numeric(NTrees)
  h<-0
  Tv<-matrix(0,nrow = NMaxNodes, ncol=5)
  TAll<-array(data=0,dim=c(NMaxNodes,5,NTrees))#mxCreateFull(NMaxNodes, 5, 0) iteration over Ntrees

  Tree<-.Fortran("mainFunction",Y=as.matrix(Y),X=as.matrix(X),
                 Measure=as.integer(measure),
                 MaxSize=as.integer(MaxSize), MaxDepth=as.integer(MaxDepth),
                 MinNodeSize=as.integer(MinNodeSize), MinH=as.double(MinH),
                 AlgoType=as.integer(AlgoType), BoundH=as.double(BoundH),
                 LookAheadDepth=as.integer(LookAheadDepth),Prior=as.double(Prior),
                 LossM=as.double(LossM), MY=as.integer(MY), NY=as.integer(NY),
                 MX=as.integer(MX), NX= as.integer(NX),
                 #output variables TAll, hAll, h, Tv
                 TAll=as.array(TAll), hAll=as.double(hAll),
                 h=as.double(h),Tv=as.matrix(Tv),
                 TreeTableSize=as.integer(TreeTableSize), NTrees=as.integer(NTrees),
                 warn=as.integer(warn), NMaxNodes=as.integer(NMaxNodes),
                 XType=as.integer(XType),
                 PACKAGE="ExactTree")


  return(Tree)
}
