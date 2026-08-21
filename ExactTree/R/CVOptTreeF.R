CVOptTreeF<-function(CVVec,Y,X,XType,measureCB,defMaxSize,defMaxDepth,defMinNodeSize,defMinH,AlgoType,BoundH,UP,UL,IsInterrupted, LookAheadDepth, measure, Prior, LossM, ClassSizes){
#library(pracma)
#CVOptTreeF(fid,CVVec,Y,X,measureCB,defMaxSize,defMaxDepth,defMinNodeSize,defMinH,AlgoType,BoundH,UP,UL)
#  global CurrentCV LogFileName LookAheadDepth IsInterrupted measure;

#measure<-globalenv()$measure
#IsInterrupted<-globalenv()$IsInterrupted
#LookAheadDepth<-globalenv()$LookAheadDepth
#LogFileName<-globalenv()$LogFileName


if(bitwAnd(AlgoType,1)==0){
  AlgoType<-AlgoType+1 #force tree table output
}

if(bitwAnd(AlgoType,4)>0){ #all trees
  if(defMaxSize>0){
    NTrees<-defMaxSize
  }else{
    NTrees<-defMaxDepth
  }

}else{
    NTrees<-1
}


n<-NROW(X)
NCV<-max(CVVec) #if CVVec is a vector then max, otherwise apply(CVVec,2, max)
Ypred<-list() #cell(NTrees);

for (t in 1:NTrees){
  Ypred[[t]]<-matrix(0, nrow = n, ncol = 1)
}

hTotal<-rep(0,NTrees)
for(c in 1:NCV){#(c in 1:NCV){
  CurrentCV <- c

  Pt<-which(CVVec==c)
  Pl<-which(CVVec!=c)
  Yt<-Y[Pt,]
  Xt<-X[Pt,]
  Yl<-Y[Pl,]
  Xl<-X[Pl,]


  optTreeFOut<-OptTreeF(Yl,Xl,XType,measureCB,defMaxSize,defMaxDepth,defMinNodeSize,defMinH,AlgoType,
                           BoundH,UP,UL,LookAheadDepth)

  h<-optTreeFOut$h
  Tree<-optTreeFOut$Tv
  hAll<-optTreeFOut$hAll
  TAll<-optTreeFOut$TAll



  for(t in 1:NTrees){
    #do postprocessing stuff in MATLAB
    ProcessT<-ProcessTree(Y=Y,X=X,Tv=TAll[,,t],node=1,measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)
    Tree<-ProcessT$Tv
    GLSout<-GetLearningStuff(Tree,Yl,Xl,Yt,Xt, measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)
    Ypredl<-GLSout$Ypred
    h<-GLSout$h
    for(id in 1:length(Pt)){
      idPt<-Pt[id]
      Ypred[[t]][idPt]<-Ypredl[id]
    }
    hTotal[t]<-hTotal[t]+h
  }
}

  if(measure!=0){
    stop('noCV. CROSSVALIDATED ERROR IS NOT YET CALCULATED FOR CLASSIFICATION TREES (MEASURE=1 or 3)')
  }

  hAll<-matrix(hAll, nrow = 1)
  NTrees <- dim(hAll)[2]
  # fprintf(file=fid,fmt='\n************************************************************\n')
  # fprintf(file=fid,fmt='\n%i-FOLD CROSS VALIDATED ERROR:\n',NCV)
  # fprintf(file=fid,fmt='%s\n','Size       Error     Rel.Error            SE           SE2         CHECK')
  maxH<-GetHeterogeneity(Y=Y, measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)

  Size<-rep(NA,NTrees)
  Error<-rep(NA,NTrees)
  SE<-rep(NA,NTrees)
  SE2<-rep(NA,NTrees)
  RelError<-rep(NA,NTrees)#Error/maxH

  for(t in 1:NTrees){
    Size[t] <- MaxEndNodes(t,NTrees,defMaxSize,defMaxDepth)
    #[Error,SE]=GetCVErrorAndSE(Y,Ypred{t});
    if(measure==0){
      GCVEASEout<-GetCVErrorAndSE(Y=Y,Ypred=Ypred[[t]], measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)
      Error[t]<-GCVEASEout$Error
      SE[t]<-GCVEASEout$SE
      SE2[t]<-GCVEASEout$SE2
      RelError[t]<-Error[t]/maxH
      #fprintf(file=fid,fmt='%2i %13.4f %13.4f %13.4f %13.4f %13.4f\n',Size,Error,Error/maxH,SE,SE2,hTotal[t])
    }else{
      #fprintf(file=fid,fmt='%2i %13.4f %13.4f %13.4f %13.4f\n',Size,Error,Error/maxH,SE,hTotal[t])
    }
  }


  CVoutput<-list(Size=Size, Error=Error, Rel_error=RelError, SE=SE, SE2=SE2, CHECK=hTotal)

  return(CVoutput)


}
