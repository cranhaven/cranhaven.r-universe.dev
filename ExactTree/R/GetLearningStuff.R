GetLearningStuff<-function(Tree,Yl,Xl,Yt,Xt, measure, Prior, LossM, ClassSizes){
#library(pracma)

Yl<-as.matrix(Yl)
Yt<-as.matrix(Yt)

m<-dim(Xl)[2]
Ypred<-matrix(rep(0,NROW(Yt)),nrow = NROW(Yt), ncol=1)#dim(Yt)[1]
I<-which(Tree[,1]==0)#=find(Tree(:,1)==0);
Tv<-Tree[I,] #Terminal Nodes
Tv<-matrix(Tv, ncol = NCOL(Tree))
nT<-NROW(Tv)
h<-0
for(t in 1:nT){ #all terminal nodes
  pc<-Reshape(Tv[t,8:(NCOL(Tv))],n=2,m=m) #Use Reshape
  Cl<-SelectCluster(Xl,pc)
  Ct<-SelectCluster(Xt,pc)
  CYl<-Yl[Cl,]
  CYt<-Yt[Ct,]
  s<-max(NROW(Ct))
  #The CV problem is here: With the s and the Ypred


  if (s==0){ #in the case of doing it as in matlab it should be s!=0
    next
  }

  Ypred[Ct,]<-repmat(GetPrediction(CYl, measure, Prior, LossM, ClassSizes),n=s,m=1)
  y<-GetPrediction(CYl,measure, Prior, LossM, ClassSizes)
  h<-h+GetHeterogeneityTest(CYt,y, measure)

}


return(list(Ypred=Ypred, h=h))

}
