ProcessSubTree<-function(Y,X,node,Tree, check, measure, Prior, LossM, ClassSizes, Indices){
  #library(Rfast)
  #global Tree check MISSING

  #Tree<-globalenv()$Tree

  n<-NROW(X)
  if(n==0){
    stop('empty node!')
  }

  h<-GetHeterogeneity(Y=Y, measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)
  if(h>0){
    if(check && (Tree[node,1]<=0) && (abs(Tree[node,5]-h)/h)>0.01){
      warning(paste0('Heterogeneity different for node ',node, Tree[node,5],h))
      Tree[node,5]<-h
    }
  }

  Tree[node,6]<-NROW(Y)
  Tree[node,7]<-GetPrediction(Y, measure, Prior, LossM, ClassSizes)
  if(NROW(X)==1){
    Cat<-rbind(X, X)
  }else{
    Cat<-rbind(apply(X,2,min), apply(X,2,max))
  }
  Tree[node,8:NCOL(Tree)]<-matrix(as.vector(Cat),nrow=1)

  Indices[[2*node-1]]<-0
  Indices[[2*node]]<-0

  # Sizes[[2*node-1]]<-list()
  # Sizes[[2*node]]<-list()
  #
  # Prob[[2*node-1]]<-list()
  # Prob[[2*node]]<-list()


  if(Tree[node,1]>0){
    p<-Tree[node,1]
    s<-Tree[node,2]
    SpSpout<-SplitSpace(Y,X,p,s)
    Y1<-SpSpout$Y1
    if(NCOL(Y1)==1 || NROW(Y1)==1){
      Y1<-matrix(Y1, ncol = 1)
    }
    X1<-SpSpout$X1
    if(NCOL(X1)==1 || NROW(X1)==1){
      X1<-matrix(X1, ncol = 1)
    }
    Y2<-SpSpout$Y2
    if(NCOL(Y2)==1 || NROW(Y2)==1){
      Y2<-matrix(Y2, ncol = 1)
    }
    X2<-SpSpout$X2
    if(NCOL(X2)==1 || NROW(X2)==1){
      X2<-matrix(X2, ncol = 1)
    }

    Indices[[2*node-1]]<-SpSpout$I1
    Indices[[2*node]]<-SpSpout$I2


    # if(measure!=0){
    #   Yleft<-as.factor(Y[Indices[[2*node-1]],])
    #   if(length(Yleft)!=0){
    #     Sizes[[2*node-1]]<-table(Yleft)
    #     names(Sizes[[2*node-1]])<-NULL
    #     Prob[[2*node-1]]<-Sizes[[2*node-1]]/length(Yleft)
    #   }
    #
    #   Yright<-Y[Indices[[2*node]],]
    #   if(length(Yright)!=0){
    #     Sizes[[2*node]]<-table(Yright)
    #     names(Sizes[[2*node]])<-NULL
    #     Prob[[2*node]]<-Sizes[[2*node]]/length(Yright)
    #   }
    # }


    ProcessST1<-ProcessSubTree(Y1,X1,node+Tree[node,3], Tree, check, measure, Prior, LossM, ClassSizes,Indices)
    Tree<-ProcessST1$Tree
    Indices<-ProcessST1$Indices

    ProcessST2<-ProcessSubTree(Y2,X2,node+Tree[node,4], Tree, check, measure, Prior, LossM, ClassSizes,Indices)
    Tree<-ProcessST2$Tree
    Indices<-ProcessST2$Indices

    Tree[node,3]<-node+Tree[node,3]
    Tree[node,4]<-node+Tree[node,4]
  }


  return(list(Tree=Tree,Indices=Indices))


}
