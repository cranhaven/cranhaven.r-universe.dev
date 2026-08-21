computeProb<-function(Y,Indices){

  dimY<-dim(table(Y))
  categoriesY<-names(table(Y))
  Sizes<-list()
  Prob<-list()

  Sizes[[1]]<-table(Y)
  names(Sizes[[1]])<-NULL
  Prob[[1]]<-as.numeric(as.vector(Sizes[[1]]))/NROW(Y)

  for (i in 2:length(Indices)) {
    Ynode<-as.factor(Y[Indices[[i-1]],])
    if(length(Ynode)!=0){
      if(dim(table(Ynode))<dimY){#not all categories in the node
        Sizes[[i]]<-table(factor(Ynode,categoriesY))
      }else{#normal case
        Sizes[[i]]<-table(Ynode)
      }
      names(Sizes[[i]])<-NULL
      Prob[[i]]<-as.numeric(as.vector(Sizes[[i]]))/length(Ynode)
    }
  }


  return(list(Sizes=Sizes, Prob=Prob))

}
