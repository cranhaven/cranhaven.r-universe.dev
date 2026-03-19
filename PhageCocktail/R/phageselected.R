
phageselected<-function(FUN,phage_names,Result){

  if(FUN=="ExhaustivePhi"| FUN=="ClusteringPhi"){
    return(c(phagelist(phage_names,c(Result[-2]))))

  }

  if(FUN=="ExhaustiveSearch" | FUN=="ClusteringSearch"){
    l<-length(Result)
    list_pn<-phagelist(phage_names,c(1,Result[l-1]))
    Size1<-list_pn
    if (l==4){return(Size1)  }
    list_pn<-phagelist(phage_names,c(2,Result[l-4],Result[l-3]))
    Size2<-list_pn
    if (l==7){return(c(Size1,Size2))}
    list_pn<-phagelist(phage_names,c(3,Result[l-6],Result[l-7],Result[l-8]))
    Size3<-list_pn
    if(l==11){return(c(Size1,Size2,Size3))}
    list_pn<-phagelist(phage_names,c(4,Result[l-10],Result[l-11],Result[l-12],Result[l-13]))
    Size4<-list_pn
    if(l==16){return(c(Size1,Size2,Size3,Size4))}
    list_pn<-phagelist(phage_names,c(5,Result[l-15],Result[l-16],Result[l-17],Result[l-18],Result[l-19]))
    Size5<-list_pn
    if(l==22){return(c(Size1,Size2,Size3,Size4,Size5))}
    list_pn<-phagelist(phage_names,c(6,Result[l-21],Result[l-22],Result[l-23],Result[l-24],Result[l-25],Result[l-26]))
    Size6<-list_pn
    if(l==29){return(c(Size1,Size2,Size3,Size4,Size5,Size6))}
    list_pn<-phagelist(phage_names,c(7,Result[l-28],Result[l-29],Result[l-30],Result[l-31],Result[l-32],Result[l-33],Result[l-34]))
    Size7<-list_pn
    return(c(Size1,Size2,Size3,Size4,Size5,Size6,Size7))
  }
}



