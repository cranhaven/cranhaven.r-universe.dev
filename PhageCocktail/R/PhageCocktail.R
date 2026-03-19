#PhageCocktail

PhageCocktail<-function(pathway,FUN="ClusteringSearch",file.output=FALSE,file.name="output.txt",limit=7){
  #Loading libraries
  #library(readxl)
  #library(stringr)
  #library(factoextra)
  #library(bipartite)
  #library(smerc)
  #library(RJSONIO)



  #Input-----------------------------------------------------------------

  #Organize data---------------------------------------------------------
  HostRangeMatrix <- read_excel(pathway,na = "NA",col_names = FALSE)
  phage_number<-ncol(HostRangeMatrix)-1
  bacteria_number<-nrow(HostRangeMatrix)-1
  phage_names<-t(t(HostRangeMatrix[1,1:phage_number+1]))
  bacteria_names<-t(HostRangeMatrix[1:bacteria_number+1,1])
  matrix_pb<-HostRangeMatrix[1:bacteria_number+1,1:phage_number+1]
  matrix_pb<-matrix(as.numeric(unlist(matrix_pb)),nrow=nrow(matrix_pb))

  #---------------------------------------------------------------------

  #Control--------------------------------------------------------------

  if(length(limit)==0){
    limit=7
  }
  if(length(limit)!=0){
    if(limit<1){
      stop("Limit number has to be 1 or upper!")
      }
    if(limit>7){limit=7
      warning("Max. limit number is 7! It has been changed")}
  }

  MaxPhage<-phage_number
  new_matrix<-matrix_pb
  list_phage_removed<-c()
  list_new_phage<-c()
  phage_new_name<-c()
  for (p in 1:phage_number){
    count_pb=0
    for (i in 1:bacteria_number){
      count_pb=count_pb+matrix_pb[i,p]}


    if ( count_pb==0){
      MaxPhage<-MaxPhage-1
      list_phage_removed<-c(p,list_phage_removed)}
    if(count_pb>=1){
      list_new_phage<-c(list_new_phage,p)
      phage_new_name<-c(phage_new_name,phage_names[p])
    }}
  for (p in list_phage_removed){
    new_matrix<-new_matrix[,-p]
  }

  MaxBacteria<-bacteria_number
  list_bacteria_removed<-c()
  for (i in 1:bacteria_number){
    count_pb=0
    for (p in 1:MaxPhage){
      count_pb=count_pb+new_matrix[i,p]}
    if ( count_pb==0){
      MaxBacteria<-MaxBacteria-1
      list_bacteria_removed=c(i,list_bacteria_removed)}}
  for (i in list_bacteria_removed){
    new_matrix<-new_matrix[-i,]
  }


  if (FUN=="ExhaustiveSearch"){
    Result<-ExhaustiveSearch(MaxPhage, MaxBacteria,new_matrix,phage_names,limit,file.name,"ES")
  }
  if (FUN=="ExhaustivePhi"){
    Result<-ExhaustivePhi(matrix_pb,bacteria_number, phage_number, MaxPhage,MaxBacteria,new_matrix,phage_names,limit,file.name,"EP")
  }
  if (FUN=="ClusteringSearch"){
    Result<-WardClustering_Search(matrix_pb,new_matrix,MaxPhage,MaxBacteria,bacteria_number,phage_names,phage_number,list_bacteria_removed,limit,list_new_phage,phage_new_name,file.name,"CS")
  }
  if (FUN=="ClusteringPhi"){
    Result<-WardClustering_Phi(list_bacteria_removed,matrix_pb,new_matrix,MaxPhage,MaxBacteria,bacteria_number,phage_names,phage_number,limit,list_new_phage,phage_new_name,file.name,"CP")
  }
  if (FUN!="ExhaustiveSearch" & FUN!="ExhaustivePhi" & FUN!="ClusteringSearch" & FUN!= "ClusteringPhi"){
    return ("There was a mistake in FUNCTION")
  }


  phage_removed<-phage_names[list_phage_removed]
  if (length(list_phage_removed)==0){
    phage_removed<-"All the phages are useful (all of them lyse some bacterium)"
  }
  bacteria_removed<-bacteria_names[list_bacteria_removed]
  if (length(list_bacteria_removed)==0){
    bacteria_removed<-"All the bacteria are lysed by some phage"
  }


  list_pn<-phageselected(FUN,phage_names,Result)


  if(FUN=="ExhaustivePhi" | FUN=="ClusteringPhi"){
    if(Result[1]==0){print("Phi is 0, so there is no possible result")}
    if(Result[1]==1){
      a<-list(Size1=Result[c(-1,-2)])
      list_bacteria_lysed<-bacteria_lysed(1,bacteria_number,matrix_pb,list_pn,bacteria_names)

      list_lysed=list(Size1=list_bacteria_lysed)
    }
    if(Result[1]==2){
      a<-list(Size2=Result[c(-1,-2)])
      list_bacteria_lysed<-bacteria_lysed(2,bacteria_number,matrix_pb,list_pn,bacteria_names)

      list_lysed=list(Size2=list_bacteria_lysed)
    }
    if(Result[1]==3){
      a<-list(Size3=Result[c(-1,-2)])
      list_bacteria_lysed<-bacteria_lysed(3,bacteria_number,matrix_pb,list_pn,bacteria_names)

      list_lysed=list(Size3=list_bacteria_lysed)
    }
    if(Result[1]==4){
      a<-list(Size4=Result[c(-1,-2)])
      list_bacteria_lysed<-bacteria_lysed(4,bacteria_number,matrix_pb,list_pn,bacteria_names)


      list_lysed=list(Size4=list_bacteria_lysed)
    }
    if(Result[1]==5){
      a<-list(Size5=Result[c(-1,-2)])
      list_bacteria_lysed<-bacteria_lysed(5,bacteria_number,matrix_pb,list_pn,bacteria_names)

      list_lysed=list(Size5=list_bacteria_lysed)
    }
    if(Result[1]==6){
      a<-list(Size6=Result[c(-1,-2)] )
      list_bacteria_lysed<-bacteria_lysed(6,bacteria_number,matrix_pb,list_pn,bacteria_names)

      list_lysed=list(Size6=list_bacteria_lysed)
    }
    if(Result[1]==7){
      a<-list(Size7=Result[c(-1,-2)])
      list_bacteria_lysed<-bacteria_lysed(7,bacteria_number,matrix_pb,list_pn,bacteria_names)

      list_lysed=list(Size7=list_bacteria_lysed)
    }

  }



  if(FUN=="ExhaustiveSearch"|FUN=="ClusteringSearch"){
    if(Result[1]==0){print("MaxPhage is 0, so there is no possible result")}
    if(Result[1]==1){
      a<-list(Size1=Result[c(-1,-2)])

      list_bacteria_lysed<-bacteria_lysed(1,bacteria_number,matrix_pb,list_pn,bacteria_names)
      list_lysed=list(Size1=list_bacteria_lysed)

    }
    if(Result[1]==2){
      a<-list(Size2=Result[c(3,4,5)],Size1=Result[c(6,7)])

      list_bacteria_lysed_size2<-bacteria_lysed(2,bacteria_number,matrix_pb,list_pn[2:3],bacteria_names)

      list_bacteria_lysed_size1<-bacteria_lysed(1,bacteria_number,matrix_pb,list_pn[1],bacteria_names)



      list_lysed<-list(Size2=list_bacteria_lysed_size2,Size1=list_bacteria_lysed_size1)
    }

    if(Result[1]==3){
      a<-list(Size3=Result[c(3,4,5,6)],Size2=Result[c(7,8,9)],Size1=Result[c(10,11)])
      list_bacteria_lysed_size3<-bacteria_lysed(3,bacteria_number,matrix_pb,list_pn[4:6],bacteria_names)

      list_bacteria_lysed_size2<-bacteria_lysed(2,bacteria_number,matrix_pb,list_pn[2:3],bacteria_names)

      list_bacteria_lysed_size1<-bacteria_lysed(1,bacteria_number,matrix_pb,list_pn[1],bacteria_names)


      list_lysed<-list(Size3=list_bacteria_lysed_size3,Size2=list_bacteria_lysed_size2,Size1=list_bacteria_lysed_size1)
    }

    if(Result[1]==4){
      a<-list(Size4=Result[c(3,4,5,6,7)],Size3=Result[c(8,9,10,11)],Size2=Result[c(12,13,14)],Size1=Result[c(15,16)])
      list_bacteria_lysed_size4<-bacteria_lysed(4,bacteria_number,matrix_pb,list_pn[7:10],bacteria_names)

      list_bacteria_lysed_size3<-bacteria_lysed(3,bacteria_number,matrix_pb,list_pn[4:6],bacteria_names)

      list_bacteria_lysed_size2<-bacteria_lysed(2,bacteria_number,matrix_pb,list_pn[2:3],bacteria_names)

      list_bacteria_lysed_size1<-bacteria_lysed(1,bacteria_number,matrix_pb,list_pn[1],bacteria_names)


      list_lysed<-list(Size4=list_bacteria_lysed_size4,Size3=list_bacteria_lysed_size3,Size2=list_bacteria_lysed_size2,Size1=list_bacteria_lysed_size1)

    }
    if(Result[1]==5){
      a<-list(Size5=Result[c(3,4,5,6,7,8)],Size4=Result[c(9,10,11,12,13)],Size3=Result[c(14,15,16,17)],Size2=Result[c(18,19,20)],Size1=Result[c(21,22)])
      list_bacteria_lysed_size5<-bacteria_lysed(5,bacteria_number,matrix_pb,list_pn[11:15],bacteria_names)

      list_bacteria_lysed_size4<-bacteria_lysed(4,bacteria_number,matrix_pb,list_pn[7:10],bacteria_names)

      list_bacteria_lysed_size3<-bacteria_lysed(3,bacteria_number,matrix_pb,list_pn[4:6],bacteria_names)

      list_bacteria_lysed_size2<-bacteria_lysed(2,bacteria_number,matrix_pb,list_pn[2:3],bacteria_names)

      list_bacteria_lysed_size1<-bacteria_lysed(1,bacteria_number,matrix_pb,list_pn[1],bacteria_names)


      list_lysed<-list(Size5=list_bacteria_lysed_size5,Size4=list_bacteria_lysed_size4,Size3=list_bacteria_lysed_size3,Size2=list_bacteria_lysed_size2,Size1=list_bacteria_lysed_size1)

    }
    if(Result[1]==6){
      a<-list(Size6=Result[c(3,4,5,6,7,8,9)],Size5=Result[c(10,11,12,13,14,15)],Size4=Result[c(16,17,18,19,20)],Size3=Result[c(21,22,23,24)],Size2=Result[c(25,26,27)],Size1=Result[c(28,29)])
      list_bacteria_lysed_size6<-bacteria_lysed(6,bacteria_number,matrix_pb,list_pn[16:21],bacteria_names)

      list_bacteria_lysed_size5<-bacteria_lysed(5,bacteria_number,matrix_pb,list_pn[11:15],bacteria_names)

      list_bacteria_lysed_size4<-bacteria_lysed(4,bacteria_number,matrix_pb,list_pn[7:10],bacteria_names)

      list_bacteria_lysed_size3<-bacteria_lysed(3,bacteria_number,matrix_pb,list_pn[4:6],bacteria_names)

      list_bacteria_lysed_size2<-bacteria_lysed(2,bacteria_number,matrix_pb,list_pn[2:3],bacteria_names)

      list_bacteria_lysed_size1<-bacteria_lysed(1,bacteria_number,matrix_pb,list_pn[1],bacteria_names)


      list_lysed<-list(Size6=list_bacteria_lysed_size6,Size5=list_bacteria_lysed_size5,Size4=list_bacteria_lysed_size4,Size3=list_bacteria_lysed_size3,Size2=list_bacteria_lysed_size2,Size1=list_bacteria_lysed_size1)

    }
    if(Result[1]==7){
      a<-list(Size7=Result[c(3,4,5,6,7,8,9,10)],Size6=Result[c(11,12,13,14,15,16,17)],Size5=Result[c(18,19,20,21,22,23)],Size4=Result[c(24,25,26,27,28)],Size3=Result[c(29,30,31,32)],Size2=Result[c(33,34,35)],Size1=Result[c(36,37)])
      list_bacteria_lysed_size7<-bacteria_lysed(7,bacteria_number,matrix_pb,list_pn[22:28],bacteria_names)

      list_bacteria_lysed_size6<-bacteria_lysed(6,bacteria_number,matrix_pb,list_pn[16:21],bacteria_names)

      list_bacteria_lysed_size5<-bacteria_lysed(5,bacteria_number,matrix_pb,list_pn[11:15],bacteria_names)

      list_bacteria_lysed_size4<-bacteria_lysed(4,bacteria_number,matrix_pb,list_pn[7:10],bacteria_names)

      list_bacteria_lysed_size3<-bacteria_lysed(3,bacteria_number,matrix_pb,list_pn[4:6],bacteria_names)

      list_bacteria_lysed_size2<-bacteria_lysed(2,bacteria_number,matrix_pb,list_pn[2:3],bacteria_names)

      list_bacteria_lysed_size1<-bacteria_lysed(1,bacteria_number,matrix_pb,list_pn[1],bacteria_names)


      list_lysed<-list(Size7=list_bacteria_lysed_size7,Size6=list_bacteria_lysed_size6,Size5=list_bacteria_lysed_size5,Size4=list_bacteria_lysed_size4,Size3=list_bacteria_lysed_size3,Size2=list_bacteria_lysed_size2,Size1=list_bacteria_lysed_size1)

    }
  }






  if (file.output==TRUE){


    exportJSON <- toJSON(list(file=pathway,method=FUN,maxCocktailSize=Result[1], allBacteriaLysed=Result[2] ,Result=a,BacteriaLysed=list_lysed, non_usefulPhages=phage_removed , non_lysedBacteria=bacteria_removed))
    write(exportJSON,file.name)

  }
  list(file=pathway,method=FUN,maxCocktailSize=Result[1], allBacteriaLysed=Result[2] ,Result=a,BacteriaLysed=list_lysed, non_usefulPhages=phage_removed , non_lysedBacteria=bacteria_removed)

}



#---------------------------------


