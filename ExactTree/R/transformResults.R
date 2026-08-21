transformResults<-function(TAll,NtermNodes,X,Y, Desc, CVdata, origData){


  if(is.list(TAll)){
    Tree<-TAll[[NtermNodes]]
  }else{
    Tree<-TAll
  }

  #MISSING WHEN NtermNodes=1






  # #Here I need to process the tree MISSING
  # Tree<-as.matrix(Tree)
  # if(Tree[1,1] > 0){
  #   #tables are available, recalculate everything in MATLAB and
  #   #show the results for each tree
  #   for(t in 1:NTrees){
  #     #do postprocessing stuff in MATLAB
  #     Tv <- TAll[[t]]
  #     Tv <- ProcessTree(Y,X,Tv,1,1) #last argument: boolean check that signals check of fortran output
  #     TAllList[[t]] <- Tv
  #     fprintf(file=fid,fmt='\n%s','POST-PROCESSED RESULTS FOR TREE OF ')
  #     if(maxSize==0){
  #       fprintf(file=fid,fmt='MAX DEPTH %2i\n',(maxLevel+1)-(NTrees-t))
  #     }else{
  #       fprintf(file=fid,fmt='MAX SIZE %2i MAX DEPTH %2i\n',maxSize-(NTrees-t),maxLevel+1)
  #     }
  #     #DisplayTreeResults(fid,Tv,Y,X,['DisplayNodes,DisplayMSpace']);
  #     #DisplayTreeResults(fid,Tv,Y,X,'DisplayNodes') #MISSING, DIMENSIONAL ERROR
  #   }
  #   Tree<-as.matrix(TAllList[[NTrees]])
  # }



  tNodes<-which(Tree[,1]==0)

  list_results<-list()

  list_results$li<-data.frame(Node=tNodes,N=Tree[tNodes,6],y=Tree[tNodes,7],h=Tree[tNodes,5], row.names = paste0("Leaf ",1:length(tNodes)))


  m<-length(Desc)/2

  if(NtermNodes > 1){
    sNodes<-which(Tree[,1]>0)
    split_vars<-c()
    split_points<-c()
    for (i in 1:length(sNodes)) {
      j<-sNodes[i]
      split_vars[i]<-Desc[[m+Tree[j,1]]]
      #Split point different if the variable is categorical or not. Change 16/03/26
      if(is.factor(origData[,split_vars[i]])){
        split_points[i]<-as.character(Desc[[Tree[j,1]]][(Tree[j,2])])#as.character added on 08/04/2026
      }else{
        split_points[i]<-Desc[[Tree[j,1]]][(Tree[j,2])+1] #Change 10/08/22
      }
    }


    list_results$si<-data.frame(Parent_node=sNodes,Child_nodes_Y=Tree[sNodes,3],Child_node_N=Tree[sNodes,4],Splitting_variable=split_vars,Split_point=split_points,
                                row.names = paste0("Split ",1:length(sNodes)))
  }else{
    list_results$si<-NULL
  }

  list_results$var.names<-unlist(c(paste0("Y",1:NCOL(Y)),Desc[(m+1):(2*m)]))

  list_results$data<-as.data.frame(cbind(Y,X))
  colnames(list_results$data)<-list_results$var.names

  list_results$NtermNodes<-NtermNodes

  list_results$CVOutput<-CVdata

  class(list_results)<-"ETree"

  return(list_results)
}
