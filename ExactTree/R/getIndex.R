getIndex<-function(prevNode=1, side="left", node,Tree,Index, keepnode=NULL){


if(Tree[node,1]!=0){

  #compute index
  if(node!=1){

    indexPrev<-ifelse(side=="left", 2*prevNode-1, 2*prevNode)
    Index[[2*(node)-1]]<-Index[[indexPrev]][Index[[2*(node)-1]]]
    Index[[2*(node)]]<-Index[[indexPrev]][Index[[2*(node)]]]
  }



  #Reiterations
  leftSide<-getIndex(node, side = "left", node + Tree[node,3],Tree,Index, keepnode)
  Index<-leftSide$Index

  rightSide<-getIndex(node, side = "right", node + Tree[node,4],Tree,Index, keepnode)
  Index<-rightSide$Index


  keepnode<-c(leftSide$keep,rightSide$keep)


}else{
  if(side=="left"){
    keepnode<-c(keepnode, 2*prevNode-1)
  }else{
    keepnode<-c(keepnode, 2*prevNode)
  }

  out<-list(Index=Index, keep=keepnode)
  return(out) #stop("Final node")
}





out<-list(Index=Index, keep=keepnode)
return(out)







}
