ProcessTree<-function(Y,X,Tv,node,defCheck=NULL, measure, Prior, LossM, ClassSizes){
#% build full Tv table given incomplete table produced in Fortran
#% NOTE: nodes are defined in FORTRAN with relative positions


if(is.null(defCheck)){
  check<-0
}else{
  check<-defCheck
}

nc<-NCOL(Tv)
nr<-NROW(Tv)
if(nr==1 || nc==1){
Tv<-matrix(Tv, nrow = nr, ncol = nc)
}
Tv<-cbind(Tv,matrix(0,nrow = nr, ncol = (2 + 2*NCOL(X)))) #[,7+2*NCOL(X)]<-0
Tree <- Tv
Indices<-list()

ProcessT<-ProcessSubTree(Y,X,node,Tree, check, measure, Prior, LossM, ClassSizes,Indices) #Updates the tree
Tree<-ProcessT$Tree
Tv <- Tree

Indices<-ProcessT$Indices



return(list(Tv=Tv,Indices=Indices))

}
