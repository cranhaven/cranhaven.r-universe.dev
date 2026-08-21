UnravelCells<-function(C){
#flats out all elements of all (nested) cells into one vector
#library(pracma)
Vector <- c()
if (!isempty(C)){
  Vector <- UnravelCellsR(C)
}

return(Vector)

}




UnravelCellsR<-function(C){#put all the lists in a column list.
#library(pracma)
C<-Reshape(C,prod(dim(C)),1)
n<-max(dim(C))
Vector <- c()
for(i in 1:n){
  if (is.list(C[[i]])){
    Vector <- c(Vector,UnravelCellsR(C[[i]])) #c or cbind? or rbind?
  }else{
    ci<-C[[i]]
    ci<-Reshape(ci,prod(dim(ci)),1)
    Vector <- c(Vector,t(ci)) #c or cbind? or rbind?
  }
}

return(Vector)

}


