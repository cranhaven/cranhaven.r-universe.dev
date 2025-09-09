#' @title Genotype probabilities
#' @description Calculate genotype probabilities from parental probabilities
#' @param probP vector of parental probabilities
#' @param probM vector of parental probabilities
#' @return matrix of genotype probabilities
#' @export
genotypeProbs <- function(probP,probM){
  a<-outer(probP,probM)
  #hago la matriz cuadrada si no lo es
  newrows <- colnames(a)[which(!colnames(a)%in%rownames(a))]
  newcols <- rownames(a)[which(!rownames(a)%in%colnames(a))]
  if(length(newrows)>0){
    a<-rbind(a,matrix(0,ncol=ncol(a),nrow=length(newrows)))
    rownames(a)[(nrow(a)-length(newrows)+1):nrow(a)]<-newrows
  }
  if(length(newcols)>0){
    a<-cbind(a,matrix(0,nrow=nrow(a),ncol=length(newcols)))
    colnames(a)[(ncol(a)-length(newcols)+1):ncol(a)]<-newcols
  }
  a <- a[order(as.numeric(rownames(a))),order(as.numeric(colnames(a)))]

  b<-a+t(a)         #sumo para obtener probs de genotipos heterocigotas
  
  diag(b) <- diag(a)    #en la diagonal no deberia sumar...asi que lo corrijo
  aux     <- t(t(b[upper.tri(b,diag=TRUE)])) 
  rownames(aux) <- paste(rownames(b)[row(b)],colnames(b)[col(b)],sep="/")[upper.tri(b,diag=TRUE)]
  aux <- aux[order(rownames(aux)),]
  return(aux)
}
