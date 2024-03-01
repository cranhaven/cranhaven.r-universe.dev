#' Extracting cluster memberships
#'
#' This function extracts a vector with MAP cluster
#' memberships assignments from the 'bnclustOmics' object
#' @param x object of class 'bnclustOmics'
#' @param consensus logical, indicates if consensus clusters will be extracted; FALSE by default
#' @return a vector of length of the number of observations corresponding to cluster assignments obtained by bnclustOmics
#' @examples
#' clusters(bnres3)
#' @export
clusters<-function(x,consensus=FALSE){
  if(consensus){
    return(x$consmemb)
  } else {
    return(x$memb)
  }
}

#' Extracting edge posterior probabilities
#'
#' This function extracts a list of matrices containing posterior probabilities of all edges
#' in the graphs discovered by bnclustOmics when the parameter 'epmatrix' was set to TRUE
#'
#' @param x object of class 'bnclustOmics'
#' @return a list of matrices containing posterior probabilities of all edges
#' in the graphs discovered by bnclustOmics when the parameter 'epmatrix' was set to TRUE
#' @examples
#'post<-posteriors(bnres4)
#' @export
posteriors<-function(x){
 return(x$ep)
}

#' Extracting edge posterior probabilities
#'
#' This function extracts a list of matrices containing posterior probabilities of all edges
#' in the graphs discovered by bnclustOmics when the parameter 'epmatrix' was set to TRUE
#'
#' @param x object of class 'bnclustOmics'
#' @return a list of matrices containing posterior probabilities of all edges
#' in the graphs discovered by bnclustOmics when the parameter 'epmatrix' was set to TRUE
#' @examples
#'DAGs<-dags(bnres3)
#' @export
dags<-function(x) {
  return(x$DAGs)
}

#' Choosing the number of clusters
#'
#' This function can be used for choosing the optimal number of clusters using AIC or BIC scores.
#'
#' @param bnlist list of objects of class 'bnclustOmics'
#' @param fun score function for choosing the optimal number of clusters; available options are 'AIC' or  'BIC'
#' @return a list consisting of a vector of scores extracted from each object of class bnclustOmics and the optimal k
#' @examples
#' bnlist<-list()
#'
#' #bnlist[[k]]<-bnclustOmics(simdata,bnnames,maxEM=4, kclust=k,startpoint = "mclustPCA")
#' bnlist[[2]]<-bnres2
#' bnlist[[3]]<-bnres3
#' bnlist[[4]]<-bnres4
#'
#'chooseK(bnlist,fun="BIC")
#'chooseK(bnlist,fun="AIC")
#' @export
chooseK<-function(bnlist,fun=c("AIC","BIC","likel")){
  allk<-unlist(lapply(bnlist,function(x)length(unique(x$memb))))
  if(any(allk==0)) allk<-allk[-which(allk==0)]
  scores<-unlist(lapply(bnlist,function(x)x[[fun]]))
  res<-list()
  res$allk<-allk
  if(fun%in%c("AIC","BIC")) {
    res[[fun]]<-scores
    res$k<-allk[which.min(scores)]
  } else {
    warning("please choose AIC or BIC")
  }
  return(res)
}



