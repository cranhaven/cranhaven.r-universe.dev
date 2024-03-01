#'Deriving consensus graphs
#'
#'When the parameter 'epmatrix' is set to TRUE, the object of class 'bnclustOmics' includes postrior probabilitis of all edges in the discovered graphs.
#'This function can be used to derive a consensus graph representing discovered clusters according to a specified posterior probability threshold. Only edges with posteriors above the threshold will be included
#'in the resulting consensus models.
#'@param bnres object of class 'bnclustOmics'
#'@param p posterior probability threshold
#'@return a list of adjacency matrices corresponding to consensus graphs representing discovered clusters
#'@author Polina Suter
#'@examples
#'MAPmod<-dags(bnres3)
#'CONSmod1<-getModels(bnres3,p=0.5)
#'CONSmod2<-getModels(bnres3,p=0.9)
#'library(BiDAG)
#'compareDAGs(MAPmod[[1]],simdags[[1]])
#'compareDAGs(CONSmod1[[1]],simdags[[1]])
#'compareDAGs(CONSmod2[[1]],simdags[[1]])
#'@export
getModels<-function(bnres,p) {
  return(lapply(bnres$ep,getmodel,p))
}

getmodel<-function(ep, p) {
  n<-ncol(ep)
  labels<-colnames(ep)
  incidence <- matrix(rep(0, n * n), nrow = n, ncol = n)
  colnames(incidence)<-rownames(incidence)<-labels
  incidence[which(ep > p)] <- 1
  return(incidence)
}
