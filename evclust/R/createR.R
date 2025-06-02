createR<-function(mean,Sigma,pro,X){
  # Computation of pairwise probabilities P_{ij}
  # Function called by bootclus
  N<-nrow(X)
  G<-length(pro)
  varpar<-mclustVariance("VVV",ncol(X),G)
  varpar$cholsigma<-simplify2array(alply(Sigma,3,chol))
  par<-list(pro=pro,mean=mean,variance=varpar)
  P<-cdens(modelName="VVV",data=X,parameters = par)*matrix(pro,N,G,byrow=TRUE)
  P<-P/matrix(rowSums(P),N,G)
  R<-P%*%t(P)
}