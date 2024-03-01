bnclustmodelscore<-function(bnres,score="AIC",ss=100, nbin=0) {
  likel<-bnres$likel[length(bnres$likel)]
  #print(likel)
  npar<-2*(ncol(bnres$DAGs[[1]])-nbin)*length(bnres$DAGs)+sum(unlist(lapply(bnres$DAGs,sum)))
  #print(npar)
  if(score=="AIC") return(2*npar-2*likel) else if(score=="BIC") return(log(ss)*npar-2*likel) else
    if(score=="ll") return(likel)
}
