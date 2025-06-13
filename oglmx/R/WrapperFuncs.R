probit.reg<-function(formula,data,start=NULL,weights = NULL,beta=NULL,analhessian=TRUE,na.action,savemodelframe=FALSE,robust=FALSE){
  cl<-match.call()
  cl[[1]]<-oglmx
  names(cl)[match("formula",names(cl))]<-"formulaMEAN"
  cl$threshparam<-0
  cl$delta<-0
  results<-eval(cl)
  return(results)
}

logit.reg<-function(formula,data,start=NULL,weights=NULL,beta=NULL,analhessian=TRUE,na.action,savemodelframe=FALSE,robust=FALSE){
  cl<-match.call()
  cl[[1]]<-oglmx
  names(cl)[match("formula",names(cl))]<-"formulaMEAN"
  cl$link<-"logit"
  cl$threshparam<-0
  cl$delta<-0
  results<-eval(cl)
  return(results)
  
}

oprobit.reg<-function(formula,data,start=NULL,weights=NULL,beta=NULL,threshparam=NULL,analhessian=TRUE,na.action,savemodelframe=FALSE,robust=FALSE,Force=FALSE){
  cl<-match.call()
  cl[[1]]<-oglmx
  names(cl)[match("formula",names(cl))]<-"formulaMEAN"
  cl$link<-"probit"
  cl$delta<-0
  cl$constantMEAN<-FALSE
  
  results<-eval(cl)
  return(results)
  
}

ologit.reg<-function(formula,data,start=NULL,weights=NULL,beta=NULL,threshparam=NULL,analhessian=TRUE,na.action,savemodelframe=FALSE,robust=FALSE,Force=FALSE){
  cl<-match.call()
  cl[[1]]<-oglmx
  names(cl)[match("formula",names(cl))]<-"formulaMEAN"
  cl$link<-"logit"
  cl$delta<-0
  cl$constantMEAN<-FALSE
  results<-eval(cl)
  return(results)
}