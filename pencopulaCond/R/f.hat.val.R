f.hat.val <- function(penden.env,cal=FALSE,temp=FALSE) {
  if(cal) {
    fit <- get("tilde.PSI.d.D",penden.env)%*%get("ck.val",penden.env)
    fit[fit<0&fit>-1e-8]<-0
    if(all(fit>=0)) {
      assign("f.hat.val",fit,penden.env)
      assign("no",FALSE,penden.env)
    }
    else{
      assign("no",TRUE,penden.env)
      return(paste("d=",get("d",penden.env),"D=",get("D",penden.env),"lambda=",get("lambda",penden.env)[1],sep=""))
    }
  }
  if(temp) {
    fit <- get("tilde.PSI.d.D",penden.env)%*%get("ck.val.temp",penden.env)
    fit[fit<0&fit>-1e-8]<-0
    if(all(fit>=0)) { 
       assign("f.hat.val.temp",fit,penden.env)
       assign("no",FALSE,penden.env)
    }
    else{
      ind<-which(fit<0)
      print(get("Y",penden.env)[ind,])
      print(fit[ind])
      assign("no",TRUE,penden.env)
      return(paste("d=",get("d",penden.env),"D=",get("D",penden.env),"lambda=",get("lambda.temp",penden.env)[1],sep=""))
    }
  }
}
