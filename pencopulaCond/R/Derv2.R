Derv2 <- function(penden.env,temp.lam=FALSE,temp.ck=FALSE,lam.fit=NULL) {
  ind.val<-get("ind.val",penden.env)
  if(!temp.lam) lambda<-get("lambda",penden.env)
  if(temp.lam) lambda<-get("lambda.temp",penden.env)
  if(temp.ck) {
       ck<-get("ck.val.temp",penden.env)
       Fy<-get("tilde.PSI.d.D",penden.env)%*%ck
  }
  if(!temp.ck) {
       ck<-get("ck.val",penden.env)
       Fy<-get("tilde.PSI.d.D",penden.env)%*%ck
  }
  if(!is.null(lam.fit)) lambda<-lam.fit
  if(!is.null(ind.val)) {
     ck<-ck[-ind.val]
     Fy <- get("tilde.PSI.d.D",penden.env)[,-ind.val]%*% ck
     DDD.sum<- get("DDD.sum",penden.env)[-ind.val,-ind.val]
  }
  else {
     DDD.sum<- get("DDD.sum",penden.env)
  }
  if(length(lam.fit>0)) DDD.sum<-get("DDD.sum",penden.env)
  if(any(Fy<1e-10)) {
     ind<-which(Fy<1e-10)
     Fy<-Fy[-ind,]
     if(is.null(ind.val)|!is.null(lam.fit)) {
       Fy<-kronecker(Fy , matrix(1,1,dim(get("tilde.PSI.d.D",penden.env)[-ind,])[2]))
       cp<- -crossprod(get("tilde.PSI.d.D",penden.env)[-ind,]/Fy)
     }
     if(!is.null(ind.val)&is.null(lam.fit)) {
       Fy<-kronecker(Fy , matrix(1,1,dim(get("tilde.PSI.d.D",penden.env)[-ind,-ind.val])[2]))
       cp<- -crossprod(get("tilde.PSI.d.D",penden.env)[-ind,-ind.val]/Fy)
     }
  }
  else {
     if(is.null(ind.val)|!is.null(lam.fit)) {
       Fy<-kronecker(Fy , matrix(1,1,dim(get("tilde.PSI.d.D",penden.env))[2]))
       cp<- -crossprod(get("tilde.PSI.d.D",penden.env)/Fy)
     }
     if(!is.null(ind.val)&is.null(lam.fit)) {
       Fy<-kronecker(Fy , matrix(1,1,dim(get("tilde.PSI.d.D",penden.env)[,-ind.val])[2]))
       cp<- -crossprod(get("tilde.PSI.d.D",penden.env)[,-ind.val]/Fy)
     }
  }
  if(temp.lam|temp.ck) {
    D2<- cp-lambda[1]*DDD.sum
    aa<-try(solve(D2),silent=TRUE)
    if(class(aa)=="try-error") {
       print("Derv2 can not be inverted, condition of Derv2 is very bad")
       #browser()
       assign("errorDerv2",TRUE,penden.env)
    }
    assign("Derv2.pen.temp",(cp-lambda[1]*DDD.sum),penden.env)
    assign("Derv2.cal.temp",cp,penden.env)
    assign("errorDerv2",FALSE,penden.env)
  }
  else {
    D2<- cp-lambda[1]*DDD.sum
    aa<-try(solve(D2),silent=TRUE)
    if(class(aa)=="try-error") {
       print("Derv2 can not be inverted, condition of Derv2 is very bad")
       assign("errorDerv2",TRUE,penden.env)
    }
    assign("Derv2.pen",(cp-lambda[1]*DDD.sum),penden.env)
    assign("Derv2.cal",cp,penden.env)
    assign("errorDerv2",FALSE,penden.env)
  }
} 
