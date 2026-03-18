Derv1 <- function(penden.env,temp.lam=FALSE,temp.ck=FALSE) {
 ind.val<-get("ind.val",penden.env)
 if(is.null(ind.val)) {
   if(temp.lam|temp.ck) ck<-get("ck.val.temp",penden.env)
   if(!temp.lam&!temp.ck) ck<-get("ck.val",penden.env)
   Fy<-get("tilde.PSI.d.D",penden.env)%*%ck
   assign("tilde.PSI.d.D.t",get("tilde.PSI.d.D",penden.env),penden.env)  
   if(any(Fy<1e-10)) {
     ind<-which(Fy<1e-10)
     Fy<-Fy[-ind,]
     assign("tilde.PSI.d.D.t",get("tilde.PSI.d.D",penden.env)[-ind,],penden.env)  
   }
   pen<-get("lambda",penden.env)[1]*get("DDD.sum",penden.env)%*%ck

   if(!temp.lam&!temp.ck)  assign("Derv1.pen",matrix(colSums(get("tilde.PSI.d.D.t",penden.env)/kronecker(Fy, matrix(1,1,dim(get("tilde.PSI.d.D",penden.env))[2]))),get("DD",penden.env),1)-pen,penden.env)

   if(temp.lam) assign("Derv1.pen.temp",matrix(colSums(get("tilde.PSI.d.D.t",penden.env)/kronecker(Fy, matrix(1,1,dim(get("tilde.PSI.d.D",penden.env))[2]))),get("DD",penden.env),1)-pen,penden.env)

   if(temp.ck) assign("Derv1.pen.temp",matrix(colSums(get("tilde.PSI.d.D.t",penden.env)/kronecker(Fy, matrix(1,1,dim(get("tilde.PSI.d.D",penden.env))[2]))),get("DD",penden.env),1)-pen,penden.env)
 }
}
