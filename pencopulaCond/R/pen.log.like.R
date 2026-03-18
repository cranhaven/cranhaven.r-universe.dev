 pen.log.like <- function(penden.env,cal=FALSE,temp.lam=FALSE,temp.ck=FALSE) {
 lam.eq<-get("lam.equal",penden.env)
 if(cal) ck<-get("ck.val",penden.env)
 if(temp.lam|temp.ck) ck<-get("ck.val.temp",penden.env)
  if(!lam.eq) {
     p <- get("p",penden.env) 
     pen<-get("lambda",penden.env)[1,1]*(t(ck)%*%get("DDD",penden.env)[,,1]%*%ck)
     pen<-pen+get("lambda",penden.env)[1,2]*(t(ck)%*%get("DDD",penden.env)[,,2]%*%ck)
     if(p==3) pen+get("lambda",penden.env)[1,3]*(t(ck)%*%get("DDD",penden.env)[,,3]%*%ck)
  }
 if(cal) {
   if(lam.eq) assign("pen.log.like",sum(sapply(get("f.hat.val",penden.env)[get("f.hat.val",penden.env)>0],log))-0.5*get("lambda",penden.env)[1]*(t(get("ck.val",penden.env))%*%get("DDD.sum",penden.env)%*%get("ck.val",penden.env)),penden.env)
   if(!lam.eq) assign("pen.log.like",sum(sapply(get("f.hat.val",penden.env)[get("f.hat.val",penden.env)>0],log))-0.5*pen,penden.env)
   assign("log.like",sum(sapply(get("f.hat.val",penden.env)[get("f.hat.val",penden.env)>0],log)),penden.env)
  }
  if(temp.lam) {
    if(lam.eq) assign("pen.log.like.temp",sum(sapply(get("f.hat.val.temp",penden.env)[get("f.hat.val.temp",penden.env)>0],log))-0.5*get("lambda.temp",penden.env)[1]*(t(get("ck.val.temp",penden.env))%*%get("DDD.sum",penden.env)%*%get("ck.val.temp",penden.env)),penden.env)
    if(!lam.eq) assign("pen.log.like.temp",sum(sapply(get("f.hat.val.temp",penden.env)[get("f.hat.val.temp",penden.env)>0],log))-0.5*pen,penden.env)
    assign("log.like.temp",sum(sapply(get("f.hat.val.temp",penden.env)[get("f.hat.val.temp",penden.env)>0],log)),penden.env)
  }
  if(temp.ck) {
    if(lam.eq) assign("pen.log.like.temp",sum(sapply(get("f.hat.val.temp",penden.env)[get("f.hat.val.temp",penden.env)>0],log))-0.5*get("lambda",penden.env)[1]*(t(get("ck.val.temp",penden.env))%*%get("DDD.sum",penden.env)%*%get("ck.val.temp",penden.env)),penden.env)
    if(!lam.eq) assign("pen.log.like.temp",sum(sapply(get("f.hat.val.temp",penden.env)[get("f.hat.val.temp",penden.env)>0],log))-0.5*pen,penden.env)
    assign("log.like.temp",sum(sapply(get("f.hat.val.temp",penden.env)[get("f.hat.val.temp",penden.env)>0],log)),penden.env)
  }
}
