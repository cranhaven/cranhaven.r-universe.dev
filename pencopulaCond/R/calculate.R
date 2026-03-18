calculate <- function(penden.env) {
 lambda1<-get("lambda",penden.env)[1]
 lambda.sa<-lambda1
 save.direction<-c()
 down<-TRUE
 up<-FALSE
 ck.start<-get("ck.val",penden.env)
 assign("lambda.crash",FALSE,penden.env)
 f.hat.val(penden.env,cal=TRUE)
  liste <- get("new.liste",penden.env)
  assign("wrong.lambda",FALSE,penden.env)
  assign("lambda.out",FALSE,penden.env)
  assign("f.hat.val.start",get("f.hat.val",penden.env),penden.env)
  pen.log.like(penden.env,cal=TRUE)
  Derv1(penden.env)
  Derv2(penden.env)
  marg.likelihood(penden.env,get("pen.log.like",penden.env))
  p <- get("p",penden.env)
  DD <- get("DD",penden.env)
  assign("i",i <- 1,penden.env)
  my.IC(penden.env)
  assign("cAIC.old",get("cAIC",penden.env),penden.env)
  assign("cAIC.temp",get("cAIC",penden.env),penden.env)
  liste[i,1] <- as.numeric(get("pen.log.like",penden.env))
  liste[i,2] <- as.numeric(get("log.like",penden.env))
  liste[i,3] <- as.numeric(get("marg.log.like",penden.env))
  liste[i,(4:(4+p-1))] <- as.numeric(get("lambda",penden.env))
  liste[i,(4+p)]<-get("cAIC",penden.env)
  liste[i,((4+p+1):(4+p+DD))] <- get("ck.val",penden.env)
  assign("liste",liste,penden.env)
  assign("calc",TRUE,penden.env)
  assign("lambda.temp",get("lambda",penden.env),penden.env)
  lambda.change<-FALSE
  new.weights(penden.env,start=TRUE)
      pen.log.like(penden.env,temp.ck=TRUE)
      #print(paste("log.like equals ",get("log.like",penden.env),sep=""))
      Derv1(penden.env,temp.lam=FALSE,temp.ck=TRUE)
      Derv2(penden.env,temp.lam=FALSE,temp.ck=TRUE)
      marg.likelihood(penden.env,get("pen.log.like",penden.env),temp=TRUE)
      my.IC(penden.env,temp=TRUE)
      n.liste <- c(get("pen.log.like.temp",penden.env),get("log.like.temp",penden.env),get("marg.log.like.temp",penden.env),get("lambda",penden.env),get("cAIC.temp",penden.env),get("ck.val.temp",penden.env))
      assign("ck.val",get("ck.val.temp",penden.env),penden.env)
      ck.v<-round(get("ck.val",penden.env),6)[1:7]
      list <- rbind(get("liste",penden.env),n.liste)
      assign("i",i <- i+1,penden.env)
      rownames(list) <- seq(0,(i-1))
      assign("liste",list,penden.env)
      my.loop(penden.env)
 #}
# l.c<-get("lambda.crash",penden.env)
 #if(l.c) {
 #   if(down) assign("lambda",rep(lambda1*0.9,p),penden.env)
 #   if(up) assign("lambda",rep(lambda1*1.1,p),penden.env)
 #   lambda1<-get("lambda",penden.env)[1]
 #   assign("ck.val",ck.start,penden.env)
 #   assign("ck.val.temp",ck.start,penden.env)
 #   penalty.matrix(penden.env)
 #   liste <- matrix(0,1,4+DD+p)
 #   lam <- coef <- c()
 #   for(i in 1:p) lam[i] <- paste("lambda.",i,sep="")
 #   for(j in 1:DD) coef[j] <- paste("b.",j,sep="")
 #   colnames(liste) <- c("pen.log.like","log.like","marg.log.like",lam,"cAIC",coef)
 #   help.str <- paste("d=",get("d",penden.env),"D=",get("D",penden.env),"lambda=",get("lambda",penden.env)[1],sep="")
 #   assign("help.str",help.str,penden.env)
 #   assign("liste",liste,penden.env)
 #   assign("i",i <- 1,penden.env)
 #   print(paste("new.lambda=",get("lambda",penden.env)[1],sep=""))
 #   assign("lambda.crash",FALSE,penden.env)
 # }
 # if(!l.c) {
 #    if((get("lambda",penden.env)[1]!=lambda1)&!lambda.change) calc77<-FALSE
 #    if((get("lambda",penden.env)[1]==lambda1)&lambda.change) {
 #      if(down) assign("lambda",rep(get("lambda",penden.env)[1],p)*0.9,penden.env)
 #      if(up) assign("lambda",rep(get("lambda",penden.env)[1],p)*1.1,penden.env)
 #      lambda1<-get("lambda",penden.env)[1]
 #      assign("ck.val",ck.start,penden.env)
 #      assign("ck.val.temp",ck.start,penden.env)
 #      print(paste("new.lambda=",get("lambda",penden.env)[1],sep=""))
 #      penalty.matrix(penden.env)
 #      liste <- matrix(0,1,4+DD+p)
 #      lam <- coef <- c()
 #      for(i in 1:p) lam[i] <- paste("lambda.",i,sep="")
 #      for(j in 1:DD) coef[j] <- paste("b.",j,sep="")
 #      colnames(liste) <- c("pen.log.like","log.like","marg.log.like",lam,"cAIC",coef)
 #      help.str <- paste("d=",get("d",penden.env),"D=",get("D",penden.env),"lambda=",get("lambda",penden.env)[1],sep="")
 #      assign("help.str",help.str,penden.env)
 #      assign("liste",liste,penden.env)
 #      assign("i",i <- 1,penden.env)
 #      print(paste("new.lambda=",get("lambda",penden.env)[1],sep=""))
 #      lambda.change<-FALSE
 #    } 
 # }
 #if((get("lambda.crash",penden.env)|lambda.change)&get("lambda",penden.env)[1]<0.1) {
 #  print("geht nicht")
 #  break
 #  #assign("lambda",rep(10*lambda.sa,p),penden.env)
 #  #lambda1<-lambda.sa*10
 #}
 #hh <- hh+1
 #if(hh>60&hh<120) {
 #  down<-FALSE
 #  up<-TRUE
 #}
 #if(hh>=120&hh<180) {
 #  down<-TRUE
 #  up<-FALSE
 #}
 #if(hh>=240&hh<300) {
 #  down<-FALSE
 #  up<-TRUE
 #}
 #if(hh>=300) {
 #  print("geht nicht")
 #  break
 #}
 #}
}
