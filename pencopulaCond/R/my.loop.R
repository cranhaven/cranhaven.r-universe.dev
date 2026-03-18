my.loop <- function(penden.env) {
  DD <- get("DD",penden.env)
  eps<-0.0125
  n.liste <- matrix(0,1,dim(get("liste",penden.env))[2])
  assign("i",i <- 2,penden.env)
  max.iter <- get("max.iter",penden.env)
  p <- get("p",penden.env)
  fix.lambda <- get("fix.lambda",penden.env)
  assign("lambda.crash",FALSE,penden.env)
  assign("lambda.hold",FALSE,penden.env)
  assign("calc2",TRUE,penden.env)
  assign("errorDerv2",FALSE,penden.env)
  if(fix.lambda) my.IC(penden.env,temp=TRUE)
  while(get("calc",penden.env)) {
    if(!get("calc2",penden.env)) {
      assign("calc",FALSE,penden.env)
      penalty.matrix(penden.env)
      f.hat.val(penden.env,cal=TRUE)
      if(get("no",penden.env)) break
      pen.log.like(penden.env,cal=TRUE)
      Derv1(penden.env)
      Derv2(penden.env)
      if(get("errorDerv2",penden.env)) break
      marg.likelihood(penden.env,get("pen.log.like",penden.env))
      my.IC(penden.env)
      n.liste <- c(get("pen.log.like",penden.env),get("log.like",penden.env),get("marg.log.like",penden.env),get("lambda",penden.env),get("cAIC",penden.env),get("ck.val",penden.env))
      list <- rbind(get("liste",penden.env),n.liste)
      rownames(list) <- seq(0,(i-1))
      assign("liste",list,penden.env)
      break
    }
    old.ck <- get("ck.val",penden.env)
    if(i>1) {
      assign("f.hat.val",get("f.hat.val.temp",penden.env),penden.env)
      assign("last.ck.val",get("ck.val",penden.env),penden.env)
      assign("ck.val",get("ck.val.temp",penden.env),penden.env)   
      ck.v<-round(get("ck.val",penden.env),6)[1:7]
      #print(cbind(i,matrix(ck.v,nrow=1)))
      assign("log.like",get("log.like.temp",penden.env),penden.env)
      assign("pen.log.like",get("pen.log.like.temp",penden.env),penden.env)
    }
    if(i>1&!fix.lambda) {
        assign("lambda.old",get("lambda",penden.env),penden.env)
        assign("lambda",get("lambda.temp",penden.env),penden.env) 
    }
    if(!fix.lambda) {
    help.lambda <- new.lambda(penden.env) 
      if(get("lambda.crash",penden.env)) break
      if(((abs(help.lambda/get("lambda",penden.env)[1]-1)<eps)&!get("lambda.hold",penden.env)|(i-1)>max.iter)){#|(i>3&(get("cAIC.old",penden.env)<get("cAIC.temp",penden.env)))){#|get("lambda.out",penden.env))) {
        if(get("cond",penden.env)) print(paste("out ",i,sep=""))
        assign("calc",FALSE,penden.env)
        f.hat.val(penden.env,cal=TRUE)
        pen.log.like(penden.env,cal=TRUE)
        Derv1(penden.env)
        Derv2(penden.env)
        if(get("errorDerv2",penden.env)){
          assign("ck.val",get("last.ck.val",penden.env),penden.env)
          assign("lambda",get("lambda.old",penden.env),penden.env)
          f.hat.val(penden.env,cal=TRUE)
          pen.log.like(penden.env,cal=TRUE)
          Derv1(penden.env)
          Derv2(penden.env)
          marg.likelihood(penden.env,get("pen.log.like",penden.env))
          my.IC(penden.env)
          assign("calc",FALSE,penden.env)
        }
        else {
          marg.likelihood(penden.env,get("pen.log.like",penden.env))
          my.IC(penden.env)
          n.liste <- c(get("pen.log.like",penden.env),get("log.like",penden.env),get("marg.log.like",penden.env),get("lambda",penden.env),get("cAIC",penden.env),get("ck.val",penden.env))
          list <- rbind(get("liste",penden.env),n.liste)
          rownames(list) <- seq(0,i)
          assign("liste",list,penden.env)
          assign("calc2",FALSE,penden.env)
        }
      }
      else {
        if(get("no",penden.env)) break
        assign("lambda.temp",rep(help.lambda,p),penden.env)
        pen.log.like(penden.env,temp.ck=TRUE)
        #if(is.infinite(get("pen.log.like",penden.env))) browser()
        #print(paste("log.like step ",i-1," equals ",get("log.like",penden.env),sep=""))
        Derv1(penden.env,temp.lam=TRUE)
        Derv2(penden.env,temp.lam=TRUE)
        if(get("errorDerv2",penden.env)) {
          assign("lambda",get("lambda.old",penden.env),penden.env)
          assign("ck.val",get("last.ck.val",penden.env),penden.env)
          f.hat.val(penden.env,cal=TRUE)
          pen.log.like(penden.env,cal=TRUE)
          Derv1(penden.env)
          Derv2(penden.env)
          marg.likelihood(penden.env,get("pen.log.like",penden.env))
          my.IC(penden.env)
          assign("calc",FALSE,penden.env)
        }
        else {
          marg.likelihood(penden.env,get("pen.log.like",penden.env),temp=TRUE)
          assign("cAIC.old",get("cAIC.temp",penden.env),penden.env)
          my.IC(penden.env,temp=TRUE)
          if(new.weights(penden.env)=="fehler") {
            assign("ck.val",get("last.ck.val",penden.env),penden.env)
            f.hat.val(penden.env,cal=TRUE)
            pen.log.like(penden.env,cal=TRUE)
            Derv1(penden.env)
            Derv2(penden.env)
            marg.likelihood(penden.env,get("pen.log.like",penden.env))
            my.IC(penden.env)
            assign("calc",FALSE,penden.env)
            break
          }
        }
        n.liste <- c(get("pen.log.like.temp",penden.env),get("log.like.temp",penden.env),get("marg.log.like.temp",penden.env),get("lambda.temp",penden.env),get("cAIC.temp",penden.env),get("ck.val",penden.env))
        list <- rbind(get("liste",penden.env),n.liste)
        rownames(list) <- seq(0,i)
        assign("liste",list,penden.env) 
      }
  }
  if(fix.lambda) {
   if(all(abs(get("ck.val.temp",penden.env)/old.ck-1)<0.001)&(i>2)| i-1>max.iter){#|(i>3)&get("cAIC.old",penden.env)<=get("cAIC.temp",penden.env)) {
        assign("calc",FALSE,penden.env)
        f.hat.val(penden.env,cal=TRUE)
        if(get("no",penden.env)) break
        pen.log.like(penden.env,cal=TRUE)
        Derv1(penden.env)
        Derv2(penden.env)
        marg.likelihood(penden.env,get("pen.log.like",penden.env))
        my.IC(penden.env)
        n.liste <- c(get("pen.log.like",penden.env),get("log.like",penden.env),get("marg.log.like",penden.env),get("lambda",penden.env),get("cAIC",penden.env),get("ck.val",penden.env))
        list <- rbind(get("liste",penden.env),n.liste)
        rownames(list) <- seq(0,i)
        assign("liste",list,penden.env)
        break
      }
      else {
        if(get("no",penden.env)) break
        pen.log.like(penden.env,temp.ck=TRUE)
        Derv1(penden.env,temp.ck=TRUE)
        Derv2(penden.env,temp.ck=TRUE)
        marg.likelihood(penden.env,get("pen.log.like.temp",penden.env),temp=TRUE)
        assign("cAIC.old",get("cAIC.temp",penden.env),penden.env)
        my.IC(penden.env,temp=TRUE)
        n.liste <- c(get("pen.log.like.temp",penden.env),get("log.like.temp",penden.env),get("marg.log.like.temp",penden.env),get("lambda",penden.env),get("cAIC.temp",penden.env),get("ck.val",penden.env))
        list <- rbind(get("liste",penden.env),n.liste)
        rownames(list) <- seq(0,i)
        assign("liste",list,penden.env)
        new.weights(penden.env)
      }
  }
assign("i",i <- i+1,penden.env)
  }
}
