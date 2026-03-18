new.weights <- function(penden.env,start=FALSE) {
  #print("start new Weights")
  dd <- get("dd",penden.env)
  p <- get("p",penden.env)
  DD <- get("DD",penden.env)
  calc <- TRUE
  prob.val2<-1
  ind.val<-get("ind.val",penden.env)
  assign("help.lambda2",NULL,penden.env)
  fix.lambda<-get("fix.lambda",penden.env)
  if(!get("cond",penden.env)) {
    l.A <- length(get("A.Restrict",penden.env)[,1,1])
    vec <- seq(1,(l.A-1))
    assign("AA.help",t(get("A.Restrict",penden.env)[vec,,1]),penden.env)  
    if(!is.null(ind.val)) assign("AA.help",t(get("A.Restrict",penden.env)[vec,,1][,-ind.val]),penden.env)  
    for(j in 2:p) {
      if(is.null(ind.val)) assign("AA.help",cbind(get("AA.help",penden.env),t(get("A.Restrict",penden.env)[vec,,j])),penden.env)
      if(!is.null(ind.val)) assign("AA.help",cbind(get("AA.help",penden.env),t(get("A.Restrict",penden.env)[vec,,j][,-ind.val])),penden.env)
    }
    #browser()
    no.cond<-p*length(vec)
    meq<-1+no.cond
  }
  if(get("cond",penden.env)&p==3) {
    if(is.null(get("vec2",penden.env))) {
      if(get("d",penden.env)==3|get("d",penden.env)==2) vec2 <- seq(1,(dim(get("A2.Restrict",penden.env))[1]-get("dd",penden.env)))
      if(get("d",penden.env)==4|get("d",penden.env)==5) vec2 <- seq(1,(dim(get("A2.Restrict",penden.env))[1]-get("d",penden.env))) 
    }
    if(!is.null(get("vec2",penden.env))) vec2<-get("vec2",penden.env)
    assign("AA.help",t(get("A2.Restrict",penden.env)[vec2,,1]),penden.env)
    #browser()
    if(get("d",penden.env)==3|get("d",penden.env)==2) vec2 <- seq(1,(dim(get("A2.Restrict",penden.env))[1]-1))
    if(get("d",penden.env)==4|get("d",penden.env)==5) vec2 <- seq(get("dd",penden.env)-1,(dim(get("A2.Restrict",penden.env))[1]))
    assign("AA.help",cbind(get("AA.help",penden.env),t(get("A2.Restrict",penden.env)[vec2,,2])),penden.env)
    no.cond<-dim(get("AA.help",penden.env))[2]
    meq<-1+no.cond
  }
  if(get("cond",penden.env)&p==4) {
    if(is.null(get("vec2",penden.env))) {
       if(get("d",penden.env)==2)  {
          vec2.1 <- seq(1,(dim(get("A2.Restrict",penden.env))[1]-1))
          vec2.2 <- seq(1,(dim(get("A2.Restrict",penden.env))[1]-6*get("dd",penden.env)+5))
       }
       if(get("d",penden.env)==3)  {
          vec2.1 <- seq(6*get("dd",penden.env)+1,(dim(get("A2.Restrict",penden.env))[1]))
          vec2.2 <- seq(1,(dim(get("A2.Restrict",penden.env))[1]-4*get("dd",penden.env)))
       }
       if(get("d",penden.env)>3) vec2.1<-vec2.2<-get("vec2",penden.env)
    }
    assign("AA.help",t(get("A2.Restrict",penden.env)[vec2.1,,1]),penden.env)
    assign("AA.help",cbind(get("AA.help",penden.env),t(get("A2.Restrict",penden.env)[vec2.2,,2])),penden.env)
    no.cond<-dim(get("AA.help",penden.env))[2]
    meq<-1+no.cond
  }

  assign("Amat",cbind(matrix(1,DD,1),get("AA.help",penden.env),t(get("tilde.PSI.d.D.knots.start.g.all",penden.env))),penden.env)

  if(fix.lambda) {
      Derv1(penden.env,temp.ck=TRUE)
      Derv2(penden.env,temp.ck=TRUE)
  }
  else {
      Derv1(penden.env,temp.lam=TRUE)
      Derv2(penden.env,temp.lam=TRUE)
  }
    bvec <- c(rep(0,1+no.cond),-get("tilde.PSI.d.D.knots.start.g.all",penden.env)%*%get("ck.val",penden.env))
    #sc <- (base:::norm(-get("Derv2.pen.temp",penden.env),"2"))
    sc <- (norm(-get("Derv2.pen.temp",penden.env),"2"))
    Derv2.pen.help <- -get("Derv2.pen.temp",penden.env)/sc
    Derv1.help<- get("Derv1.pen.temp",penden.env)/sc
    assign("aa",try(solve.QP(Dmat=Derv2.pen.help,dvec=Derv1.help,Amat=get("Amat",penden.env),bvec=bvec,meq=meq,factorized=FALSE)$solution,silent=TRUE),penden.env)
   
 if(class(get("aa",penden.env))=="try-error") {
   Derv2.pen.help <- -get("Derv2.pen.temp",penden.env)
   Derv1.help<- get("Derv1.pen.temp",penden.env)
   assign("aa",try(solve.QP(Dmat=Derv2.pen.help,dvec=Derv1.help,Amat=get("Amat",penden.env),bvec=bvec,meq=meq,factorized=FALSE)$solution,silent=FALSE),penden.env)
 }


if(get("cond",penden.env)&get("d",penden.env)==3&p==4) browser()

  if((class(get("aa",penden.env))=="try-error")&!start|all(is.na(get("aa",penden.env)))) {
     if(any(get("Derv1.pen.temp",penden.env)<0)) {
       print("AB")
       lam<-get("lambda.temp",penden.env)[1]
       lam.old<-get("lambda",penden.env)[1]
       #c.vec<-sort(seq(lam.old/lam,lam/lam.old,length=7))
       if(lam.old>lam) c.vec<-seq(1,lam.old/lam,length=7)[-1]
       if(lam>lam.old) c.vec<-seq(1,lam/lam.old,length=7)[-1]
       print(paste("lam=",lam,".lam.old=",lam.old,".cvec=",c.vec,sep=""))
       ii<-1
       res1<-foreach(ii=length(c.vec):1,.combine=rbind) %do% {
           print(ii)
           assign("lambda.temp",lam*c.vec[ii],penden.env)
           Derv1(penden.env,temp.lam=TRUE)
           Derv2(penden.env,temp.lam=TRUE)
           sc <- (norm(-get("Derv2.pen.temp",penden.env),"2"))
	   Derv2.pen.help <- -get("Derv2.pen.temp",penden.env)/sc#+diag(1e-4,get("DD",penden.env))
           Derv1.help<- get("Derv1.pen.temp",penden.env)/sc
           assign("aa",try(solve.QP(Dmat=Derv2.pen.help,dvec=Derv1.help,Amat=get("Amat",penden.env),bvec=bvec,meq=meq,factorized=FALSE)$solution,silent=FALSE),penden.env)
           if(class(get("aa",penden.env))=="try-error") vali<-c(c.vec[ii],FALSE)
           else vali<-c(c.vec[ii],TRUE)
           vali
       }
       if(length(res1[res1[,2]==1,])==0) {
         assign("calc2",FALSE,penden.env)
         return("fehler")
       }
       if(length(res1[res1[,2]==1,])>0) {
         if(lam>lam.old) lambda.temp<-max(res1[res1[,2]==1,][,1])*lam
         if(lam<lam.old) lambda.temp<-min(res1[res1[,2]==1,][,1])*lam
         assign("lambda.temp",rep(lambda.temp,get("p",penden.env)),penden.env)
         Derv1(penden.env,temp.lam=TRUE)
         Derv2(penden.env,temp.lam=TRUE)
         sc <- (norm(-get("Derv2.pen.temp",penden.env),"2"))
	 Derv2.pen.help <- -get("Derv2.pen.temp",penden.env)/sc#+diag(1e-4,get("DD",penden.env))
         Derv1.help<- get("Derv1.pen.temp",penden.env)/sc
         assign("aa",try(solve.QP(Dmat=Derv2.pen.help,dvec=Derv1.help,Amat=get("Amat",penden.env),bvec=bvec,meq=meq,factorized=FALSE)$solution,silent=FALSE),penden.env)
      }
    }
    else {
       print("fehler")
       assign("calc2",FALSE,penden.env)
       return("fehler")
    }
  }

rm("Amat",envir=penden.env)
rm(bvec)
  if(class(get("aa",penden.env))=="try-error") {
       print("fehler")
       assign("calc2",FALSE,penden.env)
       return("fehler")
  }
  else assign("ck.val.temp",get("ck.val",penden.env)+get("aa",penden.env),penden.env)
  #if(!is.null(ind.val)) {
  #  ck.val<-get("ck.val",penden.env)
  #  ind.seq<-seq(1,length(ck.val))
  #  ck.val[ind.seq[-ind.val]]<-ck.val[-ind.val]-get("aa",penden.env)
  #  ck.val[ck.val<=0]<-0
  #  ind<-which(ck.val<=0)
  #  assign("ind.val",ind,envir=penden.env) 
  #  assign("ck.val.temp",ck.val,penden.env)
  #}

  #if(any(get("ck.val.temp",penden.env)<=0)&is.null(ind.val)) {
  #   ck.val.temp<-get("ck.val.temp",penden.env)
  #   ind<-which(ck.val.temp<=0)
  #   ck.val.temp[ind]<-0
  #   assign("ck.val.temp",ck.val.temp,penden.env)
  #   assign("ind.val",ind,envir=penden.env) 
  #}
  #assign("ck.val.temp",get("ck.val",penden.env)+get("aa",penden.env),penden.env)
    if(fix.lambda) {
        Derv1(penden.env,temp.ck=TRUE)
        Derv2(penden.env,temp.ck=TRUE)
      }
      else {
        Derv1(penden.env,temp.lam=TRUE)
        Derv2(penden.env,temp.lam=TRUE)
      }
  rm("aa",envir=penden.env)
  f.hat.val(penden.env,temp=TRUE)
  #if(get("no",penden.env)) {
  #  print(paste(get("id",penden.env)," is no error",sep=""))
  #  return("fehler")
  #}
  pen.log.like(penden.env,temp.ck=TRUE)
  return("keinFehler")
}
