lam.search<-function(data,d,D,lam,m,max.iter,q,cond,id,l.lam,fix.lambda,test.ind) {
    jj<-1
    res<-foreach(jj=1:length(lam),.combine=rbind) %do% {
       cop<- try(pencopula(data=data,d=d,D=D,lambda=rep(lam[jj],l.lam),pen.order=m,base="B-spline",max.iter=max.iter,q=q,cond=cond,id=id,fix.lambda=fix.lambda,test.ind=test.ind))
       save(file=paste("cop.",jj,".id=",id,".Rdata",sep=""),list=c("cop"))
       if(class(cop)!="try-error") val<- c(lam[jj],get("log.like",cop),get("cAIC",cop),get("i",cop))
       else val <- c(lam[jj],0,0,0)
      val
    }
    print(res)
    ind<-which.min(res[,3])
    set<-seq(1,length(lam))[-ind]
    load(paste("cop.",ind,".id=",id,".Rdata",sep=""))
    for(jk in 1:length(lam)) system(paste("rm cop.",jk,".id=",id,".Rdata",sep=""))
    return(cop)
}
