cal.Dvine <- function(obj,val) {
  #if(!class(obj$Dvine)=="penDvine") stop("obj$Dvine is not from class penDvine")
  Dvine.save <- obj$Dvine
  l.D <- length(Dvine.save)
  D.struc<-obj$D.struc
  cond.arg<-obj$cond.arg
  q <- obj$q
  d <- obj$d
  d2<-obj$d2
  D <- obj$D
  D3<-obj$D3
  S<-obj$S
  pca<-obj$pca             
  Dvine <- list()
  ord1<-obj$order
  val<-val[,ord1]
  #org.data<-org.data[,ord1]
  SSi <-  list()
  for ( i in 1:length(S))
    {
      vine.knot <-  list(j1=S[i],j2=NULL,D=NULL, v=NULL,U=val[,S[i]] )
      SSi <- append(SSi, list(vine.knot))
    }
  Dvine <- append(Dvine, list(SSi))

  level <-  1
  SS <-  Dvine[[level]]
  nSS <- length(SS)
  while (nSS>1)
    {
      SSi <-  list()
      for (i in 2:nSS)
        {
          S1 <- c(SS[[i-1]]$j1, SS[[i-1]]$j2,SS[[i-1]]$D)
          S2 <- c(SS[[i]]$j1, SS[[i]]$j2,SS[[i]]$D)
          index1 <- rep(TRUE,length(S1))
          index2 <- rep(TRUE,length(S2))
          S3 <- c()
          for (j in 1:length(S1))
            {
              indexi <- S1[j]==S2
              if (sum(indexi)>0)
                {
                  S3 <- c(S3, S1[j])  
                  index1[j] <- FALSE
                  index2[indexi] <- FALSE                
                 }
            }
          if(!is.null(S3)) S3 <- sort(S3)
          S1 <- S1[index1]
          S2 <- S2[index2]
          vine.knot <-  list(j1=S1,j2=S2,D=S3,U=NULL,cond=Dvine.save[[l.D-nSS+2]][[i-1]]$cond)
          SSi <- append(SSi, list(vine.knot))
        }
      Dvine <- append(Dvine, list(SSi))
      level <- level + 1
    SS <-  Dvine[[level]]
      nSS <- length(SS)
    }
  
  level <-  2
  Tree.l <- Dvine[[level]]
 
AIC <- 0
log.like.vec<-c()

#if(doMC){
  Tree.l.temp <- foreach(i=1:length(Tree.l),.combine=list, .multicombine=TRUE) %do%   {
    p<-2
    cond.val<-FALSE
    index.j1 <- Tree.l[[i]]$j1
    index.j2 <- Tree.l[[i]]$j2
    if(!obj$Dvine[[2]][[i]]$rev) U.hat <- val[,c(index.j1,index.j2)] else U.hat<-val[,c(index.j2,index.j1)]
    if(!is.null(obj$cond.arg)&Dvine.save[[level]][[i]]$cond) {
        U.hat<-cbind(U.hat,obj$cond.arg) 
        cond.val<-TRUE
        p<-3
    }
    if(!is.null(obj$cond.arg)&Dvine.save[[level]][[i]]$cond) U <- hierarchbs.cond.cop(data=U.hat,coef=Dvine.save[[level]][[i]]$v,intp=rep(FALSE,p),d=d,D=D3,p=p,cond=cond.val,q=q) #evaluierte Dichte
    else U <- hierarchbs.cond.cop(data=U.hat,coef=Dvine.save[[level]][[i]]$v,intp=rep(FALSE,p),d=d,D=D,p=p,cond=cond.val,q=q)
    ll <- sum(sapply(U[U>0],log))
    list(j1=index.j1,j2=index.j2,D=NULL,v=Dvine.save[[level]][[i]]$v,U.hat=U.hat,U=U,cop=Dvine.save[[level]][[i]]$cop,log.like=ll,p=p,cond=cond.val,rev=obj$Dvine[[2]][[i]]$rev) 
  }
#}
  if(length(S)>2) {
    for(j in 1:length(Tree.l.temp)) {
      log.like.vec<-c(log.like.vec,Tree.l.temp[[j]]$log.like)
    }
    Dvine[[level]] <- Tree.l.temp
  }
  else {
    log.like.vec<-Tree.l.temp$log.like
    Dvine[[level]] <- list(Tree.l.temp)
  }
if(length(S)>2) {
  while( length(Dvine[[level]])>2) # loop over the levels of the Dvine. We stop if the tree in the Dvine has just one knot
  {
    level <-  level+1
    print(level)
    Tree.l <- Dvine[[level]] # current tree in vine
    Tree.l1 <- Dvine[[level-1]] # previous tree in vine, one level up
    #if(doMC){
      Tree.l.temp <- foreach(i=1:length(Tree.l),.combine=list, .multicombine=TRUE) %dopar% {
        U.hat <- c()
        index <- list(c(Tree.l[[i]]$j1,Tree.l[[i]]$D),c(Tree.l[[i]]$j2,Tree.l[[i]]$D))
        D.index<-Tree.l[[i]]$D
        len.D<-length(D.index)
        p<-2+len.D
        if(p>(2+D.struc)) p<-2+D.struc
        for(j in 1:2)
            {
              indexi <- index[[j]]
              index.ancestor <- c()
              for (m in 1:length(Tree.l1))
                {
                  index.ancestor <- c(index.ancestor, all( sort(indexi)==sort(c(Tree.l1[[m]]$j1,Tree.l1[[m]]$j2,Tree.l1[[m]]$D))))
                }
              ancestor.knot <- Tree.l1[index.ancestor][[1]]
              ord1<-c(1,2)
              if(level==3&j==1) if(ancestor.knot$rev) ord1<-c(2,1)
              if(j==1&ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,intp=c(TRUE,FALSE,
                    rep(FALSE,dim(ancestor.knot$U.hat)[2]-2)),d=d2,D=D3,p=dim(ancestor.knot$U.hat)[2],cond=ancestor.knot$cond,q=q))
              if(j==1&!ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,intp=c(TRUE,FALSE)[ord1],d=d,D=D,p=2,cond=ancestor.knot$cond,q=q))
              ord1<-c(1,2)
              if(level==3&j==2) if(ancestor.knot$rev) ord1<-c(2,1)
              if(j==2&ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,intp=c(FALSE,TRUE,
                    rep(FALSE,dim(ancestor.knot$U.hat)[2]-2)),d=d2,D=D3,p=dim(ancestor.knot$U.hat)[2],cond=ancestor.knot$cond,q=q))         
              if(j==2&!ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,intp=c(FALSE,TRUE)[ord1],d=d,D=D,p=2,cond=ancestor.knot$cond,q=q))   
        }
        if(Tree.l[[i]]$cond&!Dvine.save[[level]][[i]]$indep) {
          if(len.D<=D.struc) {
            U.hat<-cbind(U.hat,val[,Tree.l[[i]]$D])
            U <- hierarchbs.cond.cop(data=U.hat,coef=Dvine.save[[level]][[i]]$v,intp=rep(FALSE,dim(U.hat)[2]),d=d2,D=D3,p=dim(U.hat)[2],cond=TRUE,q=q)
          }
        if(len.D>D.struc) {
               if(is.null(cond.arg)) dat<-val[,D.index]
               if(!is.null(cond.arg)) dat<-cbind(val[,D.index],cond.arg)
               o<-kk<-1
               dat.norm<-foreach(kk=1:dim(dat)[2],.combine=cbind) %do% {
                  rank(pnorm(dat[,kk],0,1))/(dim(dat)[1]+1)
               }
               prcomp.D <- Dvine.save[[level]][[i]]$prcomp.D
               dat2<-dat.norm%*%prcomp.D$rotation
               data.distr<-foreach(o=1:D.struc,.combine=cbind) %do% {
                 round(rank(dat2[,o])/(dim(dat)[1]+1),10)
               }
            }
            U.hat<-cbind(U.hat,data.distr)
            U <- hierarchbs.cond.cop(data=U.hat,coef=Dvine.save[[level]][[i]]$v,intp=rep(FALSE,dim(U.hat)[2]),d=d2,D=D3,p=dim(U.hat)[2],cond=TRUE,q=q)
          }
        else {     
          p<-2
          U <- hierarchbs.cond.cop(data=U.hat,coef=Dvine.save[[level]][[i]]$v,intp=rep(FALSE,p),d=d,D=D,p=p,cond=FALSE,q=q)
        }
        ll <- sum(sapply(U[U>0],log))
        list(j1=Tree.l[[i]]$j1,j2=Tree.l[[i]]$j2,D=Tree.l[[i]]$D,U.hat=U.hat,U=U,v=Dvine.save[[level]][[i]]$v,cop=Dvine.save[[level]][[i]]$cop,log.like=ll,cond=Tree.l[[i]]$cond,p=p,indep=Dvine.save[[level]][[i]]$indep)
      #}
    } 
   for(j in 1:length(Tree.l.temp)) {
    log.like.vec<-c(log.like.vec,Tree.l.temp[[j]]$log.like)
   }
    if(length(Dvine[[level]])==1)  {
      Tree.l.temp <- list(Tree.l.temp)
    }
    Dvine[[level]] <- Tree.l.temp
    
  }
  level <-  level+1
  Tree.l <- Dvine[[level]] # current tree in vine
  Tree.l1 <- Dvine[[level-1]] # previous tree in vine, one level up
  i <- 1
  D.index<-Tree.l[[1]]$D
  len.D<-length(D.index)
  p<-2+len.D 
  if(p>(2+D.struc)) p<-2+D.struc
  U.hat <- c()
  index <- list(c(Tree.l[[i]]$j1,Tree.l[[i]]$D),c(Tree.l[[i]]$j2,Tree.l[[i]]$D))
  for(j in 1:2)
    {
      indexi <- index[[j]]
      index.ancestor <- c()
      for (m in 1:length(Tree.l1))
        {
          index.ancestor <- c(index.ancestor, all( sort(indexi)==sort(c(Tree.l1[[m]]$j1,Tree.l1[[m]]$j2,Tree.l1[[m]]$D))))
        }
      ord<-c(1,2)
      if(indexi[2]>indexi[1]) ord<-c(2,1)
      ancestor.knot <- Tree.l1[index.ancestor][[1]] # Ancestor knot of j-th Element in Index set, i.e. knot with indices {j1,D}
      if(j==1&ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,
         intp=c(TRUE,FALSE,rep(FALSE,dim(ancestor.knot$U.hat)[2]-2)),d=d2,D=D3,p=dim(ancestor.knot$U.hat)[2],cond=ancestor.knot$cond,q=q))
      if(j==1&!ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,intp=c(TRUE,FALSE),
         d=d,D=D,p=2,cond=ancestor.knot$cond,q=q))
      if(j==2&ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,
         intp=c(FALSE,TRUE,rep(FALSE,dim(ancestor.knot$U.hat)[2]-2)),d=d2,D=D3,p=dim(ancestor.knot$U.hat)[2],cond=ancestor.knot$cond,q=q))         
      if(j==2&!ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,intp=c(FALSE,TRUE),
         d=d,D=D,p=2,cond=ancestor.knot$cond,q=q))
    }
    if(Tree.l[[i]]$cond&!Dvine.save[[level]][[i]]$indep) {
          if(len.D<=D.struc) {
            U.hat<-cbind(U.hat,val[,Tree.l[[i]]$D])
            U <- hierarchbs.cond.cop(data=U.hat,coef=Dvine.save[[level]][[i]]$v,intp=rep(FALSE,dim(U.hat)[2]),d=d2,D=D3,p=p,cond=TRUE,q=q)
          }
          if(len.D>D.struc) {
             if(is.null(cond.arg)) dat<-val[,D.index]
             if(!is.null(cond.arg)) dat<-cbind(data[,D.index],cond.arg)
             o<-kk<-1
             dat.norm<-foreach(kk=1:dim(dat)[2],.combine=cbind) %do% {
               rank(pnorm(dat[,kk],0,1))/(dim(dat)[1]+1)
             }
             prcomp.D <- Dvine.save[[level]][[i]]$prcomp.D
             dat2<-dat.norm%*%prcomp.D$rotation
             data.distr<-foreach(o=1:D.struc,.combine=cbind) %do% {
               round(rank(dat2[,o])/(dim(dat)[1]+1),10)
             }
            }
            U.hat<-cbind(U.hat,data.distr)
            U <- hierarchbs.cond.cop(data=U.hat,coef=Dvine.save[[level]][[i]]$v,intp=rep(FALSE,dim(U.hat)[2]),d=d2,D=D3,p=dim(U.hat)[2],cond=TRUE,q=q)
          }
        else {
          p<-2
          U <- hierarchbs.cond.cop(data=U.hat,coef=Dvine.save[[level]][[i]]$v,intp=rep(FALSE,p),d=d,D=D,p=p,cond=FALSE,q=q)
        }
  }
  ll <- sum(sapply(U[U>0],log))
  log.like.vec<-c(log.like.vec,ll)
  Tree.l.temp <- list(j1=Tree.l[[i]]$j1,j2=Tree.l[[i]]$j2,D=Tree.l[[i]]$D,U=U,v=Dvine.save[[level]][[i]]$v,cop=Dvine.save[[level]][[i]]$cop,log.like=ll)
  Dvine[[level]][[1]] <- Tree.l.temp
  log.like <- sum(log.like.vec)
  U.result <- rep(0,dim(val)[1])
  for(i in 2:length(S))  {
    for(j in 1:length(Dvine[[i]]))  {
      U.result <- U.result+log(Dvine[[i]][[j]]$U)
    }
  }
  return(list(cal=exp(U.result),log.like=log.like,log.like.vec=log.like.vec))
}
