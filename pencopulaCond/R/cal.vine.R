cal.vine <- function(obj,val,cores) {
  vine<-obj$Dvine
  registerDoParallel(cores=cores)
  d <- obj$d
  d2<-obj$d2 
  D <- obj$D
  D3<-obj$D3
  S <- obj$S
  N <- obj$N
  base <- obj$base
  D.struc<-obj$D.struc
  q<-obj$q
  level <-  2
  Tree.l <- vine[[level]]
  cond.arg<-obj$cond.arg

log.like <- AIC <- 0

  Tree.l.temp <- foreach(i=1:length(Tree.l),.combine=list, .multicombine=TRUE) %do%   {
    index.j1 <- Tree.l[[i]]$j1
    index.j2 <- Tree.l[[i]]$j2
    U.hat <- val[,c(index.j1,index.j2)]
    U <- eval.pencopula(val=U.hat,d=d,D=D,ck.val=Tree.l[[i]]$v,p=2,ddb=Tree.l[[i]]$ddb,q=obj$q)
    ll <- sum(sapply(U[U>0],log))
    list(j1=index.j1,j2=index.j2,D=NULL,v=obj$Dvine[[level]][[i]]$v,U.hat=U.hat,U=U,log.like=ll,cond=obj$Dvine[[level]][[i]]$cond)#,cop=obj$vine[[level]][[i]]$cop) 
  }
  for(j in 1:length(Tree.l.temp)) {
    log.like <- log.like+Tree.l.temp[[j]]$log.like
  }

  vine[[level]] <- Tree.l.temp

  while( length(vine[[level]])>1) # loop over the levels of the Dvine. We stop if the tree in the Dvine has just one knot
  {
    level <-  level+1
    Tree.l <- vine[[level]] # current tree in vine
    Tree.l1 <- vine[[level-1]] # previous tree in vine, one level up
    Tree.l.temp <- foreach(i=1:length(Tree.l),.combine=list, .multicombine=TRUE) %do% {
       U.hat <- c()
       for(j in 1:2)
          {
              {
                index.ancestor <- Tree.l[[i]]$before[j]
              }
            ancestor.knot <- Tree.l1[index.ancestor][[1]] # Ancestor knot of j-th Element in Index set, i.e. knot with indices {j1,D}  # for j=1 and {j2,D} for j=2, respectively.
            if(Tree.l[[i]]$j1%in%c(ancestor.knot$j1,ancestor.knot$j2)) val.j<-Tree.l[[i]]$j1
            if(Tree.l[[i]]$j2%in%c(ancestor.knot$j1,ancestor.knot$j2)) val.j<-Tree.l[[i]]$j2
            if(ancestor.knot$cond) {
               if(dim(ancestor.knot$U.hat)[2]==3) {
                  diff.help<-c(TRUE,TRUE,FALSE)
                  p<-3
               }
               if(dim(ancestor.knot$U.hat)[2]==4) {
                  diff.help<-c(TRUE,TRUE,FALSE,FALSE)
                  p<-4
               }
            }
            if(!ancestor.knot$cond) {
               diff.help<-c(TRUE,TRUE)
               p<-2
            }
            diff.help[!(c(ancestor.knot$j1,ancestor.knot$j2)%in%val.j)]<-FALSE
            if(ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,intp=diff.help,d=d2,D=D3,p=p,q=q))
            if(!ancestor.knot$cond) U.hat <-cbind(U.hat,hierarchbs.cond.cop(data=ancestor.knot$U.hat,coef=ancestor.knot$v,intp=diff.help,d=d,D=D,p=2,q=q))
          } 
        U.hat[U.hat<0]<-0
        U.hat[U.hat>1]<-1
        if(!vine[[level]][[i]]$cond) {
          if(!vine[[level]][[i]]$indep) U <- eval.pencopula(val=U.hat,d=d,D=D,ck.val=Tree.l[[i]]$v,p=2,ddb=Tree.l[[i]]$ddb,q=obj$q)
          if(vine[[level]][[i]]$indep) U <- rep(1,dim(U.hat)[1])
        }
        if(vine[[level]][[i]]$cond) {
           ################ 
           if(length(Tree.l[[i]]$D)<=D.struc) U.hat<-cbind(U.hat,val[,Tree.l[[i]]$D])
           if(length(Tree.l[[i]]$D)>D.struc) {
               #pca<-obj$pca
               #if(pca==1) {
               #  prcomp.D <- vine[[level]][[i]]$prcomp.D
               #  load.pr <- prcomp.D$rotation[,1]
               #  load.pr <- load.pr/sum(load.pr)
               #  data.distr <- round(val[,D.index]%*%load.pr,10)
               #}
               #if(pca==3) {
               #  dat.norm<-val[,Tree.l[[i]]$D]
               #  for(kk in 1:length(Tree.l[[i]]$D)) dat.norm[,kk]<-(dat.norm[,kk]-mean(dat.norm[,kk]))/sd(dat.norm[,kk])
               #  prcomp.D <- vine[[level]][[i]]$prcomp.D
               #  dat2<-dat.norm%*%prcomp.D$rotation
               #  data.distr<-foreach(o=1:D.struc,.combine=cbind) %do% {
               #      round(rank(dat2[,o])/(dim(dat2)[1]+1),10)
               #  }
               # }
               #if(pca==2) {
                 len.D<-length(Tree.l[[i]]$D)
                 if(is.null(cond.arg)) dat<-val[,Tree.l[[i]]$D]
                 if(!is.null(cond.arg)) dat<-cbind(val[,Tree.l[[i]]$D],cond.arg)
                 kk<-o<-1
                 dat.norm<-foreach(kk=1:len.D,.combine=cbind) %do% {
                   rank(pnorm(dat[,kk],0,1))/(dim(dat)[1]+1)
                 }         
                 prcomp.D <- vine[[level]][[i]]$prcomp.D
                 dat2<-dat.norm%*%prcomp.D$rotation
                 data.distr<-foreach(o=1:D.struc,.combine=cbind) %do% {
                     round(rank(dat2[,o])/(dim(dat)[1]+1),10)
                 }
               #}
             U.hat<-cbind(U.hat,data.distr) 
           }
           U <- eval.pencopula(val=U.hat,d=d2,D=D3,ck.val=Tree.l[[i]]$v,p=dim(U.hat)[2],ddb=Tree.l[[i]]$ddb,q=obj$q)
        }
        ll <- sum(sapply(U[U>0],log))
        list(j1=Tree.l[[i]]$j1,j2=Tree.l[[i]]$j2,D=Tree.l[[i]]$D,U.hat=U.hat,U=U,v=obj$Dvine[[level]][[i]]$v,log.like=ll,cond=obj$Dvine[[level]][[i]]$cond)#,cop=obj$vine[[level]][[i]]$cop)
    }
    if(length(vine[[level]])==1)  {
      Tree.l.temp <- list(Tree.l.temp)
    }
    vine[[level]] <- Tree.l.temp
    
    for(i in 1:length(Tree.l.temp)) {
      log.like <- log.like+Tree.l.temp[[i]]$log.like
    }
  }
  U.result <- rep(1,dim(val)[1])
  U.log<-c()
  #browser()
  for(i in 2:length(S))  {
    for(j in 1:length(vine[[i]]))  {
       #ind<-which(vine[[i]][[j]]$U>0)
       U.result <- U.result*vine[[i]][[j]]$U
       U.log<-cbind(U.log,log(vine[[i]][[j]]$U))
    }
  }
  return(list(cal=U.result,log.like=log.like,Dvine=vine,U.log=U.log))
}
