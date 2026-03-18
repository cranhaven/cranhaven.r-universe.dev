order.vine.level <- function(tree,help.env) {

  l.search<-get("lambda.search",help.env)
  id<-get("id",help.env)
  RVM<-get("RVM",help.env)
  len <- length(tree)
  base <- get("base",help.env)
  order.stat <- get("order.stat",help.env)
  pairs.old <- get("pairs.fit",help.env)
  q <- get("q",help.env)
  base <- get("base",help.env)
  val <- unique(c(pairs.old))
  D.struc<-get("D.struc",help.env)
  count <- 1
  pairs.new <- matrix(NA,1,2)
    for(i in 1:length(val)) {
      val.temp <- c()
      for(j in 1:dim(pairs.old)[1]) {
        if(any(pairs.old[j,]%in%val[i])) val.temp <- c(val.temp,j)
      }
      if(length(val.temp)==2) {
        if(count==1) {
          pairs.new[count,] <- val.temp
          count <- count+1
        }
        else {
          pairs.new <- rbind(pairs.new,val.temp)
          count <- count+1
        }
      }
      if(length(val.temp)>2) {
        for(j in 1:(length(val.temp)-1)) {
          for(k in (j+1):length(val.temp)) {
	    if(count==1) {
              pairs.new[count,] <- c(val.temp[j],val.temp[k])
              count <- count+1
            }
            else {
              pairs.new <- rbind(pairs.new,c(val.temp[j],val.temp[k]))
              count <- count+1
            }
          }
        }
      }
    }

  no.pairs <- dim(pairs.new)[1]
  mcoptions <- list(preschedule=FALSE)
  if(!is.null(RVM)) {
    cops<-get("cops",help.env)[[get("level",help.env)]]
    h.help <- foreach(i=1:no.pairs,.combine=rbind,.multicombine=TRUE,.options.multicore=mcoptions) %dopar% {
      UU <- c()
      help.j1 <- c(tree[[pairs.new[i,1]]]$j1,tree[[pairs.new[i,1]]]$j2,tree[[pairs.new[i,1]]]$D)
      help.j2 <- c(tree[[pairs.new[i,2]]]$j1,tree[[pairs.new[i,2]]]$j2,tree[[pairs.new[i,2]]]$D)
      j1 <- help.j1[!help.j1%in%help.j2]
      j2 <- help.j2[!help.j2%in%help.j1]
      D.help <- c(tree[[pairs.new[i,1]]]$D,tree[[pairs.new[i,2]]]$D)
      D <- sort(unique(c(help.j1[help.j1%in%help.j2],D.help)))
      c(j1,j2,D)
    }
    ind<-rep(NA,dim(cops)[1])
    ind2<-c()
    h.help2<-cbind(h.help[,2],h.help[,1],h.help[,-c(1,2)])
    for(k in 1:dim(h.help)[1]) {
      for(ll in 1:dim(cops)[1]) {
        if(identical(h.help[k,],cops[ll,])) ind[ll]<-k
        if(identical(h.help2[k,],cops[ll,])) { 
          ind[ll]<-k
          ind2<-c(ind2,k)
        }
      }
    }
    if(!is.null(ind2)) pairs.new[ind2,]<-c(pairs.new[ind2,2],pairs.new[ind2,1])
    pairs.new<-pairs.new[ind,]
  }

  no.pairs <- dim(pairs.new)[1]
  h1 <- foreach(i=1:no.pairs,.combine=c,.multicombine=TRUE,.options.multicore=mcoptions) %do%{
      UU <- c()
      help.j1 <- c(tree[[pairs.new[i,1]]]$j1,tree[[pairs.new[i,1]]]$j2,tree[[pairs.new[i,1]]]$D)
      help.j2 <- c(tree[[pairs.new[i,2]]]$j1,tree[[pairs.new[i,2]]]$j2,tree[[pairs.new[i,2]]]$D)
      j1 <- help.j1[!help.j1%in%help.j2]
      j2 <- help.j2[!help.j2%in%help.j1]
      D.help <- c(tree[[pairs.new[i,1]]]$D,tree[[pairs.new[i,2]]]$D)
      D.index <- sort(unique(c(help.j1[help.j1%in%help.j2],D.help)))
      len.D<-length(D.index)
      p<-2+len.D 
      index <- list(c(j1,D),c(j2,D)) # list of involved indices in current knot, i.e {j1,D} and {j2,D}
      if(p>(2+D.struc)) p<-2+D.struc
      for(j in 1:2) # Loop over both index sets, i.e. {j1,D} and {j2,D}
        {
          indexi <- c(tree[[pairs.new[i,j]]]$j1,tree[[pairs.new[i,j]]]$j2,tree[[pairs.new[i,j]]]$D)
          index.ancestor <- c()#index.indep<- c()
          for (ml in 1:length(tree))
            {
              index.ancestor <- c(index.ancestor, all(indexi==(c(tree[[ml]]$j1,tree[[ml]]$j2,tree[[ml]]$D))))
            }
          ancestor.knot <- tree[index.ancestor][[1]] # Ancestor knot of j-th Element in Index set, i.e. knot with indices {j1,D} for j=1 and {j2,D} for j=2, respectively. 
          if(ancestor.knot$cond&dim(ancestor.knot$U)[2]==3) diff.help<-c(TRUE,TRUE,FALSE)
          if(ancestor.knot$cond&dim(ancestor.knot$U)[2]==4) diff.help<-c(TRUE,TRUE,FALSE,FALSE)
          if(!ancestor.knot$cond) diff.help<-c(TRUE,TRUE)
          if(j==1) {
            diff.help[!(c(tree[[pairs.new[i,1]]]$j1,tree[[pairs.new[i,1]]]$j2)%in%j1)]<-FALSE
          }
          if(j==2) {
            diff.help[!(c(tree[[pairs.new[i,2]]]$j1,tree[[pairs.new[i,2]]]$j2)%in%j2)]<-FALSE
          }
          if(j==1&!ancestor.knot$cond) UU <-cbind(UU,hierarchbs.cond.cop(data=ancestor.knot$U,coef=ancestor.knot$v,intp=diff.help,d=get("d",help.env),D=get("D",help.env),p=2,q=q))
          if(j==1&ancestor.knot$cond) UU <-cbind(UU,hierarchbs.cond.cop(data=ancestor.knot$U,coef=ancestor.knot$v,intp=diff.help,d=get("d2",help.env),D=get("D3",help.env),p=dim(ancestor.knot$U)[2],q=q))
          if(j==2&!ancestor.knot$cond) UU <-cbind(UU,hierarchbs.cond.cop(data=ancestor.knot$U,coef=ancestor.knot$v,intp=diff.help,d=get("d",help.env),D=get("D",help.env),p=2,q=q))
          if(j==2&ancestor.knot$cond) UU <-cbind(UU,hierarchbs.cond.cop(data=ancestor.knot$U,coef=ancestor.knot$v,intp=diff.help,d=get("d2",help.env),D=get("D3",help.env),p=dim(ancestor.knot$U)[2],q=q))
      }
      if(any(UU>1)) UU[which(UU>1)] <-1
      if(any(UU<0)) UU[which(UU<0)] <-0

  ##################################   
      if(!get("mod.cond",help.env)) {
         if(l.search) model.l<-lam.search(data=UU,d=get("d",help.env),D=get("D",help.env),lam=get("lam1.vec",help.env),m=get("m",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),cond=FALSE,id=get("id",help.env),l.lam=2,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
         if(!l.search) {
         model.l<-pencopula(data=UU,d=get("d",help.env),D=get("D",help.env),pen.order=get("m",help.env),base="B-spline",lambda=rep(get("lambda",help.env)[1],2),max.iter=get("max.iter",help.env),q=get("q",help.env),id=get("id",help.env),cond=FALSE,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
         }
         assign("indep",FALSE,model.l)
         cond<-FALSE
         prcomp.D<-NULL
      }
      if(get("mod.cond",help.env)) {
         print(paste("len.D=",len.D,".j1=",j1,".j2=",j2,".D=",D.index,sep=""))
         l.lam<-2+len.D
         if(l.lam>(2+D.struc)) l.lam<-2+D.struc
         if(len.D<=D.struc) {
           if(!get("cal.cond",help.env)) {
             if(get("test.cond",help.env)==1) pacotestOptions=pacotestset(testType='VI')
             if(get("test.cond",help.env)==2) pacotestOptions=pacotestset(testType='ECORR')
             if(get("test.cond",help.env)==2|get("test.cond",help.env)==1) test.res<-pacotest(UU,get("data",help.env)[,D.index],pacotestOptions)$pValue
             if(is.null(get("test.cond",help.env))) test.res<-0.051
             if(test.res<0.05) {
               UU<-cbind(UU,get("data",help.env)[,D.index])
               colnames(UU)<-NULL
               cond<-TRUE
               prcomp.D<-NULL              
               if(l.search) model.l<-lam.search(data=UU,d=get("d2",help.env),D=get("D3",help.env),lam=get("lam2.vec",help.env),m=get("m",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),cond=TRUE,id=get("id",help.env),l.lam=l.lam,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
               if(!l.search) model.l<-pencopula(data=UU,d=get("d2",help.env),D=get("D3",help.env),pen.order=get("m",help.env),base="B-spline",lambda=rep(get("lambda",help.env)[2],l.lam),max.iter=get("max.iter",help.env),cond=TRUE,q=get("q",help.env),id=get("id",help.env),fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
               assign("indep",FALSE,model.l)
             }
             else {
               prcomp.D<-NULL
               cond <- FALSE
               if(l.search) model.l<-lam.search(data=UU,d=get("d",help.env),D=get("D",help.env),lam=get("lam1.vec",help.env),m=get("m",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),cond=FALSE,id=get("id",help.env),l.lam=2,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
               if(!l.search) model.l<-pencopula(data=UU,d=get("d",help.env),D=get("D",help.env),pen.order=get("m",help.env),base="B-spline",lambda=rep(get("lambda",help.env)[1],2),max.iter=get("max.iter",help.env),cond=FALSE,q=get("q",help.env),id=id,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
             }
           }
           if(get("cal.cond",help.env)) {
             UU<-cbind(UU,get("data",help.env)[,D.index])
             prcomp.D<-NULL
             l.lam<-2+len.D
             if(l.lam>(2+D.struc)) l.lam<-2+D.struc
             cond<-TRUE
             if(l.search) model.l<-lam.search(data=UU,d=get("d2",help.env),D=get("D3",help.env),lam=get("lam2.vec",help.env),m=get("m",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),cond=TRUE,id=get("id",help.env),l.lam=l.lam,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
             if(!l.search) model.l<-pencopula(data=UU,d=get("d2",help.env),D=get("D3",help.env),pen.order=get("m",help.env),base="B-spline",lambda=rep(get("lambda",help.env)[2],l.lam),max.iter=get("max.iter",help.env),cond=TRUE,q=get("q",help.env),id=get("id",help.env),fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
             assign("indep",FALSE,model.l)
           }
         }
         if(len.D>D.struc) {          
            if(!get("cal.cond",help.env)) {
               if(get("test.cond",help.env)==1) pacotestOptions=pacotestset(testType='VI')
               if(get("test.cond",help.env)==2) pacotestOptions=pacotestset(testType='ECORR')
               if(get("test.cond",help.env)==2|get("test.cond",help.env)==1) test.res<-pacotest(UU,get("data",help.env)[,D.index],pacotestOptions)$pValue
               if(is.null(get("test.cond",help.env))) test.res<-0.051
               if(test.res<0.05) {
                 pca.temp<-cal.pca(help.env,val=get("data",help.env)[,D.index])
                 prcomp.D<-pca.temp$prcomp.D
                 UU<-cbind(UU,pca.temp$data.distr)
                 colnames(UU) <- NULL
                 cond<-TRUE
                 if(l.search) model.l<-lam.search(data=UU,d=get("d2",help.env),D=get("D3",help.env),lam=get("lam2.vec",help.env),m=get("m",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),cond=TRUE,id=get("id",help.env),l.lam=l.lam,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
                 if(!l.search) model.l<-pencopula(data=UU,d=get("d2",help.env),D=get("D3",help.env),pen.order=get("m",help.env),base="B-spline",lambda=rep(get("lambda",help.env)[2],l.lam),max.iter=get("max.iter",help.env),cond=TRUE,q=get("q",help.env),id=get("id",help.env),fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
               }
               else {
                 prcomp.D<-NULL
                 cond <- FALSE
                 if(l.search) model.l<-lam.search(data=UU,d=get("d",help.env),D=get("D",help.env),lam=get("lam1.vec",help.env),m=get("m",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),cond=FALSE,id=get("id",help.env),l.lam=2,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
                 if(!l.search) model.l<-pencopula(data=UU,d=get("d",help.env),D=get("D",help.env),pen.order=get("m",help.env),base="B-spline",lambda=rep(get("lambda",help.env)[1],2),max.iter=get("max.iter",help.env),cond=FALSE,q=get("q",help.env),id=id,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
               }
            }
            if(get("cal.cond",help.env)) {
               pca.temp<-cal.pca(help.env,val=get("data",help.env)[,D.index])
               UU<-cbind(UU,pca.temp$data.distr)
               colnames(UU) <- c()
               cond<-TRUE
               prcomp.D<-pca.temp$prcomp.D
               if(l.search) model.l<-lam.search(data=UU,d=get("d2",help.env),D=get("D3",help.env),lam=get("lam2.vec",help.env),m=get("m",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),cond=TRUE,id=get("id",help.env),l.lam=l.lam,fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
               if(!l.search) model.l<-pencopula(data=UU,d=get("d2",help.env),D=get("D3",help.env),pen.order=get("m",help.env),base="B-spline",lambda=rep(get("lambda",help.env)[2],l.lam),max.iter=get("max.iter",help.env),cond=TRUE,q=get("q",help.env),id=get("id",help.env),fix.lambda=get("fix.lambda",help.env),test.ind=get("test.ind",help.env))
               assign("indep",FALSE,model.l)
            }
         }
      }
      assign("prcomp.D",prcomp.D,model.l)
      model.l    
  }

if(dim(pairs.new)[1]==1) {
   assign("pairs.fit",pairs.new,help.env)
   assign("pairs.new",pairs.new,help.env)
   assign(paste("fit.level",get("level",help.env),sep=""),h1,help.env)
} 
if(dim(pairs.new)[1]>1) {
  if(is.null(RVM)) {
    h <- foreach(i=1:no.pairs,.combine=rbind) %dopar% {
      c(pairs.new[i,],get(get("order.stat",help.env),h1[[i]]))
    }
    colnames(h) <- c("i","j","log.like")
    mat <- matrix(0,len,len)
    diag(mat) <- rep(0,len)

    for(i in 1:dim(pairs.new)[1]) {
      mat[pairs.new[i,1],pairs.new[i,2]] <- mat[pairs.new[i,2],pairs.new[i,1]] <- h[which(h[,1]==pairs.new[i,1] & h[,2]==pairs.new[i,2]),3]
    }
  }
  assign("pairs.new",pairs.new,help.env)
  assign(paste("fit.level",get("level",help.env),sep=""),h1,help.env)
  if(is.null(RVM)) {
    obj <- minimum.spanning.tree(graph.adjacency(mat,diag=FALSE,mode="lower",weighted=TRUE),algorithm="prim")
    pairs.fit <- get.edgelist(obj, names=TRUE)
    pairs.fit <- pairs.fit[order(pairs.fit[,1]),]
  }
  else pairs.fit<-pairs.new
  assign("pairs.fit",pairs.fit,help.env)
}
}
