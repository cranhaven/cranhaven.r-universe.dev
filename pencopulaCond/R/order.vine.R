order.vine <- function(help.env,test.ind=FALSE) {
  
  RVM<-get("RVM",help.env)
  len <- get("p",help.env)
  no.pairs <- choose(len,2)
  order.stat <- get("order.stat",help.env)
  pairs.new <- matrix(NA,no.pairs,2)
  count <- 1
  ddb<-get("ddb",help.env)
  if(is.null(RVM)) {
    for(i in 1:(len-1)) {
      for(j in (i+1):len) {
        pairs.new[count,] <- c(i,j)
        count <- count+1
      }
    }
  }
  else {
    pairs.new<-pairs.fit<-get("cops",help.env)[[2]]
    no.pairs<-dim(pairs.new)[1]
  }
  l.search<-get("lambda.search",help.env)
  id<-get("id",help.env)
  fix.lambda<-get("fix.lambda",help.env)
  mcoptions <- list(preschedule=FALSE)
  print(paste("level=2 ,number of pairs=",no.pairs,sep=""))

  h1 <- foreach(i=1:no.pairs,.combine=list,.multicombine=TRUE,.options.multicore=mcoptions) %dopar% {
      if(l.search) {
          lam1.vec<-get("lam1.vec",help.env)
          skip.ind<-c()
          jj<-1
          res<-foreach(jj=1:length(lam1.vec),.combine=rbind) %do% {
            cop<- try(pencopula(data=get("U",help.env)[,pairs.new[i,]],d=get("d",help.env),D=get("D",help.env),lambda=rep(lam1.vec[jj],2),pen.order=get("m",help.env),base=get("base",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),fix.lambda=fix.lambda,test.ind=get("test.ind",help.env)))
            if(class(cop)!="try-error") {
              val<- c(lam1.vec[jj],get("log.like",cop),get("cAIC",cop))
            }
            else {
             val <- c(lam1.vec[jj],0,0)
             skip.ind<-c(jj,skip.ind)
            } 
            val
          }  
          print(res)
          if(length(skip.ind)>0) {
             res<-res[-skip.ind,]
             lam1.vec<-lam1.vec[-skip.ind]
          }
          ind<-which.min(res[,3])
          model.l<-pencopula(data=get("U",help.env)[,pairs.new[i,]],d=get("d",help.env),D=get("D",help.env),lambda=rep(lam1.vec[ind],2),pen.order=get("m",help.env),base=get("base",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),fix.lambda=fix.lambda,test.ind=get("test.ind",help.env))
          print(paste(i, "is ready",sep=""))
          model.l
      }   
      if(!l.search) model.l<- pencopula(data=get("U",help.env)[,pairs.new[i,]],d=get("d",help.env),D=get("D",help.env),lambda=rep(get("lambda",help.env)[1],2),pen.order=get("m",help.env),base=get("base",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),fix.lambda=fix.lambda,test.ind=get("test.ind",help.env))
      val <- list(v=get("ck.val",model.l),log.like = get("log.like",model.l), AIC=get("AIC",model.l),cAIC=get("cAIC",model.l),lambda=get("lambda",model.l),f.hat=get("f.hat.val",model.l),indep=FALSE,Index.basis.D=get("Index.basis.D",model.l),ddb=get("ddb",model.l),indep=get("indep",model.l))
      val
  }

  h <- foreach(i=1:no.pairs,.combine=rbind) %do% {
     if(order.stat=="cAIC") obj<-c(pairs.new[i,],h1[[i]]$cAIC)
     if(order.stat=="AIC") obj<-c(pairs.new[i,],h1[[i]]$AIC)
     obj
  }

  colnames(h) <- c("i","j","cAIC")
  if(is.null(RVM)) {
    mat <- matrix(NA,len,len)
    diag(mat) <- rep(0,len)
    for(i in 1:(len-1)) {
      for(j in (i+1):len) {
        mat[i,j] <- mat[j,i] <- h[which(h[,1]==i & h[,2]==j),3]
      }
    }
  }
  assign("pairs.new",pairs.new,help.env)
  assign("fit.level2",h1,help.env)
  assign("fit.results",h,help.env)
  if(is.null(RVM)) {
    obj <- minimum.spanning.tree(graph.adjacency(mat,diag=FALSE,mode="lower",weighted=TRUE),algorithm="prim")
    assign("min.sp.tree.2",obj,help.env)
    pairs.fit <- get.edgelist(obj, names=TRUE)
    rm(obj)
    pairs.fit <- pairs.fit[order(pairs.fit[,1]),]
    VineMatrix <- matrix(NA,dim(pairs.fit)[1],len+1)
    VineMatrix[,c(1,2)] <- pairs.fit
  }
  if(!is.null(RVM))  {
    VineMatrix <- matrix(NA,dim(pairs.fit)[1],len+1)
    VineMatrix[,c(1,2)] <- pairs.fit
  }
  numbers <- c()
  if(dim(pairs.fit)[1]<10) {
    assign("dez",10,help.env)
    numbers[1:dim(pairs.fit)[1]] <- as.numeric(paste("2.",seq(1,dim(pairs.fit)[1]),sep=""))
  }
  if(dim(pairs.fit)[1]>9&dim(pairs.fit)[1]<100) {
    assign("dez",100,help.env)
    numbers[1:9] <- as.numeric(paste("2.0",seq(1,9),sep=""))
    numbers[10:dim(pairs.fit)[1]]  <- as.numeric(paste("2.",seq(10,dim(pairs.fit)[1]),sep=""))
  }
  VineMatrix[,(len+1)] <- numbers
  assign("VineMatrix",VineMatrix,help.env)
  assign("pairs.fit",pairs.fit,help.env)

}
