order.Dvine <- function(help.env) {

  len <- length(get("S",help.env))
  no.pairs <- choose(len,2)
  order.stat <- get("order.stat",help.env)
  pairs <- matrix(NA,no.pairs,2)
  count <- 1
  lambda.search<-get("lambda.search",help.env)
  fix.lambda<-get("fix.lambda",help.env)
  for(i in 1:(len-1)) {
    for(j in (i+1):len) {
      pairs[count,] <- c(i,j)
      count <- count+1
    }
  }  
     h1 <- foreach(i=1:no.pairs,.combine=c,.multicombine=TRUE) %dopar% {
         if(!lambda.search) model.l<- pencopula(data=get("U",help.env)[,pairs[i,]],d=get("d",help.env),D=get("D",help.env),lambda=rep(get("lambda",help.env)[1],2),pen.order=get("m",help.env),base=get("base",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),fix.lambda=fix.lambda)
         if(lambda.search) {
            lam1.vec<-get("lam1.vec",help.env)
            skip.ind<-c()
            jj<-1
            res<-foreach(jj=1:length(lam1.vec),.combine=rbind) %do% {
              cop<- try(pencopula(data=get("U",help.env)[,pairs[i,]],d=get("d",help.env),D=get("D",help.env),lambda=rep(lam1.vec[jj],2),pen.order=get("m",help.env),base=get("base",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),fix.lambda=fix.lambda))
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
            model.l<-pencopula(data=get("U",help.env)[,pairs[i,]],d=get("d",help.env),D=get("D",help.env),lambda=rep(lam1.vec[ind],2),pen.order=get("m",help.env),base=get("base",help.env),max.iter=get("max.iter",help.env),q=get("q",help.env),fix.lambda=fix.lambda)
         }
         print(paste(i, "is ready",sep="")) 
         model.l
   }

   h <- foreach(i=1:no.pairs,.combine=rbind) %dopar% {
     c(pairs[i,],get(order.stat,h1[[i]]))
   }

  colnames(h) <- c("i","j","cAIC")
  mat <- matrix(NA,len,len)
  diag(mat) <- rep(0,len)
  for(i in 1:(len-1)) {
    for(j in (i+1):len) {
      mat[i,j] <- mat[j,i] <- h[which(h[,1]==i & h[,2]==j),3]
    }
  }
  assign("pairs.new",pairs,help.env)
  assign("fit.level1",h1,help.env)
  assign("fit.results",h,help.env)
  ind.val2<-which(round(mat,3)==round(min(round(mat,3)),3))
  obj <- as.integer(ceiling(ind.val2/len))
  ii<-1
  res<-foreach(ii=1:length(obj),.combine=rbind) %do% {
    tour1 <- solve_TSP(as.TSP(mat),method="nn",control=list(start=obj[ii]))
    route<-as.integer(tour1)
    len.r<-attr(tour1, "tour_length")-mat[route[1],route[length(route)]]
    c(ii,len.r,tour1)
  }
  ind<-which.min(res[,2])
  assign("order",route <- as.integer(res[ind,-c(1,2)]),help.env)
  pairs.fit <- matrix(NA,length(route)-1,2)
  for(l in 1:(length(route)-1)) pairs.fit[l,] <- route[c(l,l+1)]
  assign("pairs.fit",pairs.fit,help.env)
  print("ready")
}
