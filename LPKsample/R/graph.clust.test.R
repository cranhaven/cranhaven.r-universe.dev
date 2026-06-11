graph.clust.test <-
function(y,W,method='kmeans',perm=0,return.clust=F){
    out<-ls()
    k<-length(unique(y))
    Laplacian<- function(A){
      d<- apply(A,2,FUN="sum")
      L <- diag(1/sqrt(d))%*%A%*%diag(1/sqrt(d))
      return(L)
    }
   
    L1<-Laplacian(W)
    Lap.svd<-svd(L1)

    U.Lap<-diag(sqrt(sum(W)/rowSums(W)))%*%(Lap.svd$u[,2:k])
    if(method=='mclust'){
      m1<-mclust::Mclust(U.Lap,G=k,verbose=F)
      y.c<-m1$classification
    }else if(method=='kmeans'){
  centers<-aggregate(data.frame(U.Lap),list(y),FUN='mean')[,-1]
      if(sum(1-is.na(centers))==sum(1-is.na(unique(centers)))){
   y.c<-kmeans(U.Lap,centers,iter.max=50)$cluster
      }else{
   y.c<-kmeans(U.Lap,k,iter.max=50)$cluster
      }
    }
    test0<-LP.comean(y,y.c,perm=perm)
    if(return.clust==F){
   return(test0)
    }else if(return.clust==T){
   out$test<-list()
   out$clust<-y.c
   out$test<-test0
   return(out)
}

}
