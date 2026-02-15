#' Compute Top genes
#'
#' This function computes the n=TOP genes and the the best number of clusters
#'
#' @export TopPAM
#' @usage TopPAM(me, max_clusters=15,TOP=1000,B=100,clusterboot=FALSE)
#' @param me a matrix with genes in rows and samples in columns
#' @param max_clusters max. number of clusters to check
#' @param TOP the number of genes to take.
#' @param B integer, number of Monte Carlo (“bootstrap”) samples.
#' @param clusterboot A logical value indicating wether or not to calculate the Gap statistic and to bootstrap.
#' @details we use the clusGap algorithm from the package cluster to calculate the Gap statistic.
#' @return a list of 1. A matrix with the top genes
#' 2. A list of means of the Silhouette width for each number of clusters. 3. The optimal number of clusters. 4. gap_st the gap statistic of the clustering
#' 5. best number of clusters according to the gap statistic.
#'
#' @examples
#' 
#' ##load the org.Hs.eg Library
#' library(org.Hs.eg.db)
#' #' ## load data
#' data(rna)
#' me_x=rna
#' res<-AutoPipe::TopPAM(me_x,max_clusters = 8, TOP=100,clusterboot=FALSE)
#' me_TOP=res[[1]]
#' number_of_k=res[[3]]
#' 
#' @import  graphics
#' @import  cluster
TopPAM=function(me, max_clusters=15,TOP=1000,B=100,clusterboot=FALSE){
  #Top 1000
  dim(me)
  sd=as.data.frame(apply(me,1, function(x){sd(x)}))
  sd=as.data.frame(sd[order(sd[,1], decreasing = T), ,drop = FALSE])
  me_TOP=me[rownames(sd)[1:TOP], ]
  dim(me_TOP)

  #Do Cluster PAM
  #How many clusters
  sil_mean=as.numeric(do.call(cbind, lapply(2:max_clusters, function(i){
    pamx <- cluster::pam(t(me_TOP), k=i)
    print(paste("Cluster with k=",i, sep=""))
    si <- cluster::silhouette(pamx)
    mean_s=mean(as.numeric(si[,3]))
    return(mean_s)
  })))
  
  gap_st=NULL
  best_nc_gp=NULL
  if(clusterboot==TRUE){
    gap_st<-cluster::clusGap(t(me_TOP),FUNcluster = cluster::pam,K.max = max_clusters,B=100)
    graphics::plot(gap_st,main = "Gap Statistics", bty="n",xaxt="n")
    graphics::axis(side = 1,at = c(1:19),labels= c(2:20))
    best_nc_gp<-cluster::maxSE(gap_st$Tab[,3],gap_st$Tab[,4],method = "Tibs2001SEmax",.25)
  }

  return(list(me_TOP,sil_mean,which.max(sil_mean)+1,gap_st,best_nc_gp))
}