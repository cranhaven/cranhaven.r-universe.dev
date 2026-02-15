#' A function to plot do a Consensus clustering to validate the results
#'
#' this function  calls the ConsensusClusterPlus function with thedaraset and plots a plot
#' with the heatmaps of the clustering for each number of clusters from 2 to max_clust
#'
#' @usage cons_clust(data,max_clust,TOPgenes)
#' @param data this is the data for the ConsensusClusterPlus
#' @param max_clust the max number of clusters that should be evaluated.
#' @param TOPgenes  the number of the top genes to choose for the clustering
#' @return plots a plot with all the heatmaps from the ConsensusClusterPlus for the number ofd clusters 2 to max_clust
#'  the same return value as the COnsensusClusterPlus
#' @export cons_clust
#' @examples
#' 
#' data(rna)
#' cons_clust(rna,5,TOPgenes=50)
#' 
########Consensus Clustering
cons_clust<-function(data,max_clust=5,TOPgenes=150){
  geneset<-data
  dim(geneset)
  mads=apply(geneset,1,stats::mad)
  geneset=geneset[rev(order(mads))[1:TOPgenes],]
  geneset= sweep(geneset,1, apply(geneset,1,stats::median,na.rm=T))
  title=tempdir()
  results = ConsensusClusterPlus::ConsensusClusterPlus(as.matrix(geneset),maxK=max_clust,reps=50,pItem=0.8,pFeature=1,
                                 title=title,clusterAlg="hc",distance="euclidean")

  aa=seq(1,max_clust, by=1)
  length(aa)=suppressWarnings(prod(dim(matrix(aa,ncol = 3))))
  aa[is.na(aa)]=0
  lm=matrix(aa, ncol=3, byrow = T)
  graphics::layout(lm, c(1),c(1))

  for(i in 2:max_clust){
      xx=as.matrix(results[[i]][["consensusMatrix"]])
      nc=ncol(xx)
      nr=nrow(xx)
      xx <- sweep(xx, 1L, rowMeans(xx, na.rm = T), check.margin = FALSE)
      sx <- apply(xx, 1L, stats::sd, na.rm = T)
      xx <- sweep(xx, 1L, sx, "/", check.margin = FALSE)
      xx<-t(xx)
      graphics::par(mar = c(3, 3, 3, 3))

      graphics::image(1L:nc, 1L:nr, xx, xlim = 0.5 + c(0, nc), ylim = 0.5 +
                         c(0, nr), axes = FALSE, xlab = "", ylab = "", col=RColorBrewer::brewer.pal(n = 4, "Blues"),useRaster=T)
      graphics::title(main=paste("ConsensusCluster",i), line = 2)

    }


  return(results)
}
