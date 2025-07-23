#' Get broad and fine clusters
#'
#' @param seuratlist A list of Seurat objects, usually can be got by SplitObject(). We also accept the SingleCellExperiment object input.
#' @param n1 If the number of cells was smaller than n1, then the cluster will remain unchanged called rare cluster. The default value of n1 is 50.
#' @param n2 If the count of cells within a broad cluster is more than n2, the cluster is subdivided randomly into three fine clusters. If the cell count falls within the range of n1 to n2, two fine clusters are generated randomly. Default value is 200.
#'
#' @return A list of data frames.
#' @export
#'
#' @examples
#' \donttest{
#' data(sim_data_sce)
#' sim_data <- SCEtoSeurat(sim_data_sce)
#' seuratlist <- Seurat::SplitObject(sim_data, split.by = "Study")
#' fullcluster <- GetCluster(seuratlist)
#' }


GetCluster <- function(seuratlist,n1 = 50,n2 = 200) {

  stopifnot(exprs = {
    is.list(seuratlist)
    is.numeric(n1)
    is.numeric(n2)
  })

  allcluster <- list()

  for(i in seq_along(seuratlist)) {
    onedata <- seuratlist[[i]]
    onedata <- Seurat::NormalizeData(onedata)
    onedata <- Seurat::FindVariableFeatures(onedata, selection.method = "vst", nfeatures = 2000)
    all.genes <- rownames(onedata)
    onedata <- Seurat::ScaleData(onedata, features = all.genes)
    onedata <- Seurat::RunPCA(onedata,
                              features = Seurat::VariableFeatures(object = onedata),seed.use = sample(1000,1))
    onedata <- Seurat::FindNeighbors(onedata, dims = 1:20)
    onedata <- Seurat::FindClusters(onedata, resolution = 0.5)
    allcluster[[i]] <- onedata$seurat_clusters
  }


  allcluster2 <- list()
  for(i in seq_along(seuratlist)) {
    ncount = 1
    thisc <- allcluster[[i]]
    sumtab <- table(thisc)
    idx <- names(sumtab)
    finecluster = rarecluster = rep(NA, length(thisc))
    for(k in seq_along(sumtab)) {
      if( sumtab[k] < n1 ) {
        finecluster[thisc == idx[k]] <- ncount
        rarecluster[thisc == idx[k]] <- 1
        ncount = ncount + 1
      } else if (sumtab[k] >= n1 & sumtab[k] < n2) {
        finecluster[thisc == idx[k]] <- sample(ncount + 0:1, sum(thisc == idx[k]), replace = TRUE)
        rarecluster[thisc == idx[k]] <- 0
        ncount = ncount + 2
      } else if (sumtab[k] >= n2) {
        finecluster[thisc == idx[k]] <- sample(ncount + 0:2, sum(thisc == idx[k]), replace = TRUE)
        rarecluster[thisc == idx[k]] <- 0
        ncount = ncount + 3
      }
    }
    oneres <- data.frame(broadcluster = thisc,
                         finecluster = finecluster,
                         rarecluster = rarecluster)
    allcluster2[[i]] <- oneres
  }

  return(allcluster2)
}
