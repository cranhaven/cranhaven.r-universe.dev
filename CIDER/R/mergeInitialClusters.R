#' Merge Initial Clusters
#'
#' Merge initial clusters based on a provided similarity matrix and hierarchical clustering.
#'
#' This function accepts a list of Seurat objects and a corresponding list of similarity matrices,
#' and then merges the initial clusters using a hierarchical clustering approach. The updated cluster
#' assignments are stored within each Seurat object.
#'
#' @param seu_list A list of Seurat objects containing the single-cell data. This parameter is required.
#' @param dist_list A list of similarity matrices as returned by \code{getDistMat()}. The order of matrices
#' should correspond to that of the Seurat objects in \code{seu_list}.
#' @param use A string specifying the similarity measure to use. Currently, only "coef" is supported. Default is "coef".
#' @param method A string specifying the clustering method to employ. The default is "hc" for hierarchical clustering.
#' @param hc.method A string passed to the \code{method} parameter of \code{hclust()}. Default is "average".
#' @param cutree.by A character indicating whether to cut the dendrogram by height ("h", default) or by
#' a set number of clusters ("k").
#' @param cutree.h A numeric value defining the height at which to cut the tree if \code{cutree.by = "h"}. Default is 0.6.
#' @param cutree.k A numeric value specifying the number of clusters to generate if \code{cutree.by = "k"}. Default is 3.
#' @param batch.var A character string representing the metadata column name that contains batch information.
#' Default is "Batch".
#'
#' @return A list of Seurat objects in which the initial clustering has been updated. The new cluster
#' assignments are stored in the \code{inicluster} field of each Seurat object, whilst the original
#' assignments are preserved in the \code{inicluster_tmp} field.
#'
#' @seealso \code{\link{gatherInitialClusters}}, \code{\link{initialClustering}}
#'
#' @importFrom stats cutree hclust as.dist
#' @import Seurat
#' @export
mergeInitialClusters <- function(seu_list, dist_list, use = "coef",
                                 method = "hc",
                                 hc.method = "average", cutree.by = "h",
                                 cutree.h = 0.6, cutree.k = 3, batch.var = "Batch") {
  if (use == "coef") {
    dist_coef <- dist_list[[1]]
  } else if (use == "t") {
    dist_coef <- dist_list[[2]]
  } else if (use == "p") {
    dist_coef <- dist_list[[3]]
  }

  for (seu_itor in seq_len(length(seu_list))) {
    hc <- hclust(1 - as.dist(dist_coef[[seu_itor]] + t(dist_coef[[seu_itor]])),
                 method = hc.method)

    if (cutree.by == "h") {
      hres <- cutree(hc, h = cutree.h)
    } else {
      hres <- cutree(hc, k = cutree.k)
    }

    df_hres <- data.frame(hres)
    df_hres$hres <- paste0(df_hres$hres, "_",
                           unique(seu_list[[seu_itor]]@meta.data[[batch.var]]))
    seu_list[[seu_itor]]$inicluster_tmp <-
      paste0(seu_list[[seu_itor]]$seurat_clusters, "_",
             seu_list[[seu_itor]]@meta.data[[batch.var]])
    seu_list[[seu_itor]]$inicluster <-
      df_hres$hres[match(seu_list[[seu_itor]]$inicluster_tmp,
                         rownames(df_hres))]
  }
  return(seu_list)
}

#' Gather Initial Cluster Names
#'
#' Merge initial clustering results from a list of Seurat objects into a single Seurat object.
#'
#' @param seu_list A list containing Seurat objects with initial clustering results. Required.
#' @param seu A Seurat object to which the merged initial cluster information will be added.
#'
#' @return A Seurat object containing the initial clustering results in the \code{initial_cluster} column of its \code{meta.data}.
#'
#' @seealso \code{\link{mergeInitialClusters}}
#'
#' @export
#'
#' @import Seurat
gatherInitialClusters <- function(seu_list, seu) {
  tmp <- unlist(vapply(seu_list, function(x) {
    return(x$inicluster_tmp)
  }))
  names(tmp) <- unlist(vapply(seu_list, function(x) {
    return(colnames(.getCountsMatrix(x)))
  }))
  seu$initial_cluster_tmp <- tmp[match(colnames(seu), names(tmp))]

  tmp <- unlist(vapply(seu_list, function(x) {
    return(x$inicluster)
  }))
  names(tmp) <- unlist(vapply(seu_list, function(x) {
    return(colnames(.getCountsMatrix(x)))
  }))
  seu$initial_cluster <- tmp[match(colnames(seu), names(tmp))]

  return(seu)
}
