#' Final Clustering Step for Meta-Clustering
#'
#' This function merges initial clusters into final clusters based on the IDEr similarity matrix.
#'
#' @param seu A Seurat object that has undergone the \code{getIDEr} step. Required.
#' @param dist A list output from the \code{getIDEr} function. Required.
#' @param cutree.by Character string specifying whether to cut the dendrogram by 
#' height ("h") or by a fixed number of clusters ("k"). Default is "h".
#' @param cutree.h Numeric value between 0 and 1 indicating the height at which 
#' to cut the dendrogram. This parameter is ignored if \code{cutree.by = "k"}. Default is 0.45.
#' @param cutree.k Numeric value specifying the number of clusters to generate 
#' if \code{cutree.by = "k"}. This parameter is ignored if \code{cutree.by = "h"}. Default is 3.
#' @param hc.method Character string specifying the method to be used in 
#' hierarchical clustering (passed to \code{hclust}).
#'
#' @return A Seurat object with the final clustering results stored in 
#' the \code{CIDER_clusters} column of its \code{meta.data}.
#'
#' @seealso \code{\link{getIDEr}}
#'
#' @export
#'
#' @importFrom stats hclust cutree as.dist
finalClustering <- function(seu, dist,
                            cutree.by = "h", cutree.h = 0.45, cutree.k = 3,
                            hc.method = "complete") {

  hc <- hclust(as.dist(1 - dist[[1]]) / 2, method = hc.method)

  if (cutree.by == "h") {
    hcluster <- cutree(hc, h = cutree.h)
  } else {
    hcluster <- cutree(hc, k = cutree.k)
  }

  df_merge <- data.frame(
    initial_clusters = names(hcluster),
    final_clusters = hcluster
  )

  seu$CIDER_cluster <- df_merge$final_cluster[match(
    seu$initial_cluster,
    df_merge$initial_clusters
  )]
  seu$CIDER_cluster[is.na(seu$CIDER_cluster)] <-
    seu$initial_cluster[is.na(seu$CIDER_cluster)]

  return(seu)
}
