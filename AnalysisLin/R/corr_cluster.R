#' @title Correlation Clustering
#' @description
#'   This function performs hierarchical clustering on a correlation matrix, providing insights into the relationships between variables.
#'   It generates a dendrogram visualizing the hierarchical clustering of variables based on their correlation patterns.
#'
#' @param data Input data frame.
#' @param type The type of correlation to be computed. It can be "pearson", "kendall", or "spearman".
#' @param method The method for hierarchical clustering. It can be "complete", "single", "average", "ward.D", "ward.D2", "mcquitty", "median", or "centroid".
#' @param hclust_method The hierarchical clustering method. It can be "complete", "single", "average", "ward.D", "ward.D2", "mcquitty", "median", or "centroid".
#'
#' @return 
#'  A dendrogram visualizing the hierarchical clustering of variables based on the correlation matrix.
#'
#' @examples
#' data(mtcars)
#' corr_cluster(data = mtcars, type = 'pearson', method = 'complete')
#'
#' @importFrom Hmisc rcorr
#' @import stats
#' @export
corr_cluster <- function(data, type = 'pearson', method = 'complete', hclust_method = NULL) {
  corr <- Hmisc::rcorr(as.matrix(data), type = type)$r
  dist_matrix <- as.dist(1 - abs(corr))
  hc <- hclust(dist_matrix, method = method)
  plot(hc, hang = -1, main = "Feature Distance")
}
