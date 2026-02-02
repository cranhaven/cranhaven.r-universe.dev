#' Compute centroids/medoids for clustered snow profiles
#'
#' Wrapper for calculating centroids [averageSP] or medoids [medoidSP] when clustering, with efficient optimization when iterating kdba clustering calculations
#'
#' @param SPx a [sarp.snowprofile::snowprofileSet] to be clustered
#' @param clustering vector of integers (from 1:k) indicating the cluster to which each snow profile is allocated
#' @param config a list providing the necessary hyperparameters for distance and average calculations. Use [clusterSPconfig] functions for convenience!
#' @param centers type of center to determine, either `centroids` (default) where an average profile is computed for each cluster
#'                or `medoids` where the index of the medoid profile is identified
#' @param clustering_old same as `clustering` but from the previous iteration of a kdba (if provided then this function only computes new centroids when the profiles within a cluster changed)
#' @param cents a [sarp.snowprofile::snowprofileSet] of centroids from the previous iteration; provide `NULL` if not available
#' @param distmat a precomputed distance matrix of class dist (for faster medoid calculations)
#'
#' @return a named [sarp.snowprofile::snowprofileSet] of centroid/medoids profiles for each cluster where the name refers to the corresponding cluster and is sorted in ascending manner.
#' @author fherla shorton
#' @seealso [clusterSPkdba], [dbaSP]
#' @importFrom stats as.dist
#' @export
clusterSPcenters <- function(SPx,
                             clustering,
                             config,
                             centers = 'centroids',
                             clustering_old = NULL,
                             cents = NULL,
                             distmat = NULL) {

  ## Split profiles into clusters
  SPx_split <- split(SPx, clustering)
  names_split <- names(SPx_split)

  ## Mask NA clustering with 999
  clustering[is.na(clustering)] <- 999
  clustering_old[is.na(clustering_old)] <- 999

  ## Efficiency: only compute centers of clusters that changed!
  ## This implementation has one shortcoming: It will still recompute the cluster centers
  ## if the clustering changed, but the associated profiles remained constant (i.e., inefficient in few cases --> acceptable for now)
  if (length(clustering_old) == length(clustering)) {  # in iteration 1, clustering_old will be empty!
    recompute <- unique(c(clustering[clustering != clustering_old], clustering_old[clustering != clustering_old]))
  } else {
    recompute <- unique(clustering)
  }
  recompute <- recompute[recompute != 999]  # no center computation for NA clustering

  ## Compute centers
  if (centers == 'centroids') {
    new_centroids <- lapply(seq(which(names_split %in% recompute)), function(ii) {
      ids <- which(names_split == names_split[ii])
      xx <- SPx_split[[ids]]
      do.call('averageSP', c(list(SPx = xx, progressbar_pretext = paste0('for cluster ', names_split[ii])),
                             config$args_centers))$avg
    })
  }
  if (centers == 'medoids') {
    new_centroids <- lapply(seq(which(names_split %in% recompute)), function(ii) {
      ids <- which(names_split == names_split[ii])
      xx <- SPx_split[[ids]]
      if (is.null(distmat)) {
        id.med <- do.call('medoidSP', c(list(SPx = xx), config$args_distance))
      } else {
        dmat <- as.dist(as.matrix(distmat)[ids, ids])
        id.med <- do.call('medoidSP', c(list(SPx = xx, distmat = dmat), config$args_distance))
      }
      cent <- xx[[id.med]]
      cent$rmse <- NA
      return(cent)
    })
  }
  names(new_centroids) <- names_split[names_split %in% recompute]

  ## Reassemble new and old centroids
  old_centroids <- cents[names(cents) %in% clustering[!clustering %in% recompute]]
  return_centroids <- c(old_centroids, new_centroids)

  ## Make sure the centroids are named by their clustering and are ordered accordingly!!
  return_centroids <- snowprofileSet(return_centroids[order(as.numeric(names(return_centroids)))])
  return(return_centroids)
}
