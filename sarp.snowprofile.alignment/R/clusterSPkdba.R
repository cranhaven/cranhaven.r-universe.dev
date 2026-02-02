#' K-dimensional barycentric average clustering for snow profiles
#'
#' @param SPx a [sarp.snowprofile::snowprofileSet] to be clustered
#' @param k number of desired cluster numbers
#' @param config a list providing the necessary hyperparameters. Use [clusterSPconfig] function with `type = kdba` for convenience!
#' @param centers type of center to determine, either `centroids` (default) where an average profile is computed for each cluster
#'                or `medoids` where the index of the medoid profile is identified
#' @param distmat a precomputed distance matrix of class dist (only used if `centers = medoids`)
#' @param keepSPx append the snowprofileSet to the output?
#'
#' @return a list of class `clusterSP` containing:
#' - `clustering`: vector of integers (from 1:k) indicating the cluster to which each point is allocated
#' - `centroids`: snowprofileSet containing the centroid profile for each cluster (if calculated)
#' - `clusters_history`: matrix with history of clustering over iterations
#' - `iccentroids`: initial condition centroids
#' - `niterations`: number of iterations
#' - `converged`: did the algorithm converge?
#' - `SPx`: a copy of the input snowprofileSet (if `keepSPx = TRUE`)
#'
#' @author fherla shorton
#' @seealso [clusterSP], [clusterSPcenters]
#'
#' @importFrom stats runif
#' @export
#'
#' @examples
#' this_example_runs_too_long <- TRUE
#' if (!this_example_runs_too_long) {  # exclude from cran checks
#'   cl_kdba <- clusterSPkdba(SPgroup2, k = 2)
#'   plot(cl_kdba)
#' }
clusterSPkdba <- function(SPx, k, config = clusterSPconfig(type = 'kdba'), centers = 'centroids', distmat = NULL, keepSPx = TRUE) {

  ## ---- Unpack arguments ----
  iccentroids <- config$args_cluster$iccentroids
  icclusters <- config$args_cluster$icclusters
  itermax <- config$args_cluster$itermax
  verbose <- config$verbose

  ## ---- Initialize ----
  nSP <- length(SPx)
  i <- 1
  clustering <- vector('double', nSP)
  clustering_old <- matrix(0, nrow = itermax, ncol = nSP,
                           dimnames = list(Iterations = seq(itermax), Profiles = seq(nSP)))
  converged <- FALSE

  ## ---- Get initial cluster centroids ----

  ## Case 1: iccentroids provided
  if (is.numeric(iccentroids)) {
    if (length(iccentroids) != k) stop(paste0('Provided ', length(iccentroids), ' iccentroids, but ', k, ' required!'))
    centroids <- SPx[iccentroids]

    ## Case 2: iccentroids not provided, but icclusters are provided to calculate centroids
  } else if (all(is.na(iccentroids))) {
    if (!is.numeric(icclusters) & length(icclusters) != nSP) stop(paste0('Provided ', length(icclusters), ' icclusters, but ', nSP, ' required!'))
    if (verbose) message('Computing initial centroids..')
    centroids <- clusterSPcenters(SPx = SPx, clustering = icclusters, config = config, centers = centers)

    ## Case 3: Select k random centroids
  } else if (iccentroids == 'random'){
    iccentroids <- round(runif(k, 1, nSP))
    centroids <- SPx[iccentroids]
  } else {
    stop('Provide iccentroids (or icclusters)!')
  }
  centroidsHS <- summary(centroids)$hs


  ## ---- Iteratively assign profiles to the closest centroid ----

  while (i <= itermax){

    ## Loop through each profile
    for (s in seq_along(SPx)) {

      ## Compute distance between this profile and every centroid
      d_sp_cl <- sapply(centroids, function(cent) do.call('distanceSP', c(list(SPx = SPx[[s]], SP2 = cent), config$args_distance)))

      ## Assign profile to cluster with minimum distance to centroid
      if (!all(d_sp_cl == 1)) {
        clustering[s] <- which.min(d_sp_cl)

        ## Handle *outliers* that produce maximum distance to all centroids
      } else {
        #clustering[s] <- NA
        clustering[s] <- which.min(abs(SPx[[s]]$hs - centroidsHS))  # assigns to cluster with closest HS
        ## TODO: potentially most sophisticated approach: after few iterations of 'outlier' assigned to closest HS cluster, and still distance 1 --> set to outlier NA
      }
    }

    ## Decrease number of clusters if
    ## * only one member (will never change centroid and therefore get no other profiles assigned to it either in subsequent iterations)
    tbl_clustering <- table(clustering)
    clustering[clustering %in% as.numeric(names(tbl_clustering[tbl_clustering == 1]))] <- NA

    ## Print the clustering assignments for this iteration
    if (verbose) message(paste0('Iteration ', i, ': k = ', length(unique(na.omit(clustering))),
                                '\nclustering: ', paste0(clustering, collapse = ', ')))

    ## Update centroids
    centroids <- clusterSPcenters(SPx = SPx, clustering = clustering, config = config, centers = centers,
                                   clustering_old = clustering_old[i - 1,], cents = centroids, distmat = distmat)

    ## TODO: * merge clusters if too similar

    ## Wrap up current iteration
    message(paste0('RMSE: ', paste0(do.call('c', lapply(centroids, function(cent) round(cent$rmse, digits = 3))), collapse = ' '), '\n'))
    clustering_old[i,] <- clustering  # update cluster IDs

    # Break iterations if no profile changed clusters or if i == itermax
    if (i > 1 && all(clustering_old[i - 1,] == clustering, na.rm = TRUE)) {
      if (verbose) message('Clusters converged!')
      converged <- TRUE
      break()
    } else if (i == itermax) {
      if (verbose) message('Maxmimum iterations reached, clusters did not converge.')
      break()
    }
    i <- i + 1  # advance iteration increment
  }
  ## TODO: backtrack layers after final iteration: include in example!


  ## --- Save output ----
  clust <- list(clustering = clustering,
                centroids = centroids,
                clusters_history = clustering_old[1:i, ],
                iccentroids = iccentroids,
                niterations = i,
                converged = converged)
  if (keepSPx) clust$SPx <- SPx
  class(clust) <- c('clusterSP', class(clust))
  return(clust)
}
