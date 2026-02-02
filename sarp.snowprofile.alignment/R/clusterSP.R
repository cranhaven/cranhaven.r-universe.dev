#' Cluster snow profiles
#'
#' This function is the main gateway to [sarp.snowprofile::snowprofile] clustering.
#'
#' There are several clustering approaches that can be applied to snow profiles. Most rely on computing a pairwise distance matrix between all profiles
#' in a snowprofileSet. Current implementations with this approach rely on existing R functions:
#' - agglomerative hierarchical clustering [stats::hclust]
#' - partitioning around medoids [cluster::pam]
#' - fuzzy analysis clustering [cluster::fanny]
#'
#' Since computing a pairwise distance matrix matrix can be slow, the recommended way of testing different number of clusters $k$ is precomputing
#' a single distance matrix with the [distanceSP] function and providing it as an argument to clusterSP.
#'
#' An alternate type of clustering known a k-dimensional barycentric averaging *kdba* is conceptually similar to kmeans but specifically adapted to
#' snow profiles [clusterSPkdba]. That means that an initial clustering condition (which can be random or based on a 'sophisticated guess') is iteratively refined by
#' assigning individual profiles to the most similar cluster and at the end of every iteration recomputing the cluster centroids. The cluster centroids
#' are represented by the *average* snow profile of each cluster (see [averageSP]). Note that the results of kdba are sensitive to the initial conditions,
#' which by default are estimated with the 'fast' method below.
#'
#' And finally, a much faster 'fast' method is available that computes a pairwise distance matrix without aligning profiles, but instead based on
#' summary statistics such as snow height, height of new snow, presence or absence of weak layers and crusts, etc. The 'fast' clustering approach
#' uses the partitioning around medoids clustering approach with the 'fast' distance matrix.
#'
#' @param SPx a [sarp.snowprofile::snowprofileSet] to be clustered
#' @param k number of desired cluster numbers
#' @param type clustering type including `hclust` (default), `pam`, `fanny`, `kdba` and `fast`
#' @param distmat a precomputed distance matrix of class dist. This results in much faster clustering for `type %in% c('hclust', 'pam', 'fanny')`
#' as well as faster identification of medoid profiles if `centers %in% c('medoids', 'both')`
#' @param config a list providing the necessary hyperparameters. Use [clusterSPconfig] functions for convenience!
#' @param centers compute and return `mediods`, `centroids`, `both`, or `none` for each cluster. default 'none' will only return centroids/medoids
#' if they were already calculated with the clustering algorithm, whereas other options could result in extra processing time to calculate additional centroids/medoids
#' @param keepSPx append the snowprofileSet to the output?
#' @param keepDistmat append the distmat to the output?
#'
#' @return a list of class `clusterSP` containing:
#' - `clustering`: vector of integers (from 1:k) indicating the cluster to which each point is allocated
#' - `id.med`: vector of indices for the medoid profiles of each cluster (if calculated)
#' - `centroids`: snowprofileSet containing the centroid profile for each cluster (if calculated)
#' - `tree`: object of class 'hclust' describing the tree output by hclust
#' - `...`: all other outputs provided by the clustering algorithms (e.g., a membership matrix
#'          from `fanny.object`, `pam.object`, iteration history from [clusterSPkdba])
#' - `type`: type of clustering as provided by input argument
#' - `call`: a copy of the clusterSP function call
#' - `SPx`: a copy of the input snowprofileSet (if `keepSPx = TRUE`)
#' - `distmat`: the pairwise distance matrix of class dist (if `keepDistmat = TRUE` and a matrix has been provided or computed)
#'
#' @details
#'
#' More details here...
#'
#'
#' @seealso [clusterSPconfig], [clusterSPcenters], [clusterSPkdba], [plot.clusterSP]
#' @author fherla shorton
#' @examples
#' this_example_runs_too_long <- TRUE
#' if (!this_example_runs_too_long) {  # exclude from cran checks
#'
#'   ## Cluster with SPgroup2, which contains deposition date and p_unstable
#'   SPx <- SPgroup2
#'   config <- clusterSPconfig(simType = 'wsum_scaled', ddate = T, pwls = T)
#'
#'   ## Hierarchical clustering with k = 2
#'   cl_hclust <- clusterSP(SPx, k = 2, type = 'hclust', config = config)
#'   plot(cl_hclust)
#'
#'   ## Precompute a distance matrix and cluster with PAM for k = 2 and 3
#'   distmat <- do.call('distanceSP', c(list(SPx), config$args_distance))
#'   cl_pam2 <- clusterSP(SPx, k = 2, type = 'pam', config = config, distmat = distmat)
#'   cl_pam3 <- clusterSP(SPx, k = 3, type = 'pam', config = config, distmat = distmat)
#'   print(cl_pam2$clustering)
#'   print(cl_pam3$clustering)
#'
#'   ## kdba clustering
#'   config_kdba <- clusterSPconfig(simType = 'layerwise', type = 'kdba')
#'   cl_kdba <- clusterSP(SPx = SPgroup2, k = 2, type = 'kdba', config = config_kdba)
#'   plot(cl_kdba)
#'
#' }
#' @import cluster
#' @importFrom stats cutree
#' @export
clusterSP <- function(SPx = NULL,
                      k = 2,
                      type = c('hclust', 'pam', 'fanny', 'kdba', 'fast')[1],
                      distmat = NULL,
                      config = clusterSPconfig(type),
                      centers = 'none',
                      keepSPx = TRUE,
                      keepDistmat = TRUE) {


  ## ---- Assertion and initialization ----

  if (is.null(SPx) & is.null(distmat)) stop('You must either provide SPx (a snowprofileSet) or distmat (dist object)')
  if (length(k) > 1) stop('Multiple k not supported. We suggest calling clusterSP multiple times with different k with a precomputed distmat (if applicable))')
  if (type == 'fanny' & k > length(SPx)/2) stop('Fanny method requires k < (# of profiles/2)')
  if (!(type %in% c('hclust', 'pam', 'fanny', 'kdba', 'fast'))) stop(paste0('Clustering type "', type, '" not supported'))


  ## ---- Calculate pairwise distance matrix if not provided ----

  if (is.null(distmat)) {
    if (type == 'fast') {
      distmat <- distanceSP(SPx, fast_summary = TRUE, fast_summary_weights = config$args_fast)
    } else if (type != 'kdba') {
      message('Computing pairwise distances between profiles..')
      distmat <- do.call('distanceSP', c(list(SPx), config$args_distance))
    }
  }


  ## ----  Standard clustering methods using the distmat (hclust/pam/fanny) ----

  if (type == 'hclust') {
    tree <- do.call('hclust', c(list(d = distmat), config$args_cluster))
    clustering <- cutree(tree, k = k)
    clust <- list(clustering = clustering, tree = tree)
    clust$id.med <- medoidSP(SPx, distmat, clust$clustering)
  }

  if (type %in% c('pam', 'fast')) {
    clust <- do.call('pam', c(list(x = distmat, k = k), config$args_cluster))
  }

  if (type == 'fanny') {
    clust <- do.call('fanny', c(list(x = distmat, k = k), config$args_cluster))
    ## Add medoid indices similar to PAM using the strongest members
    clust$id.med <- apply(clust$membership, 2, which.max)
  }

  ## Compute centroids based on clustering
  if (centers %in% c('centroids', 'both') && type != 'kdba') {
    message('Computing k cluster centroids..')
    clust$centroids <- clusterSPcenters(SPx, clust$clustering, config, centers = 'centroids')
  }


  ## ----  k-dimensional barycentric averaging ----

  if (type == 'kdba') {

    ## Create initial clusters and centroids with simple distmat and pam
    if (!'icclusters' %in% names(config$args_cluster)) stop("When using kdba clustering, config must contain args_cluster$icclusters, use clusterSPconfig(type = 'kdba')")
    if (is.na(config$args_cluster$icclusters)) {
      icdistmat <- distanceSP(SPx, fast_summary = TRUE, fast_summary_weights = config$args_fast)
      ic <- cluster::pam(icdistmat, k = k)
      config$args_cluster$icclusters <- ic$clustering
      config$args_cluster$iccentroids <- ic$id.med
    }

    ## Call kdba function
    clust <- clusterSPkdba(SPx = SPx, k = k, config = config)

    ## Add medoids
    if (centers %in% c('medoids', 'both')) {
      if (is.null(distmat)) message('Computing medoids with pairwise distances within each cluster..')
      clust$id.med <- do.call('medoidSP', c(list(SPx, distmat, clust$clustering), config$args_distance))
    }
  }

  ## --- Format output object ----

  clust$medoids <- SPx[clust$id.med]
  clust$type <- type
  if (keepSPx) clust$SPx <- SPx
  if (keepDistmat && 'distmat' %in% names(clust)) clust$distmat <- distmat
  clust$call <- match.call()
  if (!('clusterSP' %in% class(clust))) class(clust) <- c('clusterSP', class(clust))
  return(clust)
}
