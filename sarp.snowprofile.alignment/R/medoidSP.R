#' Find the medoid snow profile among a group of profiles
#'
#' Find the medoid snowprofile among a group of profiles, based on their pairwise dissimilarity. Either provide a list
#' of `snowprofile` objects, or a precomputed distance matrix.
#'
#' If  providing a large number of profiles without a precomputed distance matrix consider providing a `ncores`
#' argument so `distanceSP` will calculate alignments in parallel.
#'
#' @param SPx a [sarp.snowprofile::snowprofileSet] object
#' @param distmat If you have a precalculated distance matrix, provide it here to compute the medoid on it.
#' @param clustering index of clusters, if provided instead of identifying the medoid profile of the entire snowprofileSet it will
#' return a vector of medoids for each cluster
#' @param keepDistmat Do you want to return the pairwise distance matrix?
#' @param ... arguments passed to [distanceSP] and then further to [dtwSP] and [simSP]
#'
#' @return If `keepDistmat = FALSE` return the (named) index of the medoid snow profile, otherwise return a list with the elements
#' `id.med` and `distmat`.
#'
#' @author fherla shorton
#'
#' @examples
#' this_example_runs_about_5s <- TRUE
#' if (!this_example_runs_about_5s) {  # exclude from cran checks
#'
#' ## take a list of profiles
#' grouplist <- SPgroup2[1:4]
#' plot(grouplist, SortMethod = 'unsorted', xticklabels = "originalIndices")
#'
#' ## calulate medoid profile
#' id.med <- medoidSP(grouplist)
#' representativeProfile <- grouplist[[id.med]]
#' plot(representativeProfile, main = paste0("medoid (i.e., profile ", id.med, ")"))
#'
#' }
#' @export

medoidSP <- function(SPx, distmat = NULL, clustering = NULL, keepDistmat = FALSE, ...) {

  ## Compute pairwise distance matrix from snowprofileSet
  if (is.null(distmat)) distmat <- distanceSP(SPx, output = 'matrix', ...)

  ## Medoid element (compute row sums and search for the (intra-group) minimal total distance, i.e. medoid)
  if (is.null(clustering)) {
    d <- rowSums(as.matrix(distmat), na.rm = TRUE)
    id.med <- which.min(d)

  ## Medoids for subsets of SPx defined by clustering vector
  } else {
    if (length(clustering) != length(SPx) | !is.numeric(clustering)) stop('clustering parameter must be a numeric vector of length(SPx)')
    id.med <- sapply(sort(unique(clustering)), function(ii) {
      ids <- which(clustering == ii)
      dmat <- as.matrix(distmat)[ids, ids]
      d <- rowSums(as.matrix(dmat), na.rm = TRUE)
      id.sub <- which.min(d)
      id.med <- which(clustering == ii)[id.sub]
      return(id.med)
    })
  }

  if (keepDistmat) return(list(id.med = id.med, distmat = distmat))
  else return(id.med)
}
