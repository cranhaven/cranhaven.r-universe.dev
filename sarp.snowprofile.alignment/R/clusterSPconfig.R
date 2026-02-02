#' Configure clusterSP computation
#'
#' Configure the (hyper)parameters to computing snow profile clusters.
#'
#' @param type which method of clustering, current options are `kdba` or default `n` which provides generic config
#' @param simType which profile similarity method is used for alignments, see [simSP] for options
#' @param ddate if profiles contain deposition date adjust the dimensions and weights used for layer similarities passed to [distanceSPlayers]
#' @param pwls if profiles contain stability indices then use that information to classify PWLS in [averageSP]
#' @param n_cores number of nodes passed to [averageSP] and [distanceSP] to run calculations in parallel, default NULL performs all calculations in serial
#' @param verbose logical indicating whether to print various diagnostics
#'
#' @return a list containing the following:
#' - `args_distance`: a parameter list passed on to the distance function during clustering. This list will determine the distance
#' computation in clustering type `'kdba'`, and the computation of a distance matrix with [distanceSP] for other clustering types
#' - `args_centers`: a parameter list passed on to [clusterSPcenters] during clustering
#' - `args_cluster`: a parameter list passed onto the clustering functions [stats::hclust], [cluster::pam], [cluster::fanny], [clusterSPkdba]
#' - `args_fast`: a named vectors with weights for summary stats used in in `fast` option
#' - `verbose`: copied from the input parameter `verbose`
#'
#' @seealso [clusterSP]
#' @author fherla shorton
#' @examples
#' print(clusterSPconfig(ddate = TRUE))
#' @importFrom utils modifyList
#' @export
clusterSPconfig <- function(type = 'n',
                            simType = NULL,
                            ddate = FALSE,
                            pwls = FALSE,
                            n_cores = NULL,
                            verbose = TRUE) {

  ## Basic config structure
  config <- list(args_distance = list(rescale_resample = FALSE),
                 args_centers = list(),
                 args_cluster = list(),
                 args_simpleweights = list(),
                 verbose = verbose)

  ## Update parameters related to aligning, comparing, and averaging profile via: distanceSP, averageSP
  if (!is.null(simType)) {
    config$args_distance$simType <- config$args_centers$simType <- simType
  }
  if (ddate) {
    config$args_distance$dims <- config$args_centers$dims <- c('gtype', 'hardness', 'ddate')
    config$args_distance$weights <- config$args_centers$weights <- c(0.375, 0.125, 0.5)
  }
  if (pwls) {
    config$args_centers$classifyPWLs <- list(pwl_gtype = c('SH', 'DH', 'FC', 'FCxr', 'MFcr'),
                                             threshold_RTA = 0.65,
                                             threshold_PU = 0.77,
                                             threshold_gtype = c('FC', 'FCxr', 'MFcr'))
  }
  if (!is.null(n_cores)) {
    config$args_distance$n_cores <- config$args_centers$n_cores <- n_cores
  }

  ## Parameters for specific clustering methods
  if (type == 'kdba') {
    config$args_cluster <- modifyList(config$args_cluster, list(iccentroids = 'random',
                                                                icclusters = NA,
                                                                itermax = 3))
  }

  ## Weights for simple clustering
  config$args_fast <- c(w_hs = 2, w_hn24 = 0, w_h3d = 1, w_slab = 0,
                        w_gtype = 0, w_gtype_rel = 1, w_new = 0,
                        w_pwl = 0, w_crust = 0, w_rta = 0)


  return(config)
}
