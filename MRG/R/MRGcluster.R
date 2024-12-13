#' Function that allows to apply parallel processing
#' @import parallel
#' @param nclus Number of clusters to use for parallel processing. No parallelization is used
#'            for \code{nclus = 1}.
#' @param ... arguments that should be evaluated in the cluster (can also be called later)
#' @param action Defines the action of the function. There are three options:
#'       \describe{
#'         \item{"start"}{Starts a new cluster if necessary, reuses an existing if it has already been started}
#'         \item{"restart"}{Stops the cluster and starts it again. To be used in case there are difficulties with the cluster,
#'                 or if the user wants to change the type of the cluster}
#'       }
#' @param clusType The type of cluster; see \code{\link[parallel]{makeCluster}} for more details.
#'         The default of makeCluster is used if type is missing or NA.
#' @param  outfile File to direct the output, \code{\link[parallel]{makeCluster}} for more details.
#' 
#' @returns The function will either return a cluster for parallel computation,
#' or stop a cluster (returning NULL)
#' 
#' @export
MRGcluster = function(nclus, ..., action = "start", clusType, outfile = NULL ) {
  cl = getOption("MRGcluster")
  if (length(cl) > 0) {
    tt = try(clusterApply(cl, 1:2, get("+"), 3))
    #' @importFrom methods is 
    if (is(tt, "try-error")) {options(MRGcluster = NULL); cl = NULL}
  }
  if (length(cl) > 0 && (action == "stop" | action == "restart")) {
    parallel::stopCluster(cl)
    options(MRGcluster = NULL)
  }
  if (length(cl) > 0 && action == "start") {
    if (length(list(...)) > 0) parallel::clusterEvalQ(cl, ...)
  } else if (action == "start" | action == "restart") {
    if (!requireNamespace("parallel")) stop("Not able to start cluster, parallel not available")
    if (missing(clusType) || is.null(clusType)) {
      cl <- parallel::makeCluster(nclus, outfile = outfile)
    } else {
      cl <- parallel::makeCluster(nclus, clusType, outfile = outfile)
    }
    #    doParallel::registerDoParallel(cl, nclus)
    if (length(list(...)) > 0) parallel::clusterEvalQ(cl, ...)
    options(MRGcluster = cl)
  }
  getOption("MRGcluster")
}

.onExit = function() {
  if (!is.null(options()$MRGcluster)) MRGcluster(action = "stop")
}

