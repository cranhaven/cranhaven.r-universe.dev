#' @rdname pcps.sig
#' @encoding UTF-8
#' @export
matrix.p.sig <- function (comm, phylodist, envir, checkdata = TRUE, FUN, runs = 999, parallel = NULL, newname = "pcps", ...) 
{
  res <- list(call = match.call())
  if (inherits(comm, "metacommunity.data")) {
    if (!missing(phylodist)) {
      stop("\n When you use an object of class metacommunity.data the argument phylodist must not be specified. \n")
    }
    phylodist <- comm$phylodist
    envir <- comm$environmental
    comm <- comm$community
  }
  list.warning <- list()
  if(checkdata){
    organize.temp <- organize.pcps(comm, phylodist = phylodist, envir = envir, check.comm = TRUE)
    if(!is.null(organize.temp$stop)){
      organize.temp$call <- match.call()
      return(organize.temp)
    }
    list.warning <- organize.temp$list.warning
    comm <- organize.temp$community
    phylodist <- organize.temp$phylodist
    envir <- organize.temp$environmental
    
    # envir nao conferido
    
  }
  if(length(list.warning)>0){
    res$list.warning <- list.warning
  }
  res.pcps.null <- matrix.p.null(comm, phylodist, runs = runs, calcpcps = FALSE)
  res.pcps.null <- mutate.names.matrix.p.null(res.pcps.null, "pcps", newname)
  res$P.obs <- res.pcps.null$P.obs
  statistic.obs <- sapply(list(res.pcps.null$P.obs), FUN = FUN, simplify = FALSE, return.model = TRUE, envir = envir, ...)
  res$model <- statistic.obs[[1]]$mod.obs
  res$fun <- FUN
  res$obs.statistic <- statistic.obs[[1]]$statistic.obs
  newClusters <- FALSE
  if (is.numeric(parallel)) {
    parallel <- parallel::makeCluster(parallel, type = "PSOCK")
    newClusters <- TRUE
  }
  if (!inherits(parallel, "cluster")) {
    statistic.null.taxa <- sapply(res.pcps.null$P.null.taxa, FUN = FUN, simplify = FALSE, envir = envir, ...)
    statistic.null.site <- sapply(res.pcps.null$P.null.site, FUN = FUN, simplify = FALSE, envir = envir, ...)
  }
  else {
    statistic.null.taxa <- parallel::parLapply(parallel, res.pcps.null$P.null.taxa, fun = FUN, envir = envir, ...)
    statistic.null.site <- parallel::parLapply(parallel, res.pcps.null$P.null.site, fun = FUN, envir = envir, ...)
  }
  if (newClusters) {
    parallel::stopCluster(parallel)
  }
  res$statistic.null.site <- do.call("rbind", statistic.null.site)
  res$statistic.null.taxa <- do.call("rbind", statistic.null.taxa)
  res$p.site.shuffle <- as.vector(rbind((apply(sweep(res$statistic.null.site, 2, res$obs.statistic, ">="), 2, sum)+1)/(runs + 1)))
  res$p.taxa.shuffle <- as.vector(rbind((apply(sweep(res$statistic.null.taxa, 2, res$obs.statistic, ">="), 2, sum)+1)/(runs + 1)))
  class(res) <- "pcpssig"
  return(res)
}