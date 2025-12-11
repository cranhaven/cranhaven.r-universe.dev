#' @import party

#' @export

fastcforest <- function(formula, data = list(), subset = NULL, weights = NULL,
                        controls = party::cforest_unbiased(), xtrafo = ptrafo, ytrafo = ptrafo, scores = NULL,
                        parallel = TRUE) {

  # if (!requireNamespace("party")) {
  #   stop("error : party package is not installed")
  # }
  # if (!requireNamespace("plyr")) {
  #   stop("error : plyr package is not installed")
  # }

  threads <- foreach::getDoParWorkers()

  # split data
  ntree <- controls@ntree
  Nmin <- floor(ntree/threads)
  modulus <- ntree%%threads
  split.ntree <- as.integer(c(rep(Nmin+1,modulus),rep(Nmin,threads-modulus)))

  # run in parallel
  foo <- function(x) {
    ctrl = controls
    ctrl@ntree = x
    party::cforest(formula=formula, data=data, subset=subset, weights=weights, controls=ctrl, xtrafo=xtrafo, ytrafo=ytrafo, scores=scores)
  }
  forests <- plyr::alply(split.ntree,  1, .fun=foo, .parallel=parallel, .paropts=list(.packages="party"))

  # combine/append forest
  forests[[1]]@ensemble<-unlist(lapply(forests,function(y) {y@ensemble}),recursive=F)
  forests[[1]]@where<-unlist(lapply(forests,function(y) {y@where}),recursive=F)
  forests[[1]]@weights<-unlist(lapply(forests,function(y) {y@weights}),recursive=F)

  #first forest has result
  return(forests[[1]])
}
