#' @rdname pcps.sig
#' @include pcps.sig.R
#' @include matrix.p.sig.R
#' @encoding UTF-8
#' @export
FUN.MANTEL <- function(x, envir, method.p, method.envir, sqrt.p = TRUE, ..., return.model = FALSE){
  env.dist <- vegan::vegdist(envir, method = method.envir)
  p.dist <- vegan::vegdist(x, method = method.p)
  if(sqrt.p){
    p.dist <- sqrt(p.dist)
  }
  mod.obs <- vegan::mantel(p.dist, env.dist, permutations = 0, ...)
  statistic.obs <- mod.obs$statistic
  if(return.model){
    res <- list()
    res$mod.obs <- mod.obs
    res$statistic.obs <- statistic.obs
  } else{
    res <- statistic.obs
  }
  return(res)
}