#' @rdname pcps.sig
#' @include pcps.sig.R
#' @include matrix.p.sig.R
#' @encoding UTF-8
#' @export
FUN.RDA <- function(x, envir, return.model = FALSE){
  F.rda <- function(x){
    Chi.z <- x$CCA$tot.chi
    q <- x$CCA$qrank
    Chi.xz <- x$CA$tot.chi
    r <- nrow(x$CA$Xbar) - x$CCA$QR$rank - 1
    F.0 <- (Chi.z/q)/(Chi.xz/r)
    F.0 <- round(F.0, 12)
    return(F.0)
  }
  mod.obs <- vegan::rda(x, envir)
  statistic.obs <- F.rda(mod.obs)
  if(return.model){
    res <- list()
    res$mod.obs <- mod.obs
    res$statistic.obs <- statistic.obs
  } else{
    res <- statistic.obs
  }
  return(res)
}