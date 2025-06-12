#' @rdname pcps.sig
#' @include pcps.sig.R
#' @include matrix.p.sig.R
#' @encoding UTF-8
#' @export
FUN.ADONIS <- function(x, envir, method.p, sqrt.p = TRUE, formula, return.model = FALSE){
  p.dist <- vegan::vegdist(x, method = method.p)
  if(sqrt.p){
    p.dist <- sqrt(p.dist)
  }
  mod.obs <- vegan::adonis(formula, data = data.frame(envir), permutations = 0)
  statistic.obs <- mod.obs$aov.tab$F.Model[1]
  if(return.model){
    res <- list()
    res$mod.obs <- mod.obs
    res$statistic.obs <- statistic.obs
  } else{
    res <- statistic.obs
  }
  return(res)
} 