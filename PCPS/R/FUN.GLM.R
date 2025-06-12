#' @rdname pcps.sig
#' @include pcps.sig.R
#' @include matrix.p.sig.R
#' @encoding UTF-8
#' @export
FUN.GLM <- function(x, envir, formula, ..., return.model = FALSE){
  mod.obs <- stats::glm(formula, data = data.frame(x, envir), ...)
  statistic.obs <- as.vector(stats::summary.lm(mod.obs)$fstatistic[1])
  if(return.model){
    res <- list()
    res$mod.obs <- mod.obs
    res$statistic.obs <- statistic.obs
  } else{
    res <- statistic.obs
  }
  return(res)
}