#' @rdname pcps.sig
#' @include pcps.sig.R
#' @include matrix.p.sig.R
#' @encoding UTF-8
#' @export
FUN.LME.marginal <- function(x, envir, formula, ..., return.model = FALSE){
  mod.obs <- nlme::lme(formula, data = data.frame(x,envir), ...)
  statistic.obs <- anova(mod.obs, type = "marginal")$"F-value"
  if(return.model){
    res <- list()
    res$mod.obs <- mod.obs
    res$statistic.obs <- statistic.obs
  } else{
    res <- statistic.obs
  }
  return(res)
}