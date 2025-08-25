#' Traces of MCMC iterations
#' 
#' @param fit An object of class \code{fitTK}
#' @param plots A string selecting the parameters. Defaults is \code{"all"} and select all parameters.
#' Deterministc parameters can be selected by setting \code{"deterministic"} and 
#' stochastic parameter with \code{"stochastic"}
#'
#' @return A traceplot of class \code{ggplot}.
#' 
#' @export
#'
mcmcTraces <- function(fit, plots = "all"){
  if (plots == "deterministic"){
    corr <- ggmcmc::ggs(fit[["stanfit"]], family = "^k|^g")
  } else if (plots == "stochastic") {
    corr <- ggmcmc::ggs(fit[["stanfit"]], family = "^sigma")
  } else {
    corr <- ggmcmc::ggs(fit[["stanfit"]], family = "^k|^g|^sigma")
  }
  plots <- ggmcmc::ggs_traceplot(corr, greek = TRUE) +
    scale_colour_manual(values = alpha(c("#333333", "#ee7202", "#63ad00", "#333333"), 1)) +
    theme(legend.title = element_blank())
  
  return(plots)
}