#' Correlations between parameters: colored matrix 
#' 
#' @param fit An object of class \code{fitTK}
#' 
#' @return A heatmap of class \code{ggplot}.
#' 
#' @export
#'
corrMatrix <- function(fit){
  corr <- ggmcmc::ggs(fit[["stanfit"]], family = "^k|^g|^sigma")
  levels(corr$Parameter) <- gsub("\\[|\\]", "", levels(corr$Parameter))
  plot <- ggmcmc::ggs_crosscorrelation(corr) +
    scale_fill_gradient(name = "Pearson correlations \n",low = "#ee7202", high = "#63ad00",
                        breaks=c(-1,0,1),labels=c("-1: highly - corr" ,"0: not corr","1: highly + corr"),limits=c(-1,1)) +
    scale_x_discrete(position="top")+
    scale_y_discrete(position="right") +
    theme(axis.text.x=element_text(angle = 45, hjust = 0), axis.ticks = element_blank(), aspect.ratio = 1, legend.position = "right")
  return(plot)
}

#' Correlations between parameters: pairs plot
#' 
#' @param fit An object of class \code{fitTK}
#' @param plots A string selecting the parameters. Defaults is \code{"all"} and select all parameters.
#' Deterministc parameters can be selected by setting \code{"deterministic"} and 
#' stochastic parameter with \code{"stochastic"}
#' 
#' @return A pairsplot of class \code{ggmatrix} containing planes of parameter pairs (lower triangle), marginal posterior distribution of each parameter (diagonal) and Pearson correlation coefficients (upper triangle)
#' 
#' @export
#'
corrPlot <- function(fit, plots = c("all", "deterministic", "stochastic")){
  if (match.arg(plots)=="deterministic") {
    corr <- ggmcmc::ggs(fit[["stanfit"]], family = "^k|^g")
  } else if (match.arg(plots)=="stochastic") {
    corr <- ggmcmc::ggs(fit[["stanfit"]], family = "^sigma")
  } else {
    corr <- ggmcmc::ggs(fit[["stanfit"]], family = "^k|^g|^sigma")
  }
  levels(corr$Parameter) <- gsub("\\[|\\]", "", levels(corr$Parameter))
  plot <- ggmcmc::ggs_pairs(corr, lower = list(continuous = GGally::wrap("density", color = "#ee7202")))
    #theme(aspect.ratio = 1) # make it square but mess up the axis
  return(plot)
}
