#' Function for Plotting PSC objects
#'
#' A function which visualises the data of a CFM or the combined CFM and DC data
#' for a 'psc' obecjec
#'
#' @param x an object of class 'CFM' or 'psc'
#' @param ... not used
#' @return a plot to describe the data included in the models
#' @details This function returns either density plots (continuous data) or
#' bar plots (categroical data) to describe the data in the CFM.  If an object
#' is supplied which has combied the CFM and DC (e.g. a psc object or an object
#' which has been passed through pscData()) then a comparison of the CFM and DC
#' will be supplied
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom survival Surv
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' plotCFM(gemCFM)
#' psc <- pscfit(gemCFM,e4_data,nsim=2000,nchain=1)
#' plotCFM(psc)
#' @export
plotCFM <- function(x,...){
  if(!class(x)%in%c("pscCFM","psc","pscOb")){
    x <- pscCFM(x)
  }
  ggpubr::ggarrange(plotlist=x$datavis,...)
}
