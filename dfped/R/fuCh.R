#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
fuCh <-
function(age, fuAd, percAlb, percAlpha1AG){
    concChildren <- concCh(age, percAlb, percAlpha1AG)
    concAdult <- concAd(percAlb, percAlpha1AG)
    res <- 1/(1 + (1 - fuAd)*concChildren/(fuAd*concAdult))
    return(res)
}
