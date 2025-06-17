#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
concAd <-
function(percAlb, percAlpha1AG){
    percAlb*40 + percAlpha1AG*0.7
}
