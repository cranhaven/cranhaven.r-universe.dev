#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
colVars <-
function(a){
    diff <- a - matrix(colMeans(a), nrow(a), ncol(a), byrow=TRUE)
    vars <- colMeans(diff^2)*nrow(a) / (nrow(a) - 1)
    return (vars)
}
