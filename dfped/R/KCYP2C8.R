#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP2C8 <-
function(age){
    res <- 0.716*age/(0.02 + age) + 0.3
    return(res)
}
