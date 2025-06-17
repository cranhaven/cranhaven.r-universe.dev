#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP2D6 <-
function(age){
    res <- 1.01*age/(0.101 + age) + 0.036
    return(res)
}
