#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP2B6 <-
function(age){
    res <- 1.07*age/(1.31 + age)
    return(res)
}
