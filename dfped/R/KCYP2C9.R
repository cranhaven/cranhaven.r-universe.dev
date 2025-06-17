#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP2C9 <-
function(age){
    res <- 0.821*age/(0.01 + age) + 0.21
    return(res)
}
