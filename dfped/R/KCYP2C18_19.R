#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP2C18_19 <-
function(age){
    res <- 0.857*age/(0.99 + age) + 0.23
    return(res)
}
