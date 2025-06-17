#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP3A4_5 <-
function(age){
    res <- 1*age^0.83/(0.31 + age^0.83)
    return(res)
}
