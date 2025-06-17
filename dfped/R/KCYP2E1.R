#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP2E1 <-
function(age){
    res <- 4.22*age^0.27/(7.66 + age^0.27)
    return(res)
}
