#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP1A2 <-
function(age){
    res <- age^(1.41)/(1.13 + age^1.41)
    return(res)
}
