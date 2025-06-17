#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
KCYP3A <-
function(age){
    res <- 0.639*age/(2.36 + age) + 0.42
    return(res)
}
