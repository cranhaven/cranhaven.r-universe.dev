#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
alpha1AGage <-
function(age){
    res <- 0.887*age^0.38/(8.89^0.38 + age^0.38)
}
