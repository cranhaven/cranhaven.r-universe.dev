#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
albAge <-
function(age){
    res <- 1.1287*log(age) + 33.746
    # if (age >= 27){
    # res <- 1
    #}
    return(res)
}
