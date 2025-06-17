#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
Fch <-
function(age, fabs, Eg, Eh, percCYPg, percCYPh){
    res <- fabs*(1 - Eg*weightCYPsum(age, percCYPg))*(1 - Eh*weightCYPsum (age, percCYPh))
    return(res)
}
