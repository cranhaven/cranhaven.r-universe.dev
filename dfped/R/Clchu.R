#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
Clchu <-
function(age, w, Clad, Wad, fabs, fuAd, Fad, Eg, Eh, percCYPh){
    Cl_adult_u <- Cladu(Clad, fuAd, Fad)
    res <- Cl_adult_u*weightCYPsum(age, percCYPh)*(w/Wad)^0.75
    return(res)
}
