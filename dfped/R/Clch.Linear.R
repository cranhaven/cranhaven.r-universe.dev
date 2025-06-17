#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
Clch.Linear <-
function(age, w, Clad, Wad){
    Clad*(w/Wad)
}
