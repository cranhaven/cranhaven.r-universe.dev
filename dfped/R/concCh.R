#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
concCh <-
function(age, percAlb, percAlpha1AG){
    percAlb*albAge(age) + percAlpha1AG*alpha1AGage(age)
}
