#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
weightCYPsum <-
function(age, percCYP){
    res <- 0
    for (i in 1:length(percCYP)){
        inter <- get(paste("K",colnames(percCYP[i]), sep = ""))(age)*percCYP[1,i]
        res <- res + inter
    }
    return(res)
}
