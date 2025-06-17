#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
ess <-
function(sigma, Mmin, Mmax, meana, c, wm, Tmc){
    print(sigma)
    m_vect <- seq(Mmin, Mmax)
    res <- sapply(m_vect, FUN = deltacarre, meana = meana, sigma = sigma, c = c, wm = wm, Tmc = Tmc)
    print(res)
    ess <- m_vect[which(res == min(res))]
    print(ess)
    sigma_write <- sigma*100
    return(ess)
}
