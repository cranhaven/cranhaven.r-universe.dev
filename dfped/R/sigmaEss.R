#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
sigmaEss <-
function(mStar, sigma, Mmin, Mmax, meana, c, wm, Tmc){
    n_sigma <- length(sigma)
    res <- data.frame(matrix(NA, ncol = 2, nrow = n_sigma))
    res[,1] <- sigma
    
    for(i in 1:n_sigma){
        sigma_i <- res[i,1]
        res[i,2] <- ess(sigma_i, Mmin, Mmax, meana, c, wm, Tmc)
    }
    
    index <- min(which(res[,2] == mStar))
    sig_ess <- res[index, 1]
    
    return(sig_ess)
}
