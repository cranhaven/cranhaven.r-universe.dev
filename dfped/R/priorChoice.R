#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
priorChoice <-
function(tox, givenDose, skeletonTox, lesb){
    f <- function(beta, y, d, wm, l, u){
        prod(wm[d]^(exp(beta)*y)*(1-wm[d]^exp(beta))^(1-y))
    }
    beta_H1 <- runif(100000, lesb[1], lesb[2])
    p_H1 <- 1/100000*sum(sapply(beta_H1, f, y = tox, d = givenDose, wm = skeletonTox[,1]))
    beta_H2 <- runif(100000, lesb[2], lesb[length(lesb)-1])
    p_H2 <-  1/100000*sum(sapply(beta_H2, f, y = tox, d = givenDose, wm = skeletonTox[,1]))
    beta_H3 <- runif(100000, lesb[length(lesb)-1], lesb[length(lesb)])
    p_H3 <-  1/100000*sum(sapply(beta_H3, f, y = tox, d = givenDose, wm = skeletonTox[,1]))
    post_H3 <- p_H3/(p_H1 + p_H2 + p_H3)
    if(post_H3 > 0.61){
        return(TRUE)
    }else{
        return(FALSE)
    }
}
