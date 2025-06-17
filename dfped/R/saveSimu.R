#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
saveSimu <-
function(saveName, d, alpha, beta, ttt, varphi, psi, p, nbSubjectsTreated){
    res <- data.frame(d, alpha, beta, nbSubjectsTreated, ttt, varphi, psi, p)
    nalpha <- length(alpha[1, ])
    nbeta <- length(beta[1, ])
    nvarphi <- length(varphi[1, ])
    npsi <- length(psi[1, ])
    np <- length(p[1, ])
    colnames(res) <- c("d", paste("alpha", 1:nalpha, sep = ""), paste("beta", 1:nbeta, sep = ""),
                       "nbTreated", "ttt", paste("varphi", 1:nvarphi, sep = ""),
                       paste("psi", 1:npsi, sep = ""), paste("p", 1:np, sep = ""))
    assign(saveName,res)
    get(saveName)
    save(list = saveName, file = paste(saveName,".RData", sep = ""))
}
