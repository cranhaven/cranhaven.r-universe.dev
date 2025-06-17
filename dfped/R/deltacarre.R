#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
deltacarre <-
function(m, meana, sigma, c, wm, Tmc){
    print(m)
    K <- length(wm)
    mean_a <- meana
    mu <- log(meana) - sigma^2/2
    
    Dp <- -1/(sigma^2)
    print(Dp)
    
    Dq_part1 <- -1/(c*sigma^2)
    dq_part2 <- c()
    
    a_T <- rnorm(Tmc, mu, sigma)
    for(t in 1:Tmc){
        x_M_t <- sample(1:K, m, replace = TRUE)
        prob_M_t <- wm[x_M_t]^exp(a_T[t])
        Y_M_t <- sapply(prob_M_t,rbinom, n = 1, size = 1)
        dp_p2_t <- sum(log(wm[x_M_t])*mean_a*(Y_M_t - (1-Y_M_t)*(wm[x_M_t]^mean_a/(1-wm[x_M_t]^mean_a)^2)*(1+mean_a*log(wm[x_M_t])-wm[x_M_t]^mean_a)))
        dq_part2 <- c(dq_part2, dp_p2_t)
    }
    Dq <-  Dq_part1 + (1/Tmc)*sum(dq_part2)
    print((1/Tmc)*sum(dq_part2))
    
    deltacarre <- (Dp - Dq)^2
    print(deltacarre)
    return(deltacarre)
}
