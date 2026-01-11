#' @title Metropolis updates by drawing parameters
#' @description Simulate parameters for the given model with a Metropolis-Hastings step
#' @details iterate through the parameters in \code{currsbm} and update.
#' @param sbm current \code{\link{sbm}} object
#' @param edges an \code{\link{edges}}
#' @param sbmmod an \code{\link{sbmmod}}
#' @param sigma parameter for \code{drawparam}
#' @return updated \code{sbm} object
drawparams <- function(sbm, edges, sbmmod, sigma=0.1){
    ## for the between-block parameter:
    if(sbm$blocks$kappa == 1){
        ## if only one block: draw from the prior
        sbm$params$theta0 <- sbmmod$param$r(0)$theta0
    } else{
        ## if more than one block: do a random walk
        propsbm <- sbm
        tmp <- rw(sbm$params$theta0, sbmmod$param, sigma)
        propsbm$params$theta0 <- c(tmp$prop)
        sbm <- accept(sbm, propsbm, edges, sbmmod, tmp$logjac)
    }
    ## for each within block parameter:
    for(k in 1:sbm$blocks$kappa){
        if(sbm$blocks$sizes[k] == 1){
            ## if only one node in the block k: draw from the prior
            sbm$params$thetak[k,] <- sbmmod$param$r(1)$thetak
        } else{
            ## if more than one node in block k: do a random walk
            propsbm <- sbm
            tmp <- rw(sbm$params$thetak[k,], sbmmod$param, sigma)
            propsbm$params$thetak[k,] <- c(tmp$prop)
            sbm <- accept(sbm, propsbm, edges, sbmmod, tmp$logjac)
        }
    }
    sbm
}

#' @title Random Walk
#' @description performs a random walk on a parameter value with a given parameter model
#' @param p a parameter
#' @param pm a \code{parammod} object
#' @param sigma - scale of random walk
#' @return \code{ist(proposed parameter, locjacobian)}
rw <- function(p, pm, sigma){
    pprime <- pm$invt(pm$t(p) + stats::rnorm(length(p), 0, sigma))
    logjac <- pm$loggradt(pprime) - pm$loggradt(p)
    list(prop = pprime, logjac=logjac)
}
