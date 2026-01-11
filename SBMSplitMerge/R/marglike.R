#' calculate the marginal likelihood for a node for samplers using conjugate models
#' @title Marginal likelihood model for Bernoulli distributed edges
#' @param znoi a matrix of block assignments without node i
#' @param ei edge-states incident to i
#' @param parammod a \code{parammod} object representing the Bernoulli-Beta model
#' @return log-probability of node i belonging to each block
#' @export
marglike_bern <- function(znoi, ei, parammod){
    ## block sizes
    nk  <- rowSums(znoi)
    mk <- c(ei %*% t(znoi))
    mk <- rbind(mk, nk - mk)
    if(!any(nk == 0))
        mk <- cbind(mk, 0)
    m0 <- c(sum(ei),sum(1-ei)) - mk
    a0 <- parammod$a0 + m0[1,]
    a1 <- parammod$a1 + m0[2,]
    b0 <- parammod$b0 + mk[1,]
    b1 <- parammod$b1 + mk[2,]
    logp <- lbeta(a0,a1) + lbeta(b0,b1)
    logp
}

#' calculate the marginal likelihood for a node for samplers using conjugate models
#' @title Marginal likelihood model for Poisson distributed edges
#' @param znoi a matrix of block assignments without node i
#' @param ei edge-states incident to i
#' @param parammod a \code{parammod} object
#' @return log-probability of node i belonging to each block
#' @export
marglike_pois <- function(znoi, ei, parammod){
    ## block sizes
    nk <- rowSums(znoi)
    mk <- rbind(ei %*% t(znoi), nk)
    if(!any(nk == 0))
        mk <- cbind(mk, 0)
    m0 <- rowSums(mk) - mk
    a0 <- parammod$a0 + m0[1,]
    a1 <- parammod$a1 + m0[2,]
    b0 <- parammod$b0 + mk[1,]
    b1 <- parammod$b1 + mk[2,]
    logp <- lgamma(b0) - b0 * log(b1) + lgamma(a0) - a0 * log(a1)
    logp
}

#' calculate the marginal likelihood for a node for samplers using conjugate models
#' @title Marginal likelihood model for Normal distributed edges
#' @param znoi a matrix of block assignments without node i
#' @param ei edge-states incident to i
#' @param parammod a \code{parammod} object
#' @return log-probability of node i belonging to each block
#' @export
marglike_norm <- function(znoi, ei, parammod){
    rho0 <- parammod$a0
    nu0  <- parammod$b0
    be0  <- parammod$c0
    al0  <- parammod$d0
    rhok <- parammod$a1
    nuk  <- parammod$b1
    bek  <- parammod$c1
    alk  <- parammod$d1
    nk   <- rowSums(znoi)
    mk   <- c(ei %*% t(znoi))
    ssk  <- c(ei^2 %*% t(znoi))
    if(!any(nk == 0)){
        nk <- c(nk, 0)
        mk <- c(mk, 0)
        ssk <- c(ssk, 0)
    }
    m0 <- sum(mk) - mk
    n0 <- sum(nk) - nk
    ss0 <- sum(ssk) - ssk
    postbek <- bek + 0.5*ssk - 0.5*mk^2/nk + nuk*(mk/nk - rhok)^2/(nuk+nk)/nk/2
    postbe0 <- be0 + 0.5*ss0 - 0.5*m0^2/n0 + nu0*(m0/n0 - rho0)^2/(nu0+n0)/n0/2
    postbek[is.nan(postbek)] <- bek
    postbe0[is.nan(postbe0)] <- be0
    logp <- 0
    logp <- logp + lgamma(alk + nk/2) - lgamma(alk)
    logp <- logp + 0.5*log(nuk) - 0.5*log(nuk + nk)
    logp <- logp - nk/2 * log(2*pi)
    logp <- logp + alk*log(bek)
    logp <- logp - (alk + nk/2)*log(postbek)
    logp <- logp + lgamma(al0 + n0/2) - lgamma(al0)
    logp <- logp + 0.5*log(nu0) - 0.5*log(nu0 + n0)
    logp <- logp - n0/2 * log(2*pi)
    logp <- logp + al0*log(be0)
    logp <- logp - (al0 + n0/2)*log(postbe0)
    logp
}
