#' @title Add a block move
#' @description proposes adding an empty block labelled \code{kappa+}1 to \code{sbm}
#' @param sbm the current state of the sampler
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param rho probability of choosing to add a block
#' @return an updated \code{sbm} object
addblock <- function(sbm, edges, sbmmod, rho=1){
    nempty <- sum(sbm$blocks$sizes == 0)
    propsbm <- sbm(
        blocks(sbm$blocks$z, sbm$blocks$kappa+1) ## increment the number of blocks
       ,
        params(
            sbm$params$theta0,
            rbind(sbm$params$thetak, sbmmod$param$r(1)$thetak) ## simulate a new parameter
        )
    )
    logb <- sbmmod$block$logd(propsbm$blocks) - sbmmod$block$logd(sbm$blocks)
    logu <- log(rho + nempty) - log(rho) - log(rho + nempty + 1)
    A <- exp(logb+logu)
    if(!is.nan(A) & (stats::runif(1) < A))
        sbm <- propsbm
    sbm
}

rmblock <- function(z, k)
    blocks(factor(z, levels(z)[-k]))

#' @title Delete a block move
#' @description proposes deleting an empty block (chosen at random among empty Blocks)
#' @param sbm the current state of the sampler
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param rho probability of choosing to add a block
#' @return an updated \code{sbm} object
delblock <- function(sbm, edges, sbmmod, rho=1){
    emptyind <- sbm$blocks$sizes == 0
    nempty   <- sum(emptyind)
    k <- which(emptyind)[sample(nempty,1)]
    b <- rmblock(sbm$blocks$z, k)
    p <- params(sbm$params$theta0, sbm$params$thetak[-k, ,drop=FALSE])
    propsbm <- sbm(b,p)
    logb <- sbmmod$block$logd(propsbm$blocks) - sbmmod$block$logd(sbm$blocks)
    logu <- log(rho + nempty) + log(rho) - log(rho + nempty - 1)
    A <- exp(logb+logu)
    ## potential that A up like exp(Inf - Inf?)
    if(!is.nan(A) & (stats::runif(1) < A))
        sbm <- propsbm
    sbm
}

#' @title Merge blocks
#' @description Merge-move using an average to merge parameters
#' @details the blocks are chosen at random, the nodes reassigned to the block with the smallest index, then the parameters are combined using the average on the transformed scale
#' @param sbm the current state of the sampler
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param ... additional parameter to 'accept'
#' @return an updated \code{sbm} object
mergeavg <- function(sbm, edges, sbmmod, ...){
    kl <- rcat(2, rep(1, sbm$blocks$kappa), FALSE)
    k <- min(kl)
    l <- max(kl)
    ## merges the blocks and computes q(Z).
    mp <- mergeparams(sbm$params, k, l, sbmmod$param)
    mb <- mergeblocks(sbm$blocks, mp$prop, edges, sbmmod, k, l)
    propsbm <- sbm(mb$prop, mp$prop)
    ## deterministic
    logu <- log(sbm$blocks$kappa) - log(2) + (sbm$blocks$kappa==2)*log(2)
    accept(sbm, propsbm, edges, sbmmod, mp$loga + mb$loga, logu, ...)
}

#' split move using average to merge parameters
#' @param sbm the current state of the sampler
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param ... additional parameter to 'accept'
#' @return an updated \code{sbm} object
splitavg <- function(sbm, edges, sbmmod, ...){
    ## split a block chosen at random
    ## returns sbm
    k       <- sample(sbm$blocks$kappa, 1)
    ## propose parameters
    sp      <- splitparams(sbm$params, k, sbmmod$param)
    propparams <- sp$prop
    sb      <- splitblocks(sbm$blocks, propparams, edges, sbmmod, k)
    propblocks <- sb$prop
    propsbm <- sbm(propblocks, propparams)
    ## acceptence probability
    logu <- log(2) - log(sbm$blocks$kappa+1)  - (sbm$blocks$kappa==1)*log(2)
    accept(sbm, propsbm, edges, sbmmod, logjac=sp$loga + sb$loga, logu=logu, ...)
}

#' merge move block merging
#' @param currblocks current blocks
#' @param propparams proposed parameters
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param k Blocks to merge
#' @param l Blocks to merge
#' @return \code{list(proposed block structure, log-acceptance-prob)}
mergeblocks <- function(currblocks, propparams, edges, sbmmod, k, l){
    ## which nodes are merging?
    ind <- (currblocks$z == k) | (currblocks$z == l)
    ## calculate reverse probability of assignment
    ## copy and unassign the nodes in ind
    propblocks <- currblocks
    propblocks$z[ind] <- NA
    ## shuffle the nodes to be assigned
    nodes <- which(ind)[sample(sum(ind))]
    ## for each node, get the likelihood of it being in each block
    loga <- 0
    for(i in nodes){ # shuffled nodes
        p <- nodelike(propblocks, propparams, edges, i, sbmmod)
        p <- normaliselogs(p[c(k,l)])
        loga <- loga + log(p[currblocks$z[i] == c(k,l)])
        propblocks$z[i] <- currblocks$z[i]
    }
    ## update block assignment
    propblocks$z[ind] <- k
    propblocks <- rmblock(propblocks$z, l)
    list(prop = propblocks, loga = loga)
}

#' split move: blocks
#' @param currblocks current blocks
#' @param propparams proposed parameters
#' @param edges an \code{edges} object
#' @param sbmmod a model list
#' @param k block to split
#' @return \code{list(proposed block structure, log-acceptance-prob)}
splitblocks <- function(currblocks, propparams, edges, sbmmod, k){
    ## splits two block calculating the acceptance probability along the way
    propblocks <- blocks(currblocks$z, currblocks$kappa+1)
    ind  <- propblocks$z == k
    loga <- 0
    l <- propblocks$kappa
    if( sum(ind) > 0 ){                 # if k is not-empty
        propblocks$z[ind] <- NA
        ## calculate reverse probability of assignment
        nodes <- which(ind)[sample(sum(ind))] # shuffle nodes
        for(i in nodes){
            p <- nodelike(propblocks, propparams, edges, i, sbmmod)
            p <- normaliselogs(p[c(k,l)])
            j <- rcat(1,p)
            propblocks <- updateblock(propblocks, i, c(k,l)[j])
            loga <- loga - log(p[j])
        }
        ## make the larger block stay labelled as k
        if( propblocks$sizes[l] > propblocks$sizes[k] ){
            propz <- propblocks$z
            in_l <- propz == l
            in_k <- propz == k
            propz[in_l] <- k
            propz[in_k] <- l
            propblocks <- blocks(propz, l)
        }
    } else {
        propblocks <- blocks(currblocks$z, l)
    }
    list(prop = propblocks, loga = loga)
}

#' split move: parameters
#' @param x object for dispatch
#' @param ... additional arguments for method
#' @return \code{list(proposed_params, log-acceptance-prob)}
splitparams <- function(x, ...)
    UseMethod("splitparams", x)

#' split move: \code{params}
#' @param params a \code{params} object to split
#' @param k block to split
#' @param parammod \code{parammod} object
#' @return \code{list(proposed_params, log-acceptance-prob)}
splitparams.params <- function(params, k, parammod){
    ## split params, draw x (w in paper) uniform and u normally distributed
    x    <- stats::rbeta(params$dimtheta, 5, 5)
    u    <- stats::rnorm(params$dimtheta, 0, 1)
    loga <- sum(stats::dnorm(u, 0, 1, log=TRUE))
    l    <- params$kappa+1
    propthetak <- rbind(params$thetak,0)
    theta <- params$thetak[k,]
    tmp   <- splitparams(theta, u, x, parammod)
    loga  <- loga + tmp$loga
    propthetak[c(k,l),] <- tmp$prop
    propp <- params(params$theta0, propthetak)
    list(prop = propp, loga=loga)
}

#' split move: \code{params}
#' @param theta a parameter to split
#' @param u auxiliary variable
#' @param x auxiliary variable
#' @param parammod \code{parammod} object
#' @return \code{list(proposed_params, log-acceptance-prob)}
splitparams.numeric <- function(theta, u, x, parammod){
    ## given value for theta u and x and parameter model - perform the split to yield theta_k and theta_l
    ## returns list with proposed param structure and log-jacobian of transformation
    propk <- parammod$invt((parammod$t(theta) + u)/2/x)
    propl <- parammod$invt((parammod$t(theta) - u)/2/(1-x))
    loga <- parammod$loggradt(theta) - parammod$loggradt(propk) - parammod$loggradt(propl) - sum(log(x)) - sum(log(1-x)) - length(x) * log(2)
    prop <- rbind(propk, propl)
    list(prop = prop, loga=loga)
}

#' merge parameters
#' @param x an object to dispatch on
#' @param ... additional arguments for methods
#' @return merged parameters from \code{x}
#' @seealso \code{\link{mergeparams.default}} \code{\link{mergeparams.numeric}}
mergeparams <- function(x,...)
    UseMethod("mergeparams", x)

#' Merge step: parameters
#' @param params a \code{params object}
#' @param k Blocks to merge
#' @param l Blocks to merge
#' @param parammod a \code{parammod} object
#' @return \code{list(proposed_params, log-acceptance-prob)}
mergeparams.default <- function(params, k, l, parammod){
    ## gi
    x <- stats::runif(params$dimtheta)
    propthetak <- params$thetak
    tmp <- mergeparams(params$thetak[k,], params$thetak[l,], x, parammod)
    propthetak[k,] <- tmp$prop
    loga <- tmp$loga
    propthetak <- propthetak[-l,,drop=FALSE]
    propp <- params(params$theta0, propthetak)
    list(prop = propp, loga = loga)
}

#' Merge step - parameter merging
#' @param thetak,thetal parameters to merge
#' @param x auxiliary parameter
#' @param parammod a \code{parammod} object
#' @return \code{list(proposed_params, log-acceptance-prob)}
mergeparams.numeric <- function(thetak, thetal, x, parammod){
    ## given thetak thetal and x (w in paper) and the parameter model
    ## return the merged value theta with log jacobian value
    theta <- parammod$invt(x*parammod$t(thetak) + (1-x)*parammod$t(thetal))
    loga <- -parammod$loggradt(theta) + parammod$loggradt(thetak) + parammod$loggradt(thetal) + sum(log(x)) + sum(log(1-x)) + length(x) * log(2)
    list(prop = theta, loga=loga)
}
