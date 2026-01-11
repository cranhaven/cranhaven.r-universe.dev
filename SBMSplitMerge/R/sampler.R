#' @title top level sampler function
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param nSteps number of steps to run sampler
#' @param algorithm choice of algorithm options are: \code{"conjugate", "gibbs", "dp", "rj"}
#' @param sigma random walk parameter for \code{theta}
#' @param statusfreq print the elapsed number of iterations every \code{statusfreq} iterations
#' @param currsbm initial state for \code{sbm} object (optional - one is drawn from \code{sbmmod} if not supplied)
#' @param ... additional parameters to pass to step
#' @return \code{postz} traces for block assignments \code{z}
#' @return \code{postt} traces for \code{theta}
#' @return \code{postk} traces for number of blocks \code{kappa}
#' @return \code{postn} traces for number of occupied blocks
#' @return \code{nsteps} number of iterations of chain
#' @return \code{algorithm} choice
#' @examples
#' ## see vignette("Weibull-edges")
#' @export
sampler <- function(edges, sbmmod, nSteps=1000, algorithm="rj",
                    sigma=0.5, statusfreq, currsbm, ...){
    if(missing(statusfreq))
        statusfreq <- nSteps

    step <- switch(
        algorithm ,
        "conjugate" = sampler.conj,
        "gibbs" = sampler.gibbs,
        "dp" = sampler.dp,
        "rj" = sampler.rj
    )

    if(algorithm=="conjugate")
        if(!is.function(sbmmod$marglike))
            stop("Please set a marginal likelihood function in sbmmod$marglike")

    if(algorithm == "gibbs" & !sbmmod$block$fixkappa)
        stop("Can't use the gibbs sampler unless block sbmmodel in sbmmod has a fixed kappa")

    N <- edges$numnodes
    if(missing(currsbm))
        currsbm <- sbmmod$r(N, FALSE)
    postz <- array(NA, c(N, nSteps))
    postt <- array(NA, c(currsbm$params$dimtheta, N+1, nSteps))
    postn    <- array(NA, c(N, nSteps))
    posttbar <- rep(NA, nSteps)
    posttvar <- rep(NA, nSteps)
    postl <- rep(NA, nSteps)
    postk <- rep(NA, nSteps)

    ## store sample 1
    postz[,1]   <- currsbm$blocks$z
    postt[,1,1] <- currsbm$params$theta0
    postt[,1+1:currsbm$blocks$kappa,1] <- t(currsbm$params$thetak)
    postk[1] <- currsbm$blocks$kappa
    postl[1] <- sbmmod$logd(currsbm, edges)
    postn[1:currsbm$blocks$kappa,1] <- currsbm$blocks$sizes

    for(s in 2:nSteps){
        currsbm <- step(currsbm, edges, sbmmod, sigma=sigma, ...)
        ## store
        postk[s] <- currsbm$blocks$kappa
        postl[s] <- sbmmod$logd(currsbm, edges)
        postz[,s]   <- currsbm$blocks$z
        postt[,1,s] <- currsbm$params$theta0
        postt[,1+1:currsbm$blocks$kappa,s] <- t(currsbm$params$thetak)
        postn[1:currsbm$blocks$kappa,s] <- currsbm$blocks$sizes
        if( (s %% statusfreq) == 0) message(s, "\n")
    }

    list(
        postz=postz, postl=postl, postt=postt, postk=postk,
        postn=postn, nsteps=nSteps, algorithm=algorithm
    )
}

#' accept \code{propsbm} with the acceptance probability alpha
#' @param currsbm current \code{sbm} state
#' @param propsbm proposed \code{sbm} state
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param logjac log Jacobian of transformation of variables
#' @param logu log density for auxiliary variables
#' @param ... additional arguments to pass to \code{dedges}
#' @return updated \code{sbm} object
accept <- function(currsbm, propsbm, edges, sbmmod, logjac=0, logu=0, ...){
    ll <- sbmmod$logd(propsbm, edges, ...) - sbmmod$logd(currsbm, edges, ...)
    A <- exp(ll + logjac + logu)
    ## potential that A looks like exp(Inf - Inf) - reject if A is NaN
    if(!is.nan(A) & (stats::runif(1) < A))
        currsbm <- propsbm
    currsbm
}

#' Conjugate model sampler
#' @param currsbm the current state of the sampler
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param sigma unused
#' @param ... additional arguments for \code{sbmmod$marglike}
#' @return next state of \code{currsbm} object
#' @note If using the CRP as the block model, then this is the IRM sampler of Schmidt or Morup (Schmidt, M.N. and Morup, M., 2013. Nonparametric Bayesian modeling of complex networks: An introduction. IEEE Signal Processing Magazine, 30(3), pp.110-128.)
#' @examples
#' model <- sbmmod(crp(3), param_beta(1,1,1,1), edges_bern(), marglike=marglike_bern)
#' trueSBM <- model$r(100)
#' Edges <- redges(trueSBM, model$edge)
#' out <- sampler(Edges, model, 10, "conjugate")
#' @export
sampler.conj <- function(currsbm, edges, sbmmod, sigma=NULL, ...){
    for(i in 1:currsbm$numnodes){
        znoi <- blockmat(currsbm$blocks)[,-i,drop=FALSE]
        ei   <- edges$E[i,-i]
        p <- sbmmod$marglike(znoi, ei, sbmmod$param, ...)
        if(sbmmod$block$fixkappa)
            p <- p[1:currsbm$blocks$kappa]
        q <- sbmmod$block$dcond(currsbm$blocks, i)
        p <- normaliselogs(p+q)
        newblock <- rcat(1,p)
        currsbm <- updateblock.sbm(currsbm, i, newblock, sbmmod)
    }
    currsbm
}

#' Gibbs sampling for node assignments
#' @param currsbm the current state of the sampler
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param sigma random walk parameter for theta
#' @return next state of \code{currsbm} object
#' @note This requires a block model with a fixed kappa
#' @examples
#' model <- sbmmod(multinom(1, 3), param_gamma(1,1,1,1), edges_pois())
#' trueSBM <- model$r(10)
#' Edges <- redges(trueSBM, model$edge)
#' gibbs_out <- sampler(Edges, model, algorithm="gibbs", 10, sigma=0.1)
#' eval_plots(gibbs_out)
#' @export
 sampler.gibbs <- function(currsbm, edges, sbmmod, sigma){
    stopifnot(sbmmod$block$fixkappa)
    currsbm <- drawblocks.gibbs(currsbm, edges, sbmmod)
    currsbm <- drawparams(currsbm, edges, sbmmod, sigma)
    currsbm
}

#' Dirichlet process sampler
#' @param currsbm the current state of the sampler
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param sigma random walk parameter for theta
#' @return next state of \code{currsbm} object
#' @seealso For full algorithm details see \url{http://doi.org/10.17635/lancaster/thesis/296}
#' @examples
#' model <- sbmmod(crp(4), param_norm(0,0,1,1,3,3,1,1), edges_norm())
#' trueSBM <- model$r(100)
#' Edges <- redges(trueSBM, model$edge)
#' dp_out <- sampler(Edges, model, 25, "dp", sigma=0.1)
#' @export
sampler.dp <- function(currsbm, edges, sbmmod, sigma){
    currsbm <- drawblocks.dp(currsbm, edges, sbmmod)
    currsbm <- drawparams(currsbm, edges, sbmmod, sigma)
    currsbm
}

#' reversible jump Markov chain Monte Carlo split-merge sampler
#' @param currsbm the current state of the sampler
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} model
#' @param sigma random walk parameter for \code{theta}
#' @param rho propensity to add a block
#' @return next state of \code{currsbm} object
#' @seealso For full algorithm details see \url{http://doi.org/10.17635/lancaster/thesis/296}
#' @examples
#' model <- sbmmod(dma(1,10), param_nbin(1,1,4,4,0.5,0.5,0.5,0.5), edges_nbin())
#' trueSBM <- model$r(100)
#' Edges <- redges(trueSBM, model$edge)
#' rj_out <- sampler(Edges, model, 10, "rj", sigma=0.1)
#' @export
sampler.rj <- function(currsbm, edges, sbmmod, sigma, rho=10){
    currsbm <- drawparams(currsbm, edges, sbmmod, sigma=sigma)
    if( stats::runif(1) < 0.5 | currsbm$blocks$kappa == 1 ){
        currsbm <- splitavg(currsbm, edges, sbmmod)
    } else{
        currsbm <- mergeavg(currsbm, edges, sbmmod)
    }
    emptyblocks <- sum(currsbm$blocks$sizes==0)
    pdel <- emptyblocks/(emptyblocks+rho)
    if( stats::runif(1) < pdel ){
        currsbm <- delblock(currsbm, edges, sbmmod, rho=rho)
    } else{
        currsbm <- addblock(currsbm, edges, sbmmod, rho=rho)
    }
    currsbm <- drawblocks.gibbs(currsbm, edges, sbmmod)
    currsbm
}
