##########################################################################################
## function to compute WAIC for FH-HETOP model from JAGS samples
## formula taken from Gelman et al paper
##########################################################################################
waic_hetop <- function(ngk, samps){

    if(is.null(ngk)){
        stop("ngk not specified")
    }

    if(is.null(samps)){
        stop("samps not specified")
    }

    if(!is.numeric(ngk)){
        stop("ngk must be a GxK numeric matrix of category counts")
    }
    
    if(!is.matrix(ngk)){
        stop("ngk must be a GxK numeric matrix of category counts")
    }

    if(any(is.na(ngk))){
        stop("ngk cannot contain missing values")
    }

    if(any(ngk < 0)){
        stop("ngk must contain only non-negative values")
    }

    if(any(apply(ngk, 1, sum) <= 0)){
        stop("ngk contains at least one row with insufficient data")
    }

    G <- nrow(ngk)
    K <- ncol(ngk)
    
    if(!is.numeric(samps)){
        stop("samps must be matrix of posterior samples for group means, groups SDs and cutpoints")
    }
    
    if(!is.matrix(samps)){
        stop("samps must be matrix of posterior samples for group means, groups SDs and cutpoints")        
    }

    if(any(is.na(samps))){
        stop("samps cannot contain missing values")
    }

    ## get blocks of parameter samples
    mug <- samps[,grep("mu",colnames(samps))]
    stopifnot(ncol(mug) == G)
    sigmag <- samps[,grep("sigma",colnames(samps))]
    stopifnot(ncol(sigmag) == G)
    cuts <- samps[,grep("cuts",colnames(samps))]
    stopifnot(ncol(cuts) == (K-1))

    ## compute (nsim x G) matrix of log likelihoods
    ll <- t(sapply(1:nrow(samps), function(s){
        pgk <- matrix(0, ncol=K, nrow=G)
        for(g in 1:G){
            tmp     <- c(pnorm(cuts[s,], mean=mug[s,g], sd = sigmag[s,g]), 1)
            pgk[g,] <- c(tmp[1], diff(tmp))
        }
        pgk[which(pgk <= 1e-300)]     <- 1e-300
        apply(ngk * log(pgk), 1, sum)
    }))
    
    lpd_hat <- sum(log(apply(ll, 2, function(logparts){
        m <- max(logparts)
        exp(m) * mean(exp(logparts - m))
    })))

    phat_waic <- sum(apply(ll, 2, var))
    
    return(list(lpd_hat = lpd_hat, phat_waic = phat_waic, waic = -2 * (lpd_hat - phat_waic)))
}
