###########################################################################################
## function to get MLE of (mug, sigmag, cutpoints) given first two fixed cutpoints.
## for unconstrained optimization parameters are organized as
## mug(G), log(sigmag)(G), log(cutpoint[3] - cutpoint[2), log(cutpoint[4] - cutpoint[3]) etc (K-3)
##
## Applies the following rules to deal with groups with sparse cells:
##
## 1) if group has >= 3 populated cells, estimate (mug, sigmag)
##
## 2) if a group has exactly 2 populated cells, estimate mug, and set log(sigmag)
##    = mean(log(sigmag)) of groups from case (1)
##
## 3) if a group has data in only 1 interior cell, proceed as with case (2)
##
## 4) if a group has data only in the top cell, set log(sigmag) = mean(log(sigmag))
##    of groups from case (1), and set mug = max of estimated means (cases 1+2+3)
##
## 5) if a group has data only in the bottom cell, set log(sigmag) = mean(log(sigmag))
##    of groups from case (1), and set mug = min of estimated means (cases 1+2+3)
###########################################################################################
mle_hetop <- function(ngk, fixedcuts, svals=NULL, iterlim = 1500, ...){

    ## basic checks on arguments    
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
    
    if(K <= 2){
        stop("Function requires K >= 3 categories")
    }

    if(!is.numeric(fixedcuts)){
        stop("fixedcuts must be a numeric vector of length 2")
    }
    
    if(length(fixedcuts) != 2){
        stop("fixedcuts must be a numeric vector of length 2")        
    }

    if(any(is.na(fixedcuts))){
        stop("fixedcuts cannot contain missing values")
    }

    if(fixedcuts[1] >= fixedcuts[2]){
        stop("fixedcuts[1] must be strictly less than fixedcuts[2]")
    }

    ## define metadata "pstatus" giving, for each group, the estimation status of
    ## (mug, sigmag)    
    pstatus <- as.data.frame(matrix("est", ncol=2, nrow=G), stringsAsFactors=FALSE)
    names(pstatus) <- c("mug","sigmag")
    
    pstatus$sigmag[which( apply(ngk, 1, function(x){ sum(x > 0) }) <= 2 )] <- "mean"
    pstatus$mug[which(apply(ngk, 1, function(x){ (sum(x > 0) == 1) && (x[1] > 0) }))] <- "min"
    pstatus$mug[which(apply(ngk, 1, function(x){ (sum(x > 0) == 1) && (x[K] > 0) }))] <- "max"

    n_m <- sum(pstatus$mug    == "est")
    n_s <- sum(pstatus$sigmag == "est")

    if(!( (n_m > 0) && (n_s > 0) )){
        stop("Insufficient data for estimation - too many groups with sparse counts")
    }

    n_param <- n_m + n_s + (K-3)
    if(!is.null(svals) && (length(svals) != n_param)){
        stop("length of svals inconsistent with number of estimable parameters")
    }

    if(!is.null(svals) && any(is.na(svals))){
        stop("svals contains missing values")
    }
    
    ## negative log likelihood, fixing parameters as needed
    negll <- function(param){
        .mug <- .sigmag <- rep(-99.0, G)
        .mug[which(pstatus$mug == "est")] <- param[1:n_m]
        .mug[which(pstatus$mug == "min")] <- min(param[1:n_m])
        .mug[which(pstatus$mug == "max")] <- max(param[1:n_m])

        .lsigs <- param[(n_m + 1):(n_m + n_s)]
        .sigmag[which(pstatus$sigmag == "est")]  <- exp(.lsigs)
        .sigmag[which(pstatus$sigmag == "mean")] <- exp(mean(.lsigs))
        
        if(K==3){
            .cutpoints <- fixedcuts
        } else {
            .cutpoints <- c(fixedcuts, fixedcuts[2] + cumsum(exp(param[(n_m + n_s + 1):length(param)])))
        }
        
        ## calculate cell probabilities, truncating at values very close to 0 or 1
        pgk <- matrix(0, ncol=K, nrow=G)
        for(g in 1:G){
            tmp     <- c(pnorm(.cutpoints, mean=.mug[g], sd = .sigmag[g]), 1)
            pgk[g,] <- c(tmp[1], diff(tmp))
        }
        stopifnot(max(abs(apply(pgk, 1, sum) - 1)) < 1e-8)
        pgk[which(pgk <= 1e-300)]     <- 1e-300
        pgk[which(pgk >= 1 - 1e-300)] <- 1 - 1e-300
        
        ## return negative of multinomial log likelihood based on counts "ngk"
        -sum(c(ngk)*log(c(pgk)))
    }

    ## set starting values if they are not supplied.
    ## NOTE: sd(mu) is NA is G=1 so we then just set mu = 0.
    if(is.null(svals)){
        cats <- 1:K
        mu      <- apply(ngk, 1, function(x){ weighted.mean(cats, w = x) })
        mu      <- mu[which(pstatus$mug == "est")]
        logsig  <- apply(ngk[which(pstatus$sigmag == "est"),,drop=F], 1, function(x){ log(sqrt(weighted.mean(cats^2, w = x) - (weighted.mean(cats, w = x))^2)) })
        if(G==1){
            mu <- 0.0
        } else {
            mu      <- (mu - mean(mu)) / sd(mu)
        }
        logsig  <- logsig - mean(logsig)
        svals   <- c(mu, logsig, rep(0, K-3))
    }

    ## do the optimization
    res <- nlm(f = negll, p = svals, iterlim = iterlim, ...)
    if(res$code > 1){
        warning("optimization algorithm may not have converged properly; see 'nlmdetails' element of object")
    }

    ## 1) estimates on scale with respect to fixed cutpoints.
    ## stretch back to the group level regardless of any constraints
    param <- res$estimate
    
    if(K==3){
        cutpoints <- fixedcuts
    } else {
        cutpoints <- c(fixedcuts, fixedcuts[2] + cumsum(exp(param[(n_m + n_s + 1):length(param)])))
    }

    mug <- sigmag <- rep(-99.0, G)
    mug[which(pstatus$mug == "est")] <- param[1:n_m]
    mug[which(pstatus$mug == "min")] <- min(param[1:n_m])
    mug[which(pstatus$mug == "max")] <- max(param[1:n_m])

    .lsigs <- param[(n_m + 1):(n_m + n_s)]
    sigmag[which(pstatus$sigmag == "est")]  <- exp(.lsigs)
    sigmag[which(pstatus$sigmag == "mean")] <- exp(mean(.lsigs))

    est_fc <- list(mug = mug, sigmag = sigmag, cutpoints = cutpoints)

    ## get ICC
    ng         <- as.vector(apply(ngk, 1, sum))
    pg         <- ng / sum(ng)
    est_fc$icc <- (sum(pg * mug^2) - (sum(pg * mug))^2) / (sum(pg * mug^2) - (sum(pg * mug))^2 + sum(pg * sigmag^2))    

    ## 2) estimates on scale with weighted mean equal 0 and weighted log SD = 0.
    a          <- sum(pg * est_fc$mug)
    b          <- exp(sum(pg * log(est_fc$sigmag)))
    est_zero   <- list(mug       = (est_fc$mug - a) / b,
                       sigmag    = est_fc$sigmag / b,
                       cutpoints = (est_fc$cutpoints - a) / b)
    est_zero$icc <- (sum(pg * est_zero$mug^2) - (sum(pg * est_zero$mug))^2) / (sum(pg * est_zero$mug^2) - (sum(pg * est_zero$mug))^2 + sum(pg * est_zero$sigmag^2))

    ## 3) estimates on "star" scale, with weighted mean equal 0 and marginal variance 1
    a          <- sum(pg * est_zero$mug)
    b          <- sqrt(sum(pg * ( (est_zero$mug - a)^2 + est_zero$sigmag^2)))
    est_star   <- list(mug       = (est_zero$mug - a) / b,
                       sigmag    = est_zero$sigmag / b,
                       cutpoints = (est_zero$cutpoints - a) / b)
    est_star$icc <- (sum(pg * est_star$mug^2) - (sum(pg * est_star$mug))^2) / (sum(pg * est_star$mug^2) - (sum(pg * est_star$mug))^2 + sum(pg * est_star$sigmag^2))
    if(abs(sum(pg * (est_star$mug^2  + est_star$sigma^2)) - 1) > 1e-8){
        stop("numerical problem in translating estimates to different scales")
    }
        
    ## 4) estimates on bias-corrected "star" scale.  Note that this is not defined when any of
    ## the groups have exactly one observation.  It also can lead to negative ICC estimate.
    if(any(ng <= 1)){
        est_starbc <- NULL
    } else {
        N      <- sum(ng)
        ntilde <- 1/sum((1/(ng-1))*(1/G))
        mprime <- est_zero$mug
        sprime <- est_zero$sigmag
        
        ssw_pw <- sum( (ntilde/(1+ntilde)) * pg * sprime^2 )
        ssb_pw <- sum( pg * (mprime - sum(pg * mprime) )^2 ) - sum( (ntilde/(1+ntilde)) * (1/N) * (1-pg) * sprime^2)
        b      <- sqrt(ssb_pw + ssw_pw)

        est_starbc <- list(mug       = (mprime - sum(pg*mprime)) / b,
                           sigmag    = sprime / b,
                           cutpoints = (est_zero$cutpoints - sum(pg*mprime)) / b)
        est_starbc$icc <- ssb_pw / (ssb_pw + ssw_pw)
    }
    
    ## sanity checks; all should imply the same quantiles, and ICCs should be the same
    ## except for #4
    tmp <- list(est_fc, est_zero, est_star)
    if(sd(sapply(tmp, function(x){ x$icc})) > 1e-8){
        stop("numerical problem with ICC calculation on different scales")
    }
    
    if(!any(ng <= 1)){
        tmp[[4]] <- est_starbc
    }

    tmp <- do.call("rbind",lapply(tmp, function(x){
        c(sapply(1:(K-1), function(k){ (x$mug - x$cutpoints[k]) / x$sigmag}))
    }))
    if( max(apply(tmp, 2, sd)) > 1e-8){
        stop("numerical problem in translating estimates to different scales")
    }

    ## return pieces
    return(list(est_fc     = est_fc,
                est_zero   = est_zero,
                est_star   = est_star,
                est_starbc = est_starbc,
                nlmdetails = res,
                pstatus    = pstatus))
}
