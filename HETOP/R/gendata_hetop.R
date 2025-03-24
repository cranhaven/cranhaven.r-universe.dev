##########################################################################################
## function to generate count data by group from multinomial model with the
## structured probabilities under the HETOP
##########################################################################################
gendata_hetop <- function(G, K, ng, mug, sigmag, cutpoints){
    
    if( G <= 0 ){
        stop("not enough groups")
    }
    
    if( K < 3 ){
        stop("this function required K >= 3")
    }
    
    if( !is.numeric(ng) || (length(ng) != G) || any(is.na(ng)) ){
        stop("ng must be a numeric vector of length G with no missing values")
    }
    
    if( min(ng) < 1 ){
        stop("ng must have at least one observation per group")
    }

    if( !is.numeric(mug) || (length(mug) != G) || any(is.na(mug)) ){
        stop("mug must be a numeric vector of length G with no missing values")
    }
    
    if( !is.numeric(sigmag) || (length(sigmag) != G) || any(is.na(sigmag)) ){
        stop("sigmag must be a numeric vector of length G with no missing values")
    }

    if( any(sigmag <= 0) ){
        stop("sigmag must have positive values")
    }
    
    if( !is.numeric(cutpoints) || (length(cutpoints) != (K-1)) || any(is.na(cutpoints)) ){
        stop("cutpoints must be a numeric vector of length (K-1) with no missing values")
    }

    if( any(diff(cutpoints) <= 0) ){
        stop("elements of cutpoints must be strictly increasing")
    }
    
    ## calculate cell probabilities
    pgk <- matrix(0, ncol=K, nrow=G)
    for(g in 1:G){
        tmp     <- c(pnorm(cutpoints, mean=mug[g], sd = sigmag[g]), 1)
        pgk[g,] <- c(tmp[1], diff(tmp))
    }
    stopifnot(max(abs(apply(pgk, 1, sum) - 1)) < 1e-10)
    
    ## generate and return multinomial counts
    return(t(apply(cbind(ng, pgk), 1,  function(x){ table(sample(as.factor(1:K), size=x[1], prob=x[-1], replace=T))})))
}
