get_bugs_wishart_scalemat <- function(target, nsim=25000, reltol = 0.05, quietly=TRUE){
    ## NOTE: bugs parameterization of wishart has prior mean equal to
    ## bugs.df * solve(bugs.scalemat), see
    ## http://statacumen.com/2009/07/02/wishart-distribution-in-winbugs-nonstandard-parameterization/
    ## which means that our final result needs to be the inverse of whatever scalemat we locate
    stopifnot( all(target > 0) && all(!is.na(target)) && (reltol > 0) && (nsim > 0) )
    bugs.df  <- length(target) + 1
    scalemat <- diag(length(target))
        
    done <- FALSE
    while(!done){
        .scalemat <- scalemat
        if(!quietly){
            print(diag(.scalemat))
        }
        
        ## generate "nsim" *covariance* matrices from the wishart
        simV <- rWishart(nsim, df = bugs.df,  Sigma = scalemat)
        simV <- lapply(1:nsim, function(i){ solve(simV[,,i]) })
        
        ## get the medians of the diagonals and compare to target
        d <- apply(do.call("rbind",lapply(simV, diag)), 2, median) - target
        absdif <- abs( d / target )
        done <- all( absdif < reltol )
        ## if we are too big, need to increase diagonals, if we are too small, need to reduce
        diag(scalemat) <- diag(scalemat) * ifelse(d > 0, (1 + absdif), 1 / (1 + absdif))
    }
    
    ## get summaries of variances and correlations
    .V <- do.call("rbind",lapply(simV, diag))
    .R <- do.call("rbind",lapply(simV, function(x){ .tmp <- diag(1/sqrt(diag(x))) %*% x %*% diag(1/sqrt(diag(x))); .tmp[lower.tri(.tmp)] }))
    colnames(.V) <- paste("var",1:ncol(.V), sep="")
    colnames(.R) <- paste("cor",1:ncol(.R), sep="")
    varsum       <- apply(.V, 2, quantile, prob = c(0.025, 0.05, 0.25, 0.50, 0.75, 0.95, 0.975))
    corsum       <- apply(.R, 2, quantile, prob = c(0.025, 0.05, 0.25, 0.50, 0.75, 0.95, 0.975))
    if(!quietly){
        print(varsum, digits=4)
        print(corsum, digits=4)
    }
    
    return(list(bugs.df = bugs.df, bugs.scalemat = solve(.scalemat), varsum = varsum, corsum = corsum))
}
## test
## get_bugs_wishart_scalemat(target = c(10,4,4,8), nsim = 20000, reltol = 0.02, quietly=FALSE)
## get_bugs_wishart_scalemat(target = c(1,2,10,3), nsim = 30000, reltol = 0.01, quietly=FALSE)
