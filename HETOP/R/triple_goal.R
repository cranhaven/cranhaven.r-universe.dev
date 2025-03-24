triple_goal <- function(s, stop.if.ties = FALSE, quantile.type = 7){
    ## INPUTS
    ##
    ## s:             (n x K) matrix of "n" posterior samples of "K" group parameters
    ## stop.if.ties:  logical; if TRUE, function stops if any units have
    ##                identical posterior mean ranks; otherwise breaks ties at random
    ## quantile.type: "type" argument to quantile function for different methods of computing quantiles

    ## function returns data frame with elements:
    ## theta_pm  = posterior mean estimates of group parameters
    ## theta_psd = posterior standard deviation estimates of group parameters
    ## theta_cb  = "Constrained Bayes" estimates using formula in Shen & Louis (1998)
    ## theta_gr  = "Triple Goal" estimates using algorithm defined in Shen & Louis (1998)
    ## rbar      = posterior means of ranks (1 = lowest)
    ## rhat      = integer ranks = rank(rbar)
    
    if(!is.matrix(s)){
        stop("s must be a matrix")
    }
    
    if(!is.numeric(s)){
        stop("s must be numeric")
    }
    
    if(any(is.na(s))){
        stop("s must not contain missing values")
    }

    if( !(quantile.type %in% 1:9) ){
        stop("quantile.type must be integer between 1 and 9")
    }
  
    K <- ncol(s)
    theta_pm  <- apply(s, 2, mean)
    etadot    <- mean(theta_pm)
    theta_psd <- apply(s, 2, sd)
    lambda_k  <- theta_psd^2
    var_pm    <- var(theta_pm)
    theta_cb  <- etadot + ( sqrt(1 + (mean(lambda_k) / var_pm)) * (theta_pm - etadot) )
    check1    <- mean(theta_cb) - etadot
    check2    <- var(theta_cb) - (mean(lambda_k) + var_pm)
    stopifnot( abs(check1) + abs(check2) < 1e-10 )
    
    rbar      <- apply(t(apply(s, 1, rank, ties.method="average")), 2, mean)
    if( any(diff(sort(rbar)) < .Machine$double.eps) && stop.if.ties ){
        stop("posterior mean ranks not unique")
    }
    rhat      <- rank(rbar, ties.method="random")
    stopifnot( all(sort(rhat) == 1:K) )

    ## get triple goal using quantile() on marginal distribution of samples.
    ## the ECDF of the marginal distribution of samples is equivalent to the ISEL estimator of G.
    ## evaluation points based on Shen & Louis (1998)
    theta_gr <- quantile(c(s), probs = (2*rhat - 1) / (2*K), names=FALSE, type = quantile.type)
    
    e         <- data.frame(index           = 1:K,
                            theta_pm        = theta_pm,
                            theta_psd       = theta_psd,
                            theta_cb        = theta_cb,
                            theta_gr        = theta_gr,
                            rbar            = rbar,
                            rhat            = rhat)
    if(!is.null(colnames(s))){
        rownames(e) <- colnames(s)
    } else {
        rownames(e) <- 1:nrow(e)
    }
    return(e)
}
