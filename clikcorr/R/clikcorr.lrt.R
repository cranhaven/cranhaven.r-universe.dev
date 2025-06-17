lrt <-
function(data, lower1, upper1, lower2, upper2, dist="n", df=4, sv=NA, r0=0, nlm =FALSE,...) {

    F <- prepare_data(data, lower1, upper1, lower2, upper2)
    
    ## Get the ci.

    if(dist=="n"){
        m1 <- suppressWarnings(estimate(F, starVal=sv, ...))
        m0 <- suppressWarnings(estimate(F, r0, starVal=sv, ...))
        T <- 2*(m1$loglike - m0$loglike)
        pv <- 1-pchisq(T, 1)
        r_est <- m1$C[1,2] / sqrt(m1$C[1,1]*m1$C[2,2])
        return(list(Cor=r_est, m1llk=m1$loglike, m0llk=m0$loglike, P0=pv))
    }else if(dist=="t"){
        m1 <- suppressWarnings(estimate_t(F, starVal=sv, ...))
        m0 <- suppressWarnings(estimate_t(F, r0, starVal=sv, ...))
        T <- 2*(m1$loglike - m0$loglike)
        pv <- 1-pchisq(T, 1)
        r_est <- m1$C[1,2] / sqrt(m1$C[1,1]*m1$C[2,2])
        return(list(Cor=r_est, m1llk=m1$loglike, m0llk=m0$loglike, P0=pv))
    }
    else{
        stop(sprintf("distribution must be either n or t!!"))
    }
}
