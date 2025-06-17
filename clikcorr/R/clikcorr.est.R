est <-
function(data, lower1, upper1, lower2, upper2, cp=.95, dist="n", df=4, sv=NA, nlm=FALSE, ...) {

    F <- prepare_data(data, lower1, upper1, lower2, upper2)
    
    ## Get the ci.

    if(dist=="n"){
		m <- suppressWarnings(estimate(F, starVal=sv, nlm=nlm, ...))
        ci <- suppressWarnings(profile_ci(data, lower1, upper1, lower2, upper2, cp, starVal=sv, nlm=nlm, ...))
        r_est <- m$C[1,2] / sqrt(m$C[1,1]*m$C[2,2])
        return(list(Cor=r_est, Cov=m$C, Mean=m$Mu, LCL=ci$lcl, UCL=ci$ucl))
    }else if(dist=="t"){
        m <- suppressWarnings(estimate_t(F, df=df, starVal=sv, nlm=nlm, ...))
        ci <- suppressWarnings(profile_ci_t(data, lower1, upper1, lower2, upper2, cp, df=df, starVal=sv, ...))
        r_est <- m$C[1,2] / sqrt(m$C[1,1]*m$C[2,2])
        return(list(Cor=r_est, Cov=m$C, Mean=m$Mu, LCL=ci$lcl, UCL=ci$ucl))
    }
    else{
        stop(sprintf("distribution must be either n or t!!"))
    }
}
