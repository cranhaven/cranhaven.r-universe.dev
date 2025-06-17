clikcorr_n <-
function(data, lower1, upper1, lower2, upper2, cp=.95, starVal=NA, nlm=FALSE, ...) {
    # clikcorr constructs a confidence interval for the correlation
    # coefficients between two or more variables.  The variables may
    # be censored (left, right, or interval), or missing.  The
    # interval is constructed by inverting likelihood ratio tests.
    #
    # Args
    # data : A data frame containing the data and censoring status indicators
    # lower1: The name of the variable giving the lower bound of the first
    #         measurement (NA if missing or left censored)
    # upper1: The name of the variable giving the upper bound of the first
    #         measurement (NA if missing or left censored)
    # lower2: The name of the variable giving the lower bound of the second
    #         measurement (NA if missing or left censored)
    # upper2: The name of the variable giving the upper bound of the second
    #         measurement (NA if missing or left censored)    
    # cp : The coverage probability of the confidence interval
    #
    # Returns
    #  A list containing coefficient estimates and inferential quantities.
  
    F <- prepare_data(data, lower1, upper1, lower2, upper2)
    
    ## Get the point estimate.
    m <- suppressWarnings(estimate(F, starVal=starVal, nlm=nlm, ...))
    r_est <- m$C[1,2] / sqrt(m$C[1,1]*m$C[2,2])

    ## Get the confidence interval.
    ci <- suppressWarnings(profile_ci(data, lower1, upper1, lower2, upper2, cp, starVal=starVal, nlm=nlm, ...))

    ## Get a p-value for the null hypothesis that r=0.
    p0 <- suppressWarnings(correlation_lrt(F, 0, starVal=starVal, ...))
    
    result <- list(Cor=r_est, Cov=m$C, Mean=m$Mu, P0=p0,
                   LCL=ci$lcl, UCL=ci$ucl, Loglike=m$loglike)

    return(result)
}
