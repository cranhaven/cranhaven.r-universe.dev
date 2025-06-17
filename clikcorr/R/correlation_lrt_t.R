correlation_lrt_t <-
function(F, r0, df=4, starVal=NA, nlm=FALSE, ...) {
    # correlation_lrt returns the p-value for the likelihood ratio
    # test of the null hypothesis that the correlation coefficient is
    # r0, relative to an unconstrained alternative.
    #
    # Arguments
    #  F: A list formatted as returned by prepare_data
    #  r0: The null value of the correlation parameter
    #
    # Returns
    #  The p-value for the likelihood ratio test
    
    m1 <- estimate_t(F, df=df, starVal=starVal, nlm=nlm, ...)
    m0 <- estimate_t(F, r0, df=df, starVal=starVal, nlm=nlm, ...)
    T <- 2*(m1$loglike - m0$loglike)
    pv <- 1-pchisq(T, 1)
    return(pv)
}
