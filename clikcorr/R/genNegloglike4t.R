genNegloglike4t <-
function(F, r0, df=4) {
    # Return a function that evaluates the 4 parameter log likelihood for
    # a bivariate normal model under left censoring.
    #
    # Arguments:
    #  F: A list formatted as returned by prepare_data
    #  r0: The correlation coefficient is fixed at this value
    #
    # Returns:
    #  loglike: a function with argument F that returns the log-likelihood
    #           evaluated at F, where the mean is [F[1],F[2]], and variances
    #           are [F[3],F[4]], and the correlation coefficient is fixed at
    #           r0.

    f <- genNegloglike5t(F, df=df)

    g <- function(FF) {
        if (abs(r0) >= 1) { return(-Inf) }
	#if (FF[3]*FF[4]<=0) { return(-Inf) }
        c <- r0*sqrt(exp(FF[3])*exp(FF[4]))
        V <- c(FF[1],FF[2],FF[3],c,FF[4])
        return(f(V))
    }

    return(g)
}
