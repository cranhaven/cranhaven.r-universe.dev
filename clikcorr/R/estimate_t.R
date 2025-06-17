estimate_t <-
function(F, r0=NA, df=4, starVal=NA, nlm=FALSE, ...) {
    
    # Calculate the MLE for either the 4 or 5 parameter bivariate
    # normal model, allowing for left, right, and interval censoring,
    # and missing data.  If r0 is not NA, the 4 parameter model is fit
    # with the correlation equal to r0.  Otherwise, the 5 parameter
    # model is fit.
    #
    # Arguments
    #  F: A list, formatted as returned by prepare_data
    #  r0: The fixed value of the correlation parameter
    #
    # Returns:
    #  A list containing the mean parameters, the covariance matrix, and
    #  the likelihood at the MLE.

    if (is.na(starVal)) {
	  sv <- starting_values(F)
	} else {
          sv <- starVal
	}


    if (is.na(r0)) {
        if(nlm==TRUE){
	     loglike <- genNegloglike5t(F, df=df)
             m <- nlm(loglike, sv, hessian = TRUE, ...)
             Mu <- m$estimate[1:2]
             C <- m$estimate[3:5]
             C <- array(c(exp(C[1]),C[2],C[2],exp(C[3])), c(2,2))
             tmpllik=-m$minimum
        }else{
	     loglike <- genloglike5t(F, df=df)
             m <- optim(sv, loglike, control=list(fnscale=-1), ...)

             Mu <- m$par[1:2]
             C <- m$par[3:5]
             C <- array(c(exp(C[1]),C[2],C[2],exp(C[3])), c(2,2))
	     tmpllik=m$value
	}
    } else {
        sv <- c(sv[1], sv[2], sv[3], sv[5])
        if(nlm==TRUE){
	     loglike <- genNegloglike4t(F, r0, df=df)
             m <- nlm(loglike, sv, hessian = TRUE, ...)
             Mu <- m$estimate[1:2]
             cv <- r0*sqrt(exp(C[1])*exp(C[2]))
             C <- array(c(exp(C[1]),cv,cv,exp(C[2])), c(2,2))
             tmpllik=-m$minimum
        }else{
             loglike <- genloglike4t(F, r0, df=df)
             m <- optim(sv, loglike, control=list(fnscale=-1), ...)

             Mu <- m$par[1:2]
             C <- m$par[3:4]
             cv <- r0*sqrt(exp(C[1])*exp(C[2]))
             C <- array(c(exp(C[1]),cv,cv,exp(C[2])), c(2,2))
	     tmpllik=m$value
        }
    }
    return(list(Mu=Mu, C=C, loglike=tmpllik))
}
