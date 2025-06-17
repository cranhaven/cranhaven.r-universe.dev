plot.clikcorr <- function(x, type="l", lwd=2, col="red", ...){

	dist = x$dist
	df = x$df
    r0 = x$coefficients
    lmax = x$logLik
    ci = list(lcl=x$CI[1], ucl=x$CI[2])
	Fr = x$pairData
	cp = x$CI[3]
	

    ## Get a grid of points on which to calculate the profile likelihood.
    k1 = ci$lcl - 0.2*(ci$lcl+1)
    k2 = ci$ucl + 0.2*(1-ci$ucl)
    RG = seq(k1, k2, length.out=20)

    ## Calculate the profile likelihood.
	if(dist=="n"){
	    Q = suppressWarnings(profile_grid(Fr, RG, ...))
	}else if(dist=="t"){
		Q = suppressWarnings(profile_grid_t(Fr, RG, df=df, ...))
	}else{
		  stop(sprintf("distribution must be either n or t!!"))
	}
    Q[,2] = Q[,2] #- lmax
    
    ## Make the plot.
    plot.default(1, type="n", xlab='Correlation', ylab='Log likelihood', xlim=c(min(Q[,1]), max(Q[,1])), ylim=c(min(Q[,2]), max(Q[,2])), ...)
    rect(ci$lcl, min(Q[,2]), ci$ucl, max(Q[,2]), col="grey", border = "grey", ...)
    lines(Q[,1], Q[,2], lwd=2)
    lines(c(r0,r0), c(min(Q[,2]), max(Q[,2])), type=type, lwd=lwd, col=col, ...)
    text(r0, 0.5*(min(Q[,2])+max(Q[,2])), paste("maxlogL=",round(lmax, digits=3),"\nr0=", round(r0, digits=3), paste("\n", cp*100, "%CI_LB=", sep=""), round(ci$lcl, digits=3), paste("\n", cp*100, "%CI_UB=", sep=""), round(ci$ucl, digits=3)), ...)
    
    #title(main=ti, cex=4, font=2)

}



profile_grid <-
function(F, RG, ...) {

    ## The unconstrained log likelihood function.
    loglike = genloglike5(F)

    ## Starting values.
    sv = starting_values(F)
    sv = c(sv[1], sv[2], sv[3], sv[5])
    
    Q = NULL
    for (r in RG) {

        ## Don't calculate for points close to the domain boundaries.
        if ((r < -1) | (r > 1)) { next; }

        ## The log-likelihood function with the correlation parameter
        ## constrained to equal r.
        f = function(F) {
            if ((r>=1) | (r<=-1)) {
                return(-Inf)
            }
            G = c(F[1], F[2], F[3], r*sqrt(exp(F[3])*exp(F[4])), F[4])
            return(loglike(G))
        }
        m = optim(sv, f, control=list(fnscale=-1), ...)
        Q = rbind(Q, c(r, m$value))
    }

    return(Q)
}




profile_grid_t <-
function(F, RG, df=4, ...) {

    ## The unconstrained log likelihood function.
    loglike = genloglike5t(F, df=df)

    ## Starting values.
    sv = starting_values(F)
    sv = c(sv[1], sv[2], sv[3], sv[5])
    
    Q = NULL
    for (r in RG) {

        ## Don't calculate for points close to the domain boundaries.
        if ((r < -1) | (r > 1)) { next; }

        ## The log-likelihood function with the correlation parameter
        ## constrained to equal r.
        f = function(F) {
            if ((r>=1) | (r<=-1)) {
                return(-Inf)
            }
            G = c(F[1], F[2], F[3], r*sqrt(exp(F[3])*exp(F[4])), F[4])
            return(loglike(G))
        }
        m = optim(sv, f, control=list(fnscale=-1), ...)
        Q = rbind(Q, c(r, m$value))
    }

    return(Q)
}


