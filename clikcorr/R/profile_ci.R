profile_ci <-
function(data, lower1, upper1, lower2, upper2, cp, starVal=NA, nlm=FALSE, ...) {
    # Generate a profile likelihood confidence interval for the
    # correlation coefficient between two random variables. 
    #
    # Arguments:
    # cp : target coverage probability
    #
    # Returns:
    # The lower and upper limits of the confidence interval.

    F <- prepare_data(data, lower1, upper1, lower2, upper2)
    
    ## Get the MLE.
    B <- estimate(F, starVal=starVal, nlm=nlm, ...)
    r0 <- B$C[1,2] / sqrt(B$C[1,1]*B$C[2,2])
    
    ## The unconstrained log likelihood function.
    loglike <- genloglike5(F)

    ## Starting values.
    if (is.na(starVal)) {
	  sv <- starting_values(F)
	} else {
          sv <- starVal
	}
    sv <- c(sv[1], sv[2], sv[3], sv[5])

    ## The quantile to get the desired coverage probability.
    q <- qchisq(cp, 1)/2

    ## The profile log likelihood function, translated so that the
    ## zero crossings are the endpoints of the confidence interval.
    g <- function(r, nlm=FALSE, ...) {
        if(nlm==TRUE){
          f <- genNegloglike4(F, r)
          m <- nlm(f, sv, hessian = TRUE, ...) 
          tmpllk <- -m$minimum
          sv <- c(m$estimate[1], m$estimate[2], m$estimate[3], m$estimate[4])
        }else{
          f <- genloglike4(F, r)
          m <- try(optim(sv, f, control=list(fnscale=-1), ...), silent=TRUE)
          tmpllk <- m$value
          sv <- c(m$par[1], m$par[2], m$par[3], m$par[4])
        }
        return(B$loglike - tmpllk - q)
    }
   
    llb <- -1
    r1 <- try(bracket(g, r0, llb, nlm=nlm, ...), silent=TRUE)
    while (class(r1) == "try-error"){	
	    llb <- llb/2
	    r1 <- try(bracket(g, r0, llb, nlm=nlm, ...), silent=TRUE)
     }
    lcl <- uniroot(g, c(r1, r0))

    rlb <- 1    
    r2 <- try(bracket(g, r0, rlb, nlm=nlm, ...), silent=TRUE)
    while (class(r2) == "try-error"){	
	    rlb <- rlb/2
	    r2 <- try(bracket(g, r0, rlb, nlm=nlm, ...), silent=TRUE)
     }
    ucl <- uniroot(g, c(r0, r2))

    return(list(lcl=lcl$root, ucl=ucl$root))
}
