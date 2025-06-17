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
