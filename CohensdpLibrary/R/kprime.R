#' @name kprime
#'
#' @md
#'
#' @title The K' or non-central K distribution.
#'
#' @aliases dkprime pkprime qkprime
#'
#' @description
#' The K' distribution was created to solve various problems in linear model statistics.
#' pkprime returns the cumulative probability of the lambda prime distribution with 
#' parameters nu1, nu2, ncp; dkprime returns its density and qkprime, a quantile.
#' \insertCite{l99,pl10;textual}{CohensdpLibrary}.
#'
#' @usage
#' pkprime(x, nu1, nu2, ncp) 
#' dkprime(x, nu1, nu2, ncp) 
#' qkprime(p, nu1, nu2, ncp) 
#'  
#' @param x         the value from which a probability is sought;
#' @param nu1       the first degree of freedom;
#' @param nu2       the second degree of freedom;
#' @param ncp       the noncentrality parameter;
#' @param p         the probability from which a quantile is requested;
#'
#' @return          The probability or quantile of a K' distribution.
#'
#' @details
#' kprime is a (p,d,q) set of functions that compute the K-prime distribution. This distribution has many applications,
#' including to obtain the sampling distribution of _r_ given a population rho and the predictive distributions
#' of rho given a sample _r_. See \insertCite{l99,pl10;textual}{CohensdpLibrary}.
#'
#' These functions are herein implemented from the FORTRAN source code of \insertCite{pl10b;textual}{CohensdpLibrary}.
#' Note that the library _sadists_ also implements this distribution \insertCite{p20}{CohensdpLibrary}. 
#' However, the sadists::kprime distribution is inaccurate for small nu1 or small nu2.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' 
#' dkprime(11.1, 9, 8, 10.0)  # 0.09410193
#' pkprime(11.1, 9, 8, 10.0)  # 0.606652
#' qkprime(0.01, 9, 8, 10.0)  # 3.875234
#' 

#' @export 
dkprime <- function( x, nu1, nu2, ncp) {
    if (nu1<0 | nu2<0) stop("Negative degrees of freedom forbidden. Exiting kprime...")
    res <- .Fortran("subkprimepdf",
        as.double(x), 
        as.double(nu1), 
        as.double(nu2), 
        as.double(ncp), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        as.integer(-99),
        as.double(0.00)  )

    if (res[[7]] != 0) {
        warning( messageWier("kprime", res[[7]]) )
    }
    return( res[[8]] )
}

#' @export
pkprime <- function( x, nu1, nu2, ncp) {
    if (nu1<0 | nu2<0) stop("Negative degrees of freedom forbidden. Exiting kprime...")
    res <- .Fortran("subkprimecdf",
        as.double(x), 
        as.double(nu1), 
        as.double(nu2), 
        as.double(ncp), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        as.integer(-99),
        as.double(0.00)  )

    if (res[[7]] != 0) {
        warning( messageWier("kprime", res[[7]]) )
    }
    return( res[[8]] )
}

#' @export
qkprime <- function( p, nu1, nu2, ncp) {
    if ( nu1<0 | nu2<0 ) stop("Negative degrees of freedom forbidden. Exiting kprime...")
    if ( p < 0 | p > 1 ) stop("Probability must be between 0 and 1. Exiting kprime...")
    res <- .Fortran("subkprimeidf",
        as.double(p), 
        as.double(nu1), 
        as.double(nu2), 
        as.double(ncp), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        as.integer(-99),
        as.double(0.00)  )

    if (res[[7]] != 0) {
        warning( messageWier("kprime", res[[7]]) )
    }
    return( res[[8]] )
}
