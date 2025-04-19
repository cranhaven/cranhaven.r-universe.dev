#' @name lprime
#'
#' @md
#'
#' @title Lambda prime or noncentral Lambda distribution.
#'
#' @aliases dlprime plprime qlprime
#'
#' @description
#' plprime computes the cumulative probability of the lambda-prime distribution with 
#' parameters nu, ncp. dlprime(x, nu, ncp)  returns the density of the lambda prime 
#' and distribution qlprime(p, nu, ncp) its quantiles. See
#' \insertCite{l99;textual}{CohensdpLibrary}.
#'  
#' @usage
#' plprime(x, nu, ncp) 
#' dlprime(x, nu, ncp)
#' qlprime(p, nu, ncp)
#' 
#' @param x         the score for which a probability is sought;
#' @param nu        the degree of freedom of the distribution;
#' @param ncp       the non-centrality parameter of the distribution;
#' @param p         the probability from which a quantile is requested;
#'
#' @return          The probability or quantile of a Lambda' distribution.
#'
#' @details
#' lprime are functions that compute the Lambda-prime distribution. It was shown to be the predictive  
#' distribution of a population standardized mean or standardized mean difference in between-group design
#' given an observed Cohen's dp \insertCite{l07}{CohensdpLibrary}.
#'
#' These functions are implemented from the FORTRAN source of \insertCite{pl10b;textual}{CohensdpLibrary}.
#' Note that the library sadists also implements this distribution sadists::lprime \insertCite{p20}{CohensdpLibrary}. 
#' 
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' 
#' dlprime(11.1, 9, 10.0) # 0.129447
#' plprime(11.1, 9, 10.0) # 0.7134134
#' qlprime(0.01, 9, 10.0) # 4.2453
#' 

#' @export 
dlprime <- function( x, nu, ncp) {
    if (nu < 1) stop("Degree of freedom smaller than 1 forbidden. Exiting lprime...")
    res <- .Fortran("sublprimepdf",
        as.double(x), 
        as.double(nu), 
        as.double(ncp), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        as.integer(-99),
        as.double(0.00)  )

    if (res[[6]] != 0) {
        warning( messageWier("lprime", res[[6]]) )
    }
    return( res[[7]] )
}

#' @export 
plprime <- function( x, nu, ncp) {
    if (nu < 1) stop("Degree of freedom smaller than 1 forbidden. Exiting lprime...")
    res <- .Fortran("sublprimecdf",
        as.double(x), 
        as.double(nu), 
        as.double(ncp), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        as.integer(-99),
        as.double(0.00)  )

    if (res[[6]] != 0) {
        warning( messageWier("lprime", res[[6]]) )
    }
    return( res[[7]] )
}

#' @export 
qlprime <- function( p, nu, ncp) {
    if (nu < 1) stop("Degree of freedom smaller than 1 forbidden. Exiting lprime...")
    if ( p < 0 | p > 1 ) stop("Probability must be between 0 and 1. Exiting lprime...")
    res <- .Fortran("sublprimeidf",
        as.double(p), 
        as.double(nu), 
        as.double(ncp), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        as.integer(-99),
        as.double(0.00)  )

    if (res[[6]] != 0) {
        warning( messageWier("lprime", res[[6]]) )
    }
    return( res[[7]] )
}
