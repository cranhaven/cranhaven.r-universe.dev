#' @name pilsecond
#'
#' @md
#'
#' @title prior-informed Lambda second distribution.
#'
#' @aliases dpilsecond ppilsecond qpilsecond
#'
#' @description
#' This distribution extend the lambda second distribution introduced in 
#' \insertCite{c22b;textual}{CohensdpLibrary} as the exact solution to the predictive 
#' distribution of the Cohen's dp in repeated-measure when the population correlation is known.
#' A more elegant notation was provided in \insertCite{l22;textual}{CohensdpLibrary}. 
#' The prior-informed lambda prime is a bayesian extension to the lambda prime distribution
#' in the case where the population rho is not known. It is then replaced by a prior
#' which indicates the probability of a certain rho given the observed correlation r.
#'
#' @usage
#' ppilsecond(delta, n, d, r) 
#' dpilsecond(delta, n, d, r) 
#' qpilsecond(p,     n, d, r) 
#'  
#' @param delta     the parameter of the population whose probability is to assess;
#' @param n         the sample size n
#' @param d         the observed d_p of the sample;
#' @param r         the sample correlation 
#' @param p         the probability from which a quantile is requested
#'
#' @return          The probability or quantile of a prior-informed Lambda'' distribution.
#'
#' @details
#' pilsecond are (p,d,q) functions that compute the prior-informed Lambda-second (L") distribution. This 
#' distribution is an generalization of the lambda-prime distribution
#' \insertCite{l99}{CohensdpLibrary}. It can take up to two seconds to compute.
#'
#' Note: the parameters are the raw sample size n, the observed Cohen's dp, and the 
#' sample correlation r. All the scaling required are performed within the functions (and so 
#' you do not provide degrees of freedom). This is henceforth not a generic lambda-second 
#' distribution, but a lambda-second custom-tailored  for the problem of standardized mean 
#' difference.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' ### Note: this distribution can be slow to compute
#' ### dpilsecond(0.25, 9, 0.26, 0.333) # 1.186735
#' ### ppilsecond(0.25, 9, 0.26, 0.333) # 0.5150561
#' ### qpilsecond(0.01, 9, 0.26, 0.333) # -0.7294266
#' 

#' @export 
dpilsecond <- function( delta, n, d, r ) {
    if (n < 2) stop("Sample size smaller than 2 forbidden. Exiting pilsecond...")
    if (r < -1 | r > +1 ) stop("Correlation not between -1 and +1. Exiting pilsecond...")
    res <- .Fortran("subfbdeltafromobsdpobsrpdf",
        as.double(delta), 
        as.double(n), 
        as.double(d), 
        as.double(r), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        err = as.integer(-99),
        ans = as.double(0.00)  )

    if (res$err != 0) {
        warning( messageWier("pilsecond", res$err ) )
    }
    return( res$ans )
}

#' @export 
ppilsecond <- function( delta, n, d, r ) {
    if (n < 2) stop("Sample size  smaller than 2 forbidden. Exiting pilsecond...")
    if (r < -1 | r > +1 ) stop("Correlation not between -1 and +1. Exiting pilsecond...")
    res <- .Fortran("subfbdeltafromobsdpobsrcdf",
        as.double(delta), 
        as.double(n), 
        as.double(d), 
        as.double(r), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        err = as.integer(-99),
        ans = as.double(0.00)  )

    if (res$err != 0) {
        warning( messageWier("pilsecond", res$err ) )
    }
    return( res$ans )
}

#' @export 
qpilsecond <- function( p,    n, d, r ) {
    if (n < 2) stop("Sample size  smaller than 2 forbidden. Exiting pilsecond...")
    if ( p < 0 | p > 1 ) stop("Probability must be between 0 and 1. Exiting pilsecond...")
    if (r < -1 | r > +1 ) stop("Correlation not between -1 and +1. Exiting pilsecond...")
    res <- .Fortran("subfbdeltafromobsdpobsridf",
        as.double(p), 
        as.double(n), 
        as.double(d), 
        as.double(r), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        err = as.integer(-99),
        ans = as.double(0.00)  )

    if (res$err != 0) {
        warning( messageWier("pilsecond", res$err ) )
    }
    return( res$ans  )
}
