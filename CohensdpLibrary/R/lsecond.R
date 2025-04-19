#' @name lsecond
#'
#' @md
#'
#' @title Lambda second distribution or the non-central, non-standard, Lambda distribution.
#'
#' @aliases dlsecond plsecond qlsecond
#'
#' @description
#' This distribution was introduced in \insertCite{c22b;textual}{CohensdpLibrary} as the
#' exact solution to the predictive distribution of the Cohen's dp in repeated-measure 
#' design. A more elegant notation was provided in \insertCite{l22;textual}{CohensdpLibrary}. 
#' It is the dual of the t" distribution, the sampling distribution of dp in repeated-measure 
#' design introduced in \insertCite{c22a;textual}{CohensdpLibrary}.
#'
#' @usage
#' plsecond(delta, n, d, rho) 
#' dlsecond(delta, n, d, rho) 
#' qlsecond(p,     n, d, rho) 
#'  
#' @param delta     the parameter of the population whose probability is to assess;
#' @param n         the sample size n
#' @param d         the observed d_p of the sample;
#' @param rho       the population correlation 
#' @param p         the probability from which a quantile is requested
#'
#' @return          The probability or quantile of a Lambda'' distribution.
#'
#' @details
#' lsecond are (p,d,q) functions that compute the Lambda-second (L") distribution. This 
#' distribution is an generalization of the lambda-prime distribution
#' \insertCite{l99}{CohensdpLibrary}.
#'
#' Note that the parameters are the raw sample size n, the observed Cohen's dp, and the 
#' population rho. All the scaling required are performed within the functions (and so 
#' you do not provide degrees of freedom).This is henceforth not a generic lambda-second 
#' distribution, but a lambda-second custom-tailored  for the problem of standardized mean 
#' difference.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' 
#' dlsecond(0.25, 9, 0.26, 0.333) # 1.03753
#' plsecond(0.25, 9, 0.26, 0.333) # 0.494299
#' qlsecond(0.01, 9, 0.26, 0.333) # -0.6468003
#' 

#' @export 
dlsecond <- function( delta, n, d, rho ) {
    if (n < 2) stop("Sample size smaller than 2 forbidden. Exiting lsecond...")
    if (rho < -1 | rho > +1 ) stop("Correlation not between -1 and +1. Exiting lsecond...")
    res <- .Fortran("sublsecondpdf",
        as.double(delta), 
        as.double(n), 
        as.double(d), 
        as.double(rho), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        err = as.integer(-99),
        ans = as.double(0.00)  )

    if (res$err != 0) {
        warning( messageWier("lsecond", res$err ) )
    }
    return( res$ans )
}

#' @export 
plsecond <- function( delta, n, d, rho ) {
    if (n < 2) stop("Sample size  smaller than 2 forbidden. Exiting lsecond...")
    if (rho < -1 | rho > +1 ) stop("Correlation not between -1 and +1. Exiting lsecond...")
    res <- .Fortran("sublsecondcdf",
        as.double(delta), 
        as.double(n), 
        as.double(d), 
        as.double(rho), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        err = as.integer(-99),
        ans = as.double(0.00)  )

    if (res$err != 0) {
        warning( messageWier("lsecond", res$err ) )
    }
    return( res$ans )
}

#' @export 
qlsecond <- function( p,    n, d, rho ) {
    if (n < 2) stop("Sample size  smaller than 2 forbidden. Exiting lsecond...")
    if ( p < 0 | p > 1 ) stop("Probability must be between 0 and 1. Exiting lsecond...")
    if (rho < -1 | rho > +1 ) stop("Correlation not between -1 and +1. Exiting lsecond...")
    res <- .Fortran("sublsecondidf",
        as.double(p), 
        as.double(n), 
        as.double(d), 
        as.double(rho), 
        as.double(getOption("CohensdpLibrary.TOLERAN")), 
        as.integer(getOption("CohensdpLibrary.MAXITER")),
        err = as.integer(-99),
        ans = as.double(0.00)  )

    if (res$err != 0) {
        warning( messageWier("lsecond", res$err ) )
    }
    return( res$ans  )
}
