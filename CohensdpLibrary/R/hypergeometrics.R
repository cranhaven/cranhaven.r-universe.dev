#' @name hypergeometrics
#'
#' @md
#'
#' @title hypergeometric functions.
#'
#' @aliases hypergeometric0F1 hypergeometric1F1 hypergeometric2F1
#'
#' @description
#' The hypergeometric functions are a series of functions which includes
#' the hypergeometric0F1, called the confluent hypergeometric limit function (D. Cousineau);
#' the hypergeometric1F1, called the confluent hypergeometric function \insertCite{m14}{CohensdpLibrary};
#' and the hypergeometric2F1, called Gauss' confluent hypergeometric function \insertCite{MICHEL2008535}{CohensdpLibrary}.
#' These functions are involved in the computation of the K' and Lambda' 
#' distributions, as well as the Chi-square" and the t" distributions
#' \insertCite{c22a}{CohensdpLibrary}.
#'
#' @usage
#' hypergeometric0F1(a, z)      
#' hypergeometric1F1(a, b, z)  
#' hypergeometric2F1(a, b, c, z) 
#'  
#' @param a    the first parameter;
#' @param b    the second parameter;
#' @param c    the third parameter;
#' @param z    the argument raised to the powers 0 ... infinity ;
#'
#' @return     The result of the hypergeometric function.
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#' 
#' hypergeometric0F1(12, 0.4)         #   1.033851
#' hypergeometric1F1(12, 14, 0.4)     #   1.409877
#' hypergeometric2F1(12, 14, 16, 0.4) # 205.5699
#' 

#' @export
hypergeometric0F1 <- function( a, z ) {
    res <- .Fortran("subhyg0f1",
        as.double(a), 
        as.double(z), 
        as.double(0.00)  )
    return( res[[3]] )
}

#' @export 
hypergeometric1F1 <- function( a, b, z ) {
    res <- .Fortran("subhyg1f1",
        as.double(a), 
        as.double(b), 
        as.double(z), 
        as.double(0.00)  )
    return( res[[4]] )
}

#' @export 
hypergeometric2F1 <- function( a, b, c, z ) {
    res <- .Fortran("subhyg2f1",
        as.double(a), 
        as.double(b), 
        as.double(c), 
        as.double(z), 
        as.double(0.00)  )
    return( res[[5]] )
}
