#' Random generation for the Good distribution
#'
#' @description Random generation for the Good distribution with parameters z and s.
#'
#' @usage rgood ( n , z , s , th = 10^-6 )
#'
#' @param n vector of number of observations to be generated
#' @param z vector of first parameter for the Good distribution
#' @param s vector of second parameter for the Good distribution
#' @param th defines the lower (\eqn{q_1}) and upper (\eqn{q_2}) quantiles such that \eqn{P(X \le q_1)=th} and
#' \eqn{P(X \le q_2)=1-th} respectively.
#'
#' @return A vector containing n random deviates from a Good distribution with parameters z
#' and s. Parameter z should be within the interval \eqn{(0,1)}, and parameter s in the reals.
#' \code{rgood} returns NaN if either arguments n or th are negative. \code{rgood} calls \code{qgood}
#' and \code{pgood} from package \pkg{good}.
#'
#' @seealso
#' See also \code{\link[copula]{polylog}} from \pkg{copula}, \code{\link[good]{dgood}},
#' and \code{\link[good]{pgood}} and \code{\link[good]{qgood}} from \pkg{good}.
#'
#' @author
#'  Jordi Tur, David Moriña, Pere Puig, Alejandra Cabaña, Argimiro Arratia,
#'  Amanda Fernández-Fontelo
#'
#' @references
#' Good, J. (1953). The  population  frequencies  of  species  and  the  estimation  of  population
#' parameters. Biometrika, 40: 237–264.
#'
#' Zörnig, P. and Altmann, G. (1995). Unified representation of zipf distributions.
#' Computational Statistics & Data Analysis, 19: 461–473.
#'
#' Kulasekera, K.B. and Tonkyn, D. (1992). A new distribution with applications to survival
#' dispersal and dispersion. Communication in Statistics - Simulation and Computation,
#' 21: 499–518.
#'
#' Doray, L.G. and Luong, A. (1997). Efficient estimators for the good family.
#' Communications in Statistics - Simulation and Computation, 26: 1075–1088.
#'
#' Johnson, N.L., Kemp, A.W. and Kotz, S. Univariate Discrete Distributions.
#' Wiley, Hoboken, 2005.
#'
#' Kemp. A.W. (2010). Families of power series distributions, with particular
#' reference to the lerch family. Journal of Statistical Planning and Inference,
#' 140:2255–2259.
#'
#' Wood, D.C. (1992). The Computation of Polylogarithms. Technical report. UKC,
#' University of Kent, Canterbury, UK (KAR id:21052).
#'
#' @importFrom stats runif
#' @importFrom plyr alply
#'
#' @examples
#' # if n is not a non-negative integer, function returns NaN with a warning
#' rgood ( n = -100 , z = 0.5 , s = -3 )
#'
#' # if th is not positive, th is replaced by 1e-06 and a warning is provided
#' rgood ( n = 1 , z = 0.5 , s = -3 , th = -9 )
#'
#' # if z is not within 0 and 1, NaN is returned with a warning
#' rgood ( n = 2 , z = c( -0.5, 0.5 ) , s = -3 )
#'
#' rgood ( n = 10 , z = 0.6 , s = -3 )
#' rgood ( n = 1000 , z = 0.6 , s = -3 )
#' rgood ( n = c ( 3 , 10 ) , z = 0.6  , s = -3 )
#' rgood ( n = c ( 3 , 10 ) , z = c ( 0.2 , 0.8 ) , s = - 3 )
#' rgood ( n = c ( 3 , 10 , 6 ) , z = c ( 0.2 , 0.8 ) , s = c ( - 3 , -2 ) )
#' rgood ( n = 1000 , z = 0.3 , s = - 170 )
#'
#' @export

rgood <- function ( n , z , s , th = 10^-6 ) {
  if ( sum ( z <= 0 | !z < 1 ) != 0 ) {
    warning ( "z should be within 0 and 1" ) }
  if ( th < 0 ) {
    th <- 10^-6
    warning ( "th should be positive; replaced by th = " , th ) }
  if ( sum ( n <= 0 | n %% 1 != 0 ) != 0 ) {
    warning ( "n should be a non-negative integer" )
    return ( NaN )
  } else {
    lim <- apply ( expand.grid ( lim = c ( th , ( 1 - th ) ) , z , s ) , 1 , function ( j ) suppressWarnings( qgood ( j [ 1 ] , j [ 2 ] , j [ 3 ] ) ) )
    lim.lower <- matrix ( lim , ncol = 2 , byrow = TRUE) [ , 1 ]
    lim.upper <- matrix ( lim , ncol = 2 , byrow = TRUE) [ , 2 ]
    cdf <- alply ( suppressWarnings ( cbind ( n , z , s , lim.lower , lim.upper ) ) , 1 , function ( j ) if( j [ 2 ] < 1 && j [ 2 ] > 0){ pgood ( j [ 4 ] : j [ 5 ] , j [ 2 ] , j [ 3 ] )
                                                                                                         } else NaN)
    rdefault <- function ( cdf , lim ) {
      u <- runif ( 1 )
      if( any( is.nan( lim ) ) ){
        return( NaN )
      } else if ( u <= cdf [ 1 ] ) {
        return ( 0 )
      } else {
        return ( min ( c ( lim [ 1 ] :  lim [ 2 ] ) [ u <= cdf ] ) ) } }
    res <- apply ( suppressWarnings ( cbind ( 1 : length ( cdf ) , n , z , s , lim.lower , lim.upper ) ) , 1 , function ( j ) replicate ( j [ 2 ] ,  rdefault ( cdf [[ j [ 1 ] ]] , j [ c ( 5 , 6 ) ] ) ) )
  }
  return ( as.vector ( unlist (res ) ) ) }


