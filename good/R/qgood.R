#' Quantile function for the Good distribution
#'
#' @description Quantile function for the Good distribution with parameters z and s.
#'
#' @usage qgood ( p , z , s , lower.tail = TRUE )
#'
#' @param p vector of non-negative integer quantiles.
#' @param z vector of first parameter for the Good distribution.
#' @param s vector of second parameter for the Good distribution.
#' @param lower.tail logical; if TRUE (default), probabilities are
#' \eqn{P(X \le x)}. Otherwise, \eqn{P(X > x)}.
#'
#' @return The smallest integer x such that \eqn{P(X \le x) \ge p}
#' (or such that \eqn{P(X \le x) \ge 1-p} if lower.tail is FALSE),
#' where X is a random variable following a Good distribution with parameters z
#' and s. Parameter z should be within the interval \eqn{(0,1)}, and parameter s
#' in the reals. Vector p should have values between \eqn{0} and \eqn{1}. If vector p has
#' negative values and/or outside the interval \eqn{(0,1)}, \code{qgood} returns NaN
#' with a warning. If vector p contains 1, \code{qgood} returns Inf. \code{qgood}
#' calls \code{dgood} from package \pkg{good}.
#'
#' @seealso
#' See also \code{\link[copula]{polylog}} from \pkg{copula}, \code{\link[good]{dgood}},
#' and \code{\link[good]{pgood}} and \code{\link[good]{rgood}} from \pkg{good}.
#'
#' @author
#' Jordi Tur, David Moriña, Pere Puig, Alejandra Cabaña, Argimiro Arratia,
#' Amanda Fernández-Fontelo
#'
#' @references
#' Good, J. (1953). The  population  frequencies  of  species  and  the  estimation  of  population
#' parameters. Biometrika, 40: 237–264.
#'
#' Zörnig, P. and Altmann, G. (1995). Unified representation of zipf distributions.
#' Computational Statistics & Data Analysis, 19: 461–473.
#'
#' Kulasekera, K.B. and Tonkyn, D. (1992). A new distribution with applications to survival
#' dispersal anddispersion. Communication in Statistics - Simulation and Computation,
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
#' @examples
#' # if p is not within [0, 1], NaN is returned with a warning
#' qgood ( p = c ( -0.6 , 1.3 ) , z = 0.5 , s = -3 )
#'
#' # if z is not within 0 and 1, NaN is returned with a warning
#' qgood ( p = 0.5 , z = c(-0.6, -9, 0.5) , s = -3 )
#'
#' qgood ( p = 0.5 , z = 0.6 , s = -3 )
#' qgood ( p = c ( 0.025 , 0.5 , 0.975 ) , z = 0.6 , s = -3 )
#' qgood ( p = c ( 0.025 , 0.5 , 0.975 ) , z = c ( 0.6 , 0.3 , 0.1 ) , s = -5 )
#' qgood ( p = c ( 0.025 , 0.5 , 0.975 ) , z = c ( 0.6 , 0.3 , 0.5 ) , s = -3 , lower.tail = FALSE )
#' qgood ( p = c ( 0.025 , 0.5 , 0.975 ) , z = c ( 0.6 , 0.3 ) , s = -3 )
#'
#' @export

qgood <- function ( p , z , s , lower.tail = TRUE ) {
  if ( sum ( z <= 0 | !z < 1 ) != 0 ) {
    warning ( "z should be within 0 and 1" ) }
  if ( sum ( p <= 0 | !p < 1 ) != 0 ) {
    warning ( "p should be within 0 and 1" ) }
  q1 <- vector ( )
  j <- suppressWarnings( cbind ( p , z , s ) )
  i <- 1
  if ( lower.tail == FALSE ) {
    j [ , 1 ] <- 1 - j [ , 1 ] }
  while ( i <= nrow ( j ) ) {
    f <- 0
    q1 [ i ] <- -1
    if ( ( j [ i , 2 ] >= 1 | j [ i , 2 ] <= 0 ) ) {
      q1 [ i ] <- NaN
    } else if ( ( j [ i , 1 ] > 1 | j [ i , 1 ] < 0 ) ) {
      q1 [ i ] <- NaN
    } else if ( j [ i , 1 ] == 1 ) {
      q1 [ i ] <- Inf
    } else {
      while ( f < j [ i , 1 ] ) {
        q1 [ i ] <- q1 [ i ] + 1
        f <- dgood ( q1 [ i ] , j [ i , 2 ] , j [ i , 3 ] ) + f } }
    i <- i + 1 }
  q1 <- ifelse ( is.nan ( q1 ) | q1 >= 0 , q1 , 0 )
  return ( q1 ) }
