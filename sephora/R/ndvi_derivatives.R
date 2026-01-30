#' Calculates derivatives of idealized NDVI
#' 
#' Provides function expression of derivatives of an idealized NDVI curve
#' fitted through a harmonic regression model
#' 
#' @param    amp numeric vector specifying amplitude parameter
#' @param    pha numeric vector specifying phase angle parameter
#' @param degree integer. What derivative's degree should be calculated? 
#'               \code{degree=0} corresponds to harmonic regression fit
#' @param      L integer giving the number of observations per period
#' 
#' @export
#' 
#' @details This function returns the derivatives of \eqn{f(t)}, with respect
#' to \eqn{t}, when \eqn{f} has the representation:
#' 
#' \eqn{f(t) = \sum_{k=1}^{p} a[i] cos( (2  \pi k t)/L - \phi[i] )},
#' 
#' where \eqn{a} and \eqn{\phi} are substituted by the vectors \code{amp}
#' and \code{phase}, respectively. The degree of the derivative is given by the
#' argument \code{degree}.
#' 
#' @note For historic reasons, we ended up using the name \code{ndvi_derivatives}
#' for this function, but it can be used to calculate derivatives of any function
#' expression defined through \code{amp}, \code{pha}, \code{degree} and \code{L}.
#' 
#' @return A function expression
#' 
#' @seealso \code{\link{phenopar}}, \code{\link{phenopar_polygon}}, \code{\link[geoTS]{haRmonics}}
#' 
ndvi_derivatives <- function(amp, pha, degree, L){
  
  vp <- 0:(length(amp)-1)
  
  if( degree == 0 ){
    core_derivative <- function(t) cos( vp * (2*pi/L) * t - pha/(180/pi) )
  } else {
    if( degree %% 2 == 0 ){
      core_derivative <- function(t) (-1)^(degree/2) * (2*pi/L)^(degree/2) * vp^degree *
        cos( vp * (2*pi/L) * t - pha/(180/pi) )
    } else {
      core_derivative <- function(t) (-1)^( (degree-1)/2 + 1) * (2*pi/L)^degree *
        vp^degree * sin( vp * (2*pi/L) * t - pha/(180/pi) )
    }
  }
  
  function(t) sapply( t, function(s)  sum( amp * core_derivative( s ) ) )
}
