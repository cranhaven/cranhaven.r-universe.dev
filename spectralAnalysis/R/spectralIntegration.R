# Integrate spectral data over a wavelength range
# 
# Author: Robin Van OirBeek , Nicolas Sauwen , Adriaan Blommaert (order of making adaptations )
#
#
###############################################################################

#' @include internalHelpers.R objectSpectraInTime.R
NULL



#' Integrate spectraInTime object 
#'
#' The integrated value over a user-specified spectral range is calculated (trapezium rule) per time point, afterwards smoothing over time can be applied 
#' @param object \code{\link{SpectraInTime-class}}
#' @param spectralRange numeric vector of 2 elements i.e. integration limits
#' @param smoothingValue numeric value between 0 and 1, amount of \code{\link[stats]{lowess}}-smoothing,
#'  default to \code{0} i.e no smoothing. Note that smoothing is applied  after integration
#' @param timeUnit character value, choose between: \code{second , minutes and hours}, defaults to 
#'  \code{seconds}
#' @return \code{data.frame} with variables \code{time} and \code{integratedValue}
#' @examples 
#' spectra                   <-  getSpectraInTimeExample()
#' defaults                  <-  spectralIntegration( spectra , c(200 , 300) , timeUnit = "hours" )
#' unsmoothedTrend           <-  spectralIntegration( spectra , c(200 , 300) , timeUnit = "hours" )
#' smoothedTrend             <-  spectralIntegration( spectra , c(200 , 300) ,
#'   smoothingValue = 0.5 , timeUnit = "hours" ) 
#' @importFrom methods new
#' @importFrom stats lowess
#' @export
spectralIntegration                    <- function( object, spectralRange , smoothingValue = 0 , timeUnit = "seconds"  ) {
  ## input checking
  checkSmoothing                       <-  is.numeric( smoothingValue ) && ( smoothingValue  >= 0 )  &&  ( smoothingValue <= 1 ) 
  if( !checkSmoothing ) {
    stop( "'smoothingValue' should be a numeric value between 0 and 1")
  }
  
  ## subset on spectral range   
  spectraSelect                        <-  object[ , r( spectralRange ) ]
  ## extract elements
  times                                <-  getTimePoints( spectraSelect , timeUnit = timeUnit)
  spectralAxis                          <-  getSpectralAxis( spectraSelect )
  pureSpectra                          <-  abs(getSpectra( spectraSelect ))
  TIMEMARGIN                           <-  1
  ## integrate
  integratedValues                     <-  apply( pureSpectra , TIMEMARGIN ,  "trapz" ,   x = spectralAxis )
  ## smooth
  if(smoothingValue == 0 ) {
    integratedValuesSmooth             <-  integratedValues
  } else {
    integratedValuesSmooth             <-  lowess( times , integratedValues , f = smoothingValue )$y
  }
  
  data.frame( time = times , value = integratedValuesSmooth )
}



#' Integration via trapezium rule
#' 
#' Originally from caTools package, included to avoid dependency on old package
#' with correction inverse scale, should give still possitive values (taken absolute values )
#' 
#' @keywords internal
trapz                         <-  function( x , y ) { 

    
  idx = 2:length(x)
  return (as.double( abs(x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}

