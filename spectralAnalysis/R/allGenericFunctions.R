#' @include internalHelpers.R allGenericFunctions.R
NULL



#' Get the first spectrum
#' 
#' @name firstSpectrum
#' @param ... additional parameters
#' @return  numeric vector containing observations first spectrum
#' @export
setGeneric( "firstSpectrum" , function( object , ... ) {
    standardGeneric( "firstSpectrum" ) 
  } 
)

#'  Get the last spectrum
#' 
#' @name lastSpectrum
#' @param ... additional parameters
#' @return numeric vector containing values last spectrum
#' @export
setGeneric( "lastSpectrum" , function( object  , ... ) {
    standardGeneric( "lastSpectrum" ) 
  } 
)




#' Check object compatibility
#' 
#' Check wheter 2 objects are compatible before using them together e.g. in time aligment using a time file with matching experiment name. 
#' 
#' @param x first object
#' @param y second object
#' @param ... additional parameters 
#' @return  no output, produces an error when object are not compatible with each other
#' @export
setGeneric( "checkCompatible" , function( x , y , ... ) {
    stop(" no compatibility check defined for these objects check defined for these object types")
  }
)

#' Time align first object, using info in the second object
#' 
#' @param x and S4 object to be aligned
#' @param y object to use time information from
#' @param ... additional arguments 
#' @export 
setGeneric( "timeAlign" , function( x , y , ... ) {
    stop(" no timeAlign function defined for these objects ")
  } )

#' Wavelength align spectral data
#' 
#' Align SpectraInTime objects with differing wavelength axes to a common wavelength axis 
#' using cubic spline interpolation.
#' @export 
setGeneric( "wavelengthAlign" , function( ref , toAlign ) {
			stop(" no wavelengthAlign function defined for these objects ")
		} )


### getters


#' generic function to extract \code{range}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getRange
#' @export
setGeneric( "getRange", function( object, ... ) {
    standardGeneric ( "getRange" )
  } 
)

#' generic function to extract \code{elements}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getElements
#' @export
setGeneric( "getElements", function( object, ... ) {
    standardGeneric ( "getElements" )
  } 
)


#' generic function to extract \code{spectra}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getSpectra
#' @return matrix of spectra
#' @export
setGeneric( "getSpectra", function( object, ... ) {
    standardGeneric ( "getSpectra" )
  } 
)

#' generic function to extract \code{timePoints}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getTimePoints
#' @return numberic vector containing timepoints
#' @export
setGeneric( "getTimePoints", function( object, ... ) {
    standardGeneric ( "getTimePoints" )
  } 
)

#' generic function to extract \code{experimentName}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getExperimentName
#' @return string containing experiment name
#' @export
setGeneric( "getExperimentName", function( object, ... ) {
    standardGeneric ( "getExperimentName" )
  } 
)

#' generic function to extract \code{spectralAxis}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getSpectralAxis
#' @return numeric vector containing wavelengths
#' @export
setGeneric( "getSpectralAxis", function( object, ... ) {
    standardGeneric ( "getSpectralAxis" )
  } 
)

#' generic function to extract \code{extraInfo}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name   getExtraInfo
#' @return list of extraInfo 
#' @export
setGeneric( "getExtraInfo", function( object, ... ) {
    standardGeneric ( "getExtraInfo" )
  } 
)

#' generic function to extract \code{startTime}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getStartTime
#' @return character vector with start time 
#' @export
setGeneric( "getStartTime", function( object, ... ) {
    standardGeneric ( "getStartTime" )
  } 
)

#' generic function to extract \code{units}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getUnits 
#' @return list of units
#' @export
setGeneric( "getUnits", function( object, ... ) {
    standardGeneric ( "getUnits" )
  } 
)


#' generic function to extract \code{preprocessing}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getPreprocessing 
#' @return list with preprocessing steps
#' @export
setGeneric( "getPreprocessing", function( object, ... ) {
    standardGeneric ( "getPreprocessing" )
  } 
)




#' generic function to extract \code{dimensionReduction}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getDimensionReduction
#' @return dimension reduction slot of an object
#' @export
setGeneric( "getDimensionReduction" , function( object, ... ){
      standardGeneric ( "getDimensionReduction" )
    } 
)



### setters
#' set the experiment name
#' 
#' @param object a S4 class object
#' @param value a vector of time points
#' @usage setExperimentName(object) <- value
#' @docType methods
#' @rdname setExperimentName
#' @export setExperimentName<-
#' @return \code{\link{SpectraInTime-class}} with modified experiment name
setGeneric( "setExperimentName<-", function( object, value ) {
    standardGeneric("setExperimentName<-")
  } 
)

#'set time alternative time axis
#' 
#' @param object a S4 class object
#' @param value a vector of time points
#' @usage setTimePointsAlt(object) <- value
#' @docType methods
#' @rdname setTimePointsAlt
#' @export setTimePointsAlt<-
#' @return \code{\link{SpectraInTime-class}} with modified timePointsAlt axis
setGeneric( "setTimePointsAlt<-", function( object, value ) {
    standardGeneric("setTimePointsAlt<-")
  } 
)




### preprocessing related generic functions 


#' generic normalization function 
#' 
#' @param object a S4 class object
#' @param ... additional parameters
#' @docType methods
#' @name  normalize 
#' @export
#' @return \code{\link{SpectraInTime-class}}
setGeneric( "normalize", function( object,  ... ) {
      standardGeneric ( "normalize" )
    } 
)


#' generic smoothing function
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  smooth  
#' @export
#' @return \code{\link{SpectraInTime-class}}
setGeneric( "smooth", function( object,  ... ) {
      standardGeneric ( "smooth" )
    } 
)


#' generic function to perfom baseline correction
#' 
#' @param object a S4 class object
#' @docType methods
#' @name baselineCorrect 
#' @export
#' @return \code{\link{SpectraInTime-class}}
setGeneric( "baselineCorrect", function( object,  ... ) {
      standardGeneric ( "baselineCorrect" )
    } 
)


#' perform multiplicative scatter correction
#' 
#' @param object a S4 class object such as \code{\link{SpectraInTime-class}}
#' @param ... for internal method consistency
#' @param referenceSpectra the reference spectra when not derived from the current data, defaults to \code{NULL} when \code{referenceMethod} is used 
#' to derive the reference spectra from the data
#' @param referenceMethod name of the function used to derive the spectra over the data measurements, defaults to \code{"mean"} when the average spectra in the 
#' \code{object} is used
#' @return new S4 object with spectra scatter corrected
#' @details multiplicative scatter correction consist of fitting regressing each spectra against the reference spectra and using the coefficients 
#'  of this regression model to correct for multiplicative light scatter by substracting the intercept and dividing by the slope
#' @author Adriaan Blommaert
#' 
#' @docType methods
#' @name scatterCorrect 
#' @export
#' @return \code{\link{SpectraInTime-class}}
setGeneric( "scatterCorrect" , function( object , ... ) {
			standardGeneric( "scatterCorrect" )
		}
)



#' generic function to preprocess an S4 object
#' 
#' @param object a S4 class object
#' @param with an other object containing preprocessing information: other S4 object, list or expression 
#' @docType methods
#' @name preprocess 
#' @export
#' @return \code{\link{SpectraInTime-class}}
setGeneric( "preprocess" , function( object , with ) {
      standardGeneric( "preprocess" )
    }
)










