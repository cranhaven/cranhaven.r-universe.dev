

#' @include allGenericFunctions.R
NULL

### object definition: spectralData 


validitySpectraInTime         <-  function( object ) {
  errors                      <-  character()
  ## check dimensions 
  nWavelengths                <-  length( object@spectralAxis )
  nTimePoints                 <-  length( object@timePoints )
  dimSpectra                  <-  dim( object@spectra )
  checkSpectralDim          <-  dimSpectra[ 2 ] ==  nWavelengths
  checkTimeDim                <-  dimSpectra[ 1 ] ==  nTimePoints
  
  if( ! checkSpectralDim  ) {
    errors                    <-  addMessage( errors , "spectra do not match spectral dimensions"  )
  }
  if( ! checkTimeDim ) {
    errors                    <-  addMessage( errors ,  "spectra do not match with time dimensions" ) 
  }
  checkEqualTimeIntervals     <-  all.equal.numeric( diff( object@timePoints ) , diff( object@timePointsAlt ) )
  if( ! checkEqualTimeIntervals ) { 
    errors                    <-  addMessage( errors ,  "Increments of 'timePoints' , does not match 'timePointsAlt' " )
  }
  
  ## return output
  if( length( errors ) == 0 ){
    return( TRUE ) 
  } else {
    cat(errors) 
    return( FALSE )
  }
}





#' SpectraInTime-class 
#' 
#' 
#' Time resolved spectra for one experiment
#' 
#' @slot spectra matrix of spectral measurement with as rows timePoints and columns spectral axis 
#' @slot experimentName character vector with name of the experiment
#' @slot spectralAxis numeric vector of spectral values (e.g. wavelengths or mass to charge)
#' @slot timePoints of measurement in seconds
#' @slot timePointsAlt numeric vector of shifted time points in order to time align multiple spectral measurements in plots, by default equal to timePoints
#' @slot extraInfo list additional information such as probe type  
#' @slot startTime start date and time POSIXct format 
#' @slot units list with information on measurement units
#' @slot preprocessing list of preprocessing steps in taken in order
#' @name SpectraInTime-class
#' @author Adriaan Blommaert
#' @aliases SpectraInTime spectraInTime spectraintime
#' @export
SpectraInTime                  <-  setClass ( "SpectraInTime",
  slots = list(
    spectra             =  "matrix" ,
    experimentName      =  "character" ,
    spectralAxis         =  "numeric" ,  
    timePoints          =  "numeric" , 
    timePointsAlt       =  "numeric" , 
    extraInfo           =  "list",
    startTime           =  "POSIXct",       
    units               =  "list"  ,
    preprocessing       =  "list"
  )
)


setValidity( "SpectraInTime" , validitySpectraInTime )



#' Artificial example \code{\link{SpectraInTime-class}}
#' 
#' exponential conversion from 2 concentrations with gaussion curves for spectra at different wavelength per compounds
#' @param showPlots logical indicator to show plots 
#' @export
#' @examples 
#' ex1  <-   getSpectraInTimeExample()
#' @importFrom grDevices terrain.colors
#' @importFrom graphics lines legend
#' @importFrom stats dnorm
#' @return \code{\link{SpectraInTime-class}}
#' @author Adriaan Blommaert
getSpectraInTimeExample          <-  function( showPlots = FALSE ){
  
  ## set up: concentration data 
  wavelengths                   <-  seq( 100 , 500 , 0.5 )
  nWavelengths                  <-  length( wavelengths )
  times                         <-  seq( 0 , 10 , 0.1 )
  nTimes                        <-  length( times )
  RATE                          <-  0.25
  timeFunction                  <-  exp( - RATE * times )
  conc1                         <-  0.5  * timeFunction
  conc2                         <-  0.25 * ( 1- timeFunction )
  if( showPlots ) {
#    x11()
    plot(  x = times , y = conc1 , lwd = 2.5 , type = "l" , lty = 1 ,  xlab = "Time" , ylab = "Concentration" )
    lines( x = times , y = conc2 , lwd = 2.5 , lty = 3 )
    legend( "topright" ,  lwd = 2.5 , lty = c( 1 , 3 ) ,
      legend = c( "compound 1" , "compound 2" )
    )
  }
  
  ## generate artificial
  
  specExFunc                    <-  function(  c1 , c2 , wavelength = wavelengths  ) {
    c1 * dnorm( wavelength ,   mean = 300 , sd = 20  ) +  c2 * dnorm(wavelength ,  mean = 400 , sd = 25 )
  } 
  
  spectralDataMatrix            <-  t( mapply( specExFunc , conc1 , conc2  ) )
  
  # plot spectra in time 
  if( showPlots ) {
#    x11()
    timeIndVec                  <-  seq( 1 , 100 , length.out =  5)
    nTimePlot                   <-  length( timeIndVec )
    colVec                      <-  terrain.colors( nTimePlot )
    timeVec                     <-  times[ timeIndVec ]

    plot(  x = wavelengths , xlim = range( wavelengths ) , ylim = c( range( spectralDataMatrix ) ) , type = "l" , lwd = 2.5 , xlab = "Wavelength" ,
      ylab = "Absorbance" )
    for( iLine in seq_along( timeIndVec ) ) {
      timeInd                   <-  timeIndVec[ iLine ] 
      lines( x = wavelengths , y = spectralDataMatrix[ timeInd  ,   ] , type = "l" , lwd = 2.5 , col = colVec[ iLine ] )
    }
    
    legend( "topleft" , col = colVec , legend = paste( "time:" , timeVec  ) , lwd = 2.5 )
  }
  
  ## construct spectral object 
  getSpectraInTimeExample            <-  SpectraInTime(  spectra =  spectralDataMatrix,
    experimentName        =  "ABLOMMAERT-01-00376" ,
    spectralAxis           =  wavelengths ,  
    timePoints            =  times*60*60 , 
    timePointsAlt         =  times*60*60 - 3*60*60 ,
    extraInfo             =  list() ,
    startTime             =  as.POSIXct( "03/06/2013 20:00:03" , format = "%d/%m/%Y %H:%M:%S" ) ,         
  
    units                 =  list(
      wavelength          =  "expression(tilde(nu)/cm^-1)" ,
      spc                 =  "expression('A')" ,
      z                   =   "expression(t/s)" ,
      z.end               =   "expression(t/s)"  )
    )
  return( getSpectraInTimeExample )
}

### methods for spectral object (besided getters and setters)

#' @rdname SpectraInTime-class
#' @export 
setMethod( "dim" , "SpectraInTime" , definition = function( x ) {
    dims                           <-  dim( getSpectra( x ) )
    c( time = dims[ 1 ] , spectralAxis = dims[ 2 ] )
  }
)



#' function to get default summary functions 
#' 
#' @return character vector of functions
#' @export
getDefaultSumFunc     <- function(){
  c( "firstSpectrum" , "lastSpectrum" , "mean" , "median" , "sd" )
}

#' extract summary specs from \code{\link{SpectraInTime-class}} by wavelengths over time 
#' 
#' @rdname SpectraInTime-class
#' @param object 
#' @param summaryFunctions a character vector of summary functions
#' @return data.frame with first column wavelengths followed 
#' 
#' @keywords internal 
extractSummarySpecs          <-  function( object ,   summaryFunctions = getDefaultSumFunc() ) {
    spectra                    <-  getSpectra( object )
  
  ## function application keep wavelengths
  applyOverWavelength         <-  function( aFunction , matrix = spectra ){
     apply( matrix , MARGIN = 2 ,  aFunction )
  }
  
  ## a list of summaries
  result                      <-  lapply( summaryFunctions , applyOverWavelength )
  resultFrame                 <-  data.frame( result )
  names( resultFrame )        <-  summaryFunctions
  resultWithWavelengths       <-  data.frame( wavelengths = getSpectralAxis( object ) , resultFrame )
  return( resultWithWavelengths )
}


#' S4-class for summary object SpectraInTime
#' @name SummaryByWavelengths-class
#' @keywords internal
SummaryByWavelengths  <-  setClass( "SummaryByWavelengths" ,
  slots = c( 
    experimentName         = "character" , 
    timeRange              = "numeric" ,
    timeRangeAlt           = "numeric" , 
    spectralRange          = "numeric" ,
    spectra                = "data.frame" , 
    units                  = "list" 
  )
)



#' plot summary of \code{\link{SpectraInTime-class}}
#' 
#' @param x  \code{\link{SummaryByWavelengths-class}}
#' @param y no used, for internal consistency
#' @param colors a vector of colors used in the plot, default to \code{NULL}, when default colors are used 
#' 
#' @importFrom RColorBrewer brewer.pal
#' @examples 
#' summary               <-  summary( getSpectraInTimeExample() )
#' plot( summary )
#' @rdname SummaryByWavelengths-class
#' @export
setMethod( "plot" , c( x = "SummaryByWavelengths" , y = "ANY" ) , function( x , y , colors = NULL  ) {
    ## settings 
    MULTTOLERANCE        <-  1  # multiplicative tolerence change appearance of the plot 
    LINEWIDTH            <-  3
    
    ## preset plot elements 
    xRange               <-  x@spectralRange
    spectralSummaryTable <-  getSpectra( x )
    flagWavelengths      <-  colnames( spectralSummaryTable ) == "wavelengths"
    spectralAxis          <-  spectralSummaryTable[ , flagWavelengths ]
    summaryTable         <-  spectralSummaryTable[ , !flagWavelengths ]
    summaryStats         <-  colnames( summaryTable )
    nSummaryStats        <-  length( summaryStats )
    yRange               <-  range( summaryTable )
    yLimits              <-  c( min( 0 , yRange ) , max( yRange )*MULTTOLERANCE )
    units                <-  x@units 
    xLabel               <-  "Wavelength"
    yLabel               <-  "Response"
        
    ## colors 
    if( is.null( colors ) ) {
      baseCol            <-  c( "green" , "orange" , "black" , "brown" )
      extraCol           <-  brewer.pal( 12 , "Paired" )
      colvec             <-  c( baseCol , extraCol )     
    } else {
      colvec             <-  colors
    }
    nColors              <-  length( colvec )
    
    ## checks 
    checkNcolors            <-  nColors >= nSummaryStats
    if( ! checkNcolors ) {
      stop( "number of collors smaller then the number of summary statistics, define a new 'colors'" )
    }
      
    ##  make plot
      #  basic plto
    plot( x = spectralAxis , y = NULL , ylim = yLimits , cex.axis = 1.15 , cex.lab = 1.15 ,  xlab = xLabel , ylab = yLabel ,
      main = "Summary statistics per spectral value" )
      #  insert lines  # remark first and last captures by min and max 
    lapply( seq_len( nSummaryStats )  , function( iStat ) {
        lines( x = spectralAxis , y = summaryTable[ , iStat ] , col = colvec[ iStat ] , type = "l" , lwd = LINEWIDTH  )
      } 
    ) 
    # test first and last


      #  legend 
    legend( x = "topright" , legend = summaryStats , lwd = LINEWIDTH , col = colvec  , bty = "n")
  }
)






#' @rdname SpectraInTime-class
#' @examples
#'  spectralExample  <-  getSpectraInTimeExample() 
#'  summarySpectra   <-  summary( spectralExample )
#'  str( summarySpectra )
#'  summarySpectra
#' @param  object \code{SpectraInTime-class}
#' @param summaryFunctions character vector of summary functions
#' @importFrom methods new
#' @export
setMethod( "summary" , "SpectraInTime" , def =  function( object , summaryFunctions = getDefaultSumFunc()  ) {
    name                            <-  getExperimentName( object )
    timeRange                       <-  range( getTimePoints( object ) )
    timeRangeAlt                    <-  range( getTimePoints( object , timePointsAlt = TRUE )  )
    spectralAxisRange                 <-  range( getSpectralAxis( object) )
    specSummary                     <-  extractSummarySpecs( object , summaryFunctions )
    unitList                        <-  getUnits( object ) 
    ## create summary object  
    new( "SummaryByWavelengths" , 
      experimentName  = name ,
      timeRange       = timeRange ,
      timeRangeAlt    = timeRangeAlt ,
      spectralRange   = spectralAxisRange ,
      spectra         = specSummary ,
      units           = unitList
    )
  }
)

#' @rdname getSpectra
#' @export
setMethod( f = "getSpectra" , signature = "SpectraInTime" , 
  definition = function( object ) { 
    return( object@spectra ) 
  }
)


#' @rdname SummaryByWavelengths-class
#' @export
setMethod( f = "getSpectra" , signature = "SummaryByWavelengths" , 
    definition = function( object ) { 
      return( object@spectra ) 
    }
)

#' @rdname SpectraInTime-class
#' @importFrom utils head tail
setMethod( "show" , "SummaryByWavelengths" , function( object ) {
    cat("Experiment name =" , object@experimentName , "\n" ) 
    cat( "Time range = " , object@timeRange ,  "\n" ) 
    cat( "Spectral range =" , object@spectralRange , "\n" ) 
    print( "Summary specs:" )
#    str( object@specSummary )
    cat("units =" )
    print( object@units )
    print( head( object@spectra ) , justify = "right" )
    cat("..." , "\n"  )
    cat("..." , "\n" )
    cat("..." , "\n"  )
    print( tail( object@spectra ) , justify = "right" )
  }
)


###  Getter functions


#' @rdname getSpectra
#' 
#' @export
setMethod( f = "getSpectra" , signature = "SpectraInTime" , 
  definition = function( object ) { 
    return( object@spectra ) 
  }
)


#' @rdname getTimePoints
#' 
#' @param timePointsAlt logical indicator to get alternative (shifted) instead of recorded time points, defaults to \code{FALSE}
#' @param timeUnit unit to use , choose between: \code{seconds} , \code{minutes} or \code{hours}, defaults equal to \code{seconds} 
#' @examples
#'  spectra    <-  getSpectraInTimeExample()
#'  getTimePoints( spectra )
#'  getTimePoints( spectra , timePointsAlt = TRUE  )
#'  getTimePoints( spectra , timeUnit = "hours"  )
#' @export
setMethod( f = "getTimePoints" , signature = "SpectraInTime" , 
  definition =  function( object , timePointsAlt = FALSE  , timeUnit = "seconds"   ) {
	  timeConversionFactor            <-  getTimeConversionFactor( timeUnit )
	  if( timePointsAlt ) {
		  return( object@timePointsAlt * timeConversionFactor )
	  } else {
		  return( object@timePoints * timeConversionFactor ) 
	  }
  }
)


#' @rdname getExperimentName
#' @export
setMethod( f = "getExperimentName" , signature = "SpectraInTime" , 
  definition = function( object ) { 
    return( object@experimentName ) 
  }
)

#' @rdname getSpectralAxis
#' @export
setMethod( f = "getSpectralAxis" , signature = "SpectraInTime" , 
  definition = function( object ) { 
    return( object@spectralAxis ) 
  }
)

#' @rdname getExtraInfo 
#' @export
setMethod( f = "getExtraInfo" , signature = "SpectraInTime" , 
  definition = function( object ) { 
    return( object@extraInfo ) 
  }
)

#' @rdname getStartTime
#' @export
setMethod( f = "getStartTime" , signature = "SpectraInTime" , 
  definition = function( object  ) { 
    return( object@startTime ) 
  }
)

#' @rdname getUnits
#' @export
setMethod( f = "getUnits" , signature = "SpectraInTime" , 
  definition = function( object ) { 
    return( object@units ) 
  }
)

#' @rdname getPreprocessing
#' @export
setMethod( f = "getPreprocessing" , signature = "SpectraInTime" , 
  definition = function( object ) { 
    return( object@preprocessing ) 
  }
)


### Setter functions  (modifying data is not allowed directly only via methods)


#' @rdname setExperimentName
#' @importFrom methods validObject
#' @export
setReplaceMethod(f = "setExperimentName" , signature = "SpectraInTime" ,
  def = function( object , value ){
    object@experimentName   <-  value 
    validObject( object )
    return( object )
  }
)

#' @rdname setExperimentName
#' @export
setReplaceMethod(f = "setTimePointsAlt" , signature = "SpectraInTime" ,
  def = function( object , value ){
    object@timePointsAlt   <-  value 
    validObject( object )
    return( object )
  }
)


#' @rdname firstSpectrum
#' @param object S4 object
#' @export
setMethod( "firstSpectrum" , "SpectraInTime" , function( object ) { 
    spectra                 <-  getSpectra( object )
    spectraSelect           <-  spectra[ 1 , ]
    spectraSelect
  }
) 
#' @rdname firstSpectrum
setMethod("firstSpectrum" , "numeric" , function( object ) { first(object) } )

#' @rdname lastSpectrum
setMethod("lastSpectrum" , "numeric" , function( object) { last(object) } )


#' @rdname lastSpectrum
#' @param object S4 object
#' @export
setMethod( "lastSpectrum" , "SpectraInTime" , function( object ) {
    spectra                 <-  getSpectra( object )
    nSpectra                <-  dim( spectra )[1]
    spectraSelect           <-  spectra[ nSpectra ,  ]
    spectraSelect
  }
)


#' convert to data.frame (used in visualization)
#' 
#' @param timePointsAlt see \code{\link{getTimePoints}}
#' @param  timeUnit see \code{\link{getTimePoints}}
#' @return a \code{data.frame} containing spectral information in long format
#' @examples
#' 
#' ## convert to data.frame
#' spectra                 <-  getSpectraInTimeExample()
#' spectraFlat             <-  as.data.frame( spectra )
#' 
#' @importFrom BiocGenerics as.data.frame
#' @rdname SpectraInTime-class
#' @return data.frame
#' @export
setMethod( "as.data.frame" , signature = "SpectraInTime" , function( x , timePointsAlt = FALSE , timeUnit = "seconds" ) {
      ## extract components
      spectra                <-  getSpectra( x )
      timePoints             <-  getTimePoints( x , timePointsAlt = timePointsAlt , timeUnit = timeUnit )
      spectralAxis            <-  getSpectralAxis( x )
      nTimes                 <-  length( timePoints )
      nWavelengths           <-  length( spectralAxis )
      
      ## convert to long format 
      spectraLong            <-  as.vector( spectra )
      timePointsLong         <-  rep( timePoints , nWavelengths )
      spectralAxisLong             <-  rep( spectralAxis  ,  rep( nTimes , nWavelengths ) )

      ## output data.frame 
      output                 <-  data.frame( spectralAxis = spectralAxisLong ,
          timePoints = timePointsLong , 
          response   = spectraLong 
      )
      return( output )
    } 
)


