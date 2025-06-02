#' normalization function
#' @keywords internal
spectralNormalization             <-  function( object , method = "normalize" ,  spectralRange = r(-Inf, Inf) ,
    spectralAxisVal = NULL , scaleFunction = 'sd' , meanFunction = NULL   ){
  ## general objects
#  print(as.list(match.call())) # to check arguments 
#  print(sys.calls())
#  arguments                       <-  as.list( sys.call( 2 ) )
#  arguments                       <-  match.call()
  arguments                       <-  list( "normalize" ,  method = method , spectralRange = spectralRange , spectralAxisVal = spectralAxisVal , 
      scaleFunction = scaleFunction , meanFunction = meanFunction )
#  arguments                       <-  as.list( sys.call(1) )
  TIMEMARGIN                      <-  1  
  oldSpectra                      <-  getSpectra( object )
  
  ## check inputs 
  allowedMethods                  <-  c( "integration" , "peak" , "normalize"  )# tODO method checking 
  checkMethod                     <-  method %in% allowedMethods
  if( ! checkMethod  ) {
    stop( paste0( "'method' not recognized, choose method from :" , "\n" , "\t" , paste( allowedMethods , collapse = "; ") ) )
  }
  
  ## per method to normalization/scaling step 
  if( method == "integration" ){
    integratedValues              <-  spectralIntegration( object  , spectralRange = spectralRange   )
    referenceValue                <-  integratedValues[ , "value" ] 
    newSpectra                    <-  sweep( oldSpectra , TIMEMARGIN , STATS =  referenceValue , FUN = "/")
  }
  if( method == "peak" ) {
    if( is.null( spectralAxisVal ) ) { stop("specify  'spectralAxisVal' to use peak normalization" ) }
    referenceValue                <-  getSpectra( object[ , e( spectralAxisVal ) ] )
    newSpectra                    <-  sweep( oldSpectra , TIMEMARGIN, STATS =  referenceValue , FUN = "/"  )
  }
    
  if( method == "normalize" ){
    if( is.null( meanFunction ) ) {
		meanFunction             <-  "mean"
    } 
	
	meanSpec                    <-  apply( oldSpectra , TIMEMARGIN , meanFunction )
	meanCorrSpectra             <-  sweep( oldSpectra , TIMEMARGIN , STATS = meanSpec )
	
     
    scalePerTime                  <-  apply( meanCorrSpectra  , TIMEMARGIN , scaleFunction )
    newSpectra                    <-  sweep( meanCorrSpectra  , TIMEMARGIN , STATS = scalePerTime , FUN = "/" )
  }
  
  newObject                       <-  addPreprocessingStep( object , newSpectra,  arguments )
  return( newObject )
}


#' @rdname normalize
#' @param method a method for normalization or peak correction , choose from:
#' 
#'     * normalize substract \code{mean} and divide by \code{scale}
#'     * peak scale by reference \code{spectralAxisVal}
#'     * integration scale by integrating over \code{spectralAxisRange}
#' 
#' @param  spectralRange range for integration if method = \code{integration} , defaults to complete range 
#' @param  spectralAxisVal reference spectral axis value (wavelength or other) for \code{peak} regresssion 
#' @param  scaleFunction scale function used when method = \code{normalize} defaults to \code{\link[stats]{sd}}  
#' @param  meanFunction mean function used when method = \code{normalize} defaults to \code{\link[base]{mean}} 
#' @examples 
#'  spectralEx            <-  getSpectraInTimeExample()
#'  timeRange             <-  range( getTimePoints( spectralEx ))
#'  timesToSelect         <-  e(  seq( timeRange[1] , timeRange[2] , length.out = 5  )   )
#'  \donttest{
#'  plot( spectralEx )
#'  plot( spectralEx[ timesToSelect ,  ] , type = "time" )
#' }
#'  normalizePeak         <-  normalize( spectralEx , method = "peak" , spectralAxisVal = 400 )
#'  getPreprocessing( normalizePeak )
#' \donttest{
#'  plot( normalizePeak[ timesToSelect ,  ] , type = "time" )
#'  plot( normalizePeak )
#' }
#'  normalizeIntegration  <-  normalize( spectralEx , method = "integration" )
#' \donttest{
#'  plot( normalizeIntegration[ timesToSelect ,  ] , type = "time" )
#' }
#'  normalizedUser  <-  normalize( spectralEx , method = "normalize" , mean = "median" , scale = "sd" )
#' \donttest{
#'  plot( normalizedUser[ timesToSelect ,  ] , type = "time" ) 
#' }
#' @importFrom baseline baseline getCorrected
#' @export 
setMethod( "normalize" , "SpectraInTime"  , definition = spectralNormalization )

#' @rdname normalize
setMethod( "normalize" , "SpectraInTimeComp"  , definition = function( object , ... ){
      newObject          <-  as( object , Class = "SpectraInTime" )
      callNextMethod( object = newObject , ... )
    } 
)

spectralBaselineCorrect            <-  function( object , method = 'modpolyfit' , degree = 4 , ...   ) {
  additionalArguments              <-  list( ... )
#  arguments                        <-  as.list( sys.call( 2 ) )
#  arguments                       <-  as.list( sys.call(1) )
  arguments                        <-  c( list( "baselineCorrect" , method = method , degree = degree  ) , additionalArguments )
  oldSpectra                       <-  getSpectra( object )
  
  ## pass baseline correction method
  baseArguments                    <-  list( spectra = oldSpectra , method = method , degree = degree )
  if( method != 'modpolyfit' ){
    baseArguments$degree           <-  NULL 
  }
  argumentsForBaselineCorrection   <-  c( baseArguments , additionalArguments )
    
#  newSpectra                       <-  getCorrected( do.call( baseline , args = argumentsForBaselineCorrection ) )
  newSpectraBaselineFormat          <-  do.call( "baseline" , args = argumentsForBaselineCorrection )
  newSpectra                        <-  getCorrected( newSpectraBaselineFormat )  
  
  ## output new object with appended methods part
  newObject                         <-  addPreprocessingStep( object , newSpectra, arguments )
  return( newObject )
}





#'@rdname baselineCorrect 
#' 
#' @note baseline correction in the wavelength domain by linking to the \code{\link[baseline]{baseline}}
#' @param method method of baseline correction, default value is to \code{'modpolyfit'},  see \code{\link[baseline]{baseline.modpolyfit}}
#' @param degree numeric value, degree of the polynomial used only if \code{method} is \code{'modpolyfit'}
#' @param ... other parameters passed to \code{\link[baseline]{baseline}}
#' @examples 
#'  spectralEx           <-  getSpectraInTimeExample()
#'  timeRange            <-  range( getTimePoints( spectralEx ) )
#'  timesToSelect        <-  e(  seq( timeRange[1] , timeRange[2] , length.out = 5  )   )
#'  baselineDefault      <-  baselineCorrect( spectralEx )
#'  baselineHighPolynomial  <-  baselineCorrect( spectralEx, 
#'    method = 'modpolyfit', degree = 4 )
#' 
#'  # filtering with fast fourier transform, not so good on example 
#'  baselineLowpass         <-  baselineCorrect( spectralEx , method = "lowpass" )
#' 
#'  # visual inspection
#'  \donttest{
#'  plot( spectralEx )   
#'  plot( baselineDefault[ timesToSelect , ] , type = "time"  )
#'  plot( baselineHighPolynomial[ timesToSelect , ] , type = "time"  )
#'  plot( baselineLowpass[ timesToSelect , ] , type = "time"  ) 
#' }
#' 
#' @export
setMethod( "baselineCorrect" , "SpectraInTime" , spectralBaselineCorrect )

#'@rdname baselineCorrect 
setMethod( "baselineCorrect" , "SpectraInTimeComp"  , definition = function( object , ... ){
      newObject          <-  as( object , Class = "SpectraInTime" )
      callNextMethod( object = newObject , ... )
    } 
)

#' internal smoothing and differentiation function
#' @importFrom zoo rollapply
#' @keywords internal
spectralSmoothAndDifferentiate     <-  function( object, method = "sg" , order = 3 ,  window = order + 7 - order%%2 , derivative = 0 , dim = "spectralAxis"){
#  arguments                        <-  as.list( sys.call(2) )
#  arguments                       <-  as.list( sys.call(1) )
  arguments                        <-  list( "smooth" , method = method , order = order , 
      window = window , derivative = derivative )
  MARGIN                           <-  switch(dim, spectralAxis = 1, time = 2)  
  oldSpectra                       <-  getSpectra( object )

  
  ##  check inputs 
    #  derivative and order
  checkOrderDerivative             <- order > derivative
  if( ! checkOrderDerivative ) {
    stop( "'order' should be larger than 'derivative'")
  }
    # method
  allowedMethods                   <-  c( "sg" , "mean" )
  checkMethod( method = method , allowedMethods = allowedMethods )
  
  ## SG filtering  
  # tODO:test
  if( method == 'sg') {
	  filterFunction                 <- function( row ) {
		  result                       <-  signal::sgolayfilt( x = row , p = order  , n = window , m = derivative )
		  return( result )
	  } 
  } else if( method == 'mean') {
	  filterFunction                 <- function( row ) {
		  result                       <-  zoo::rollapply(data = row, width = window, mean, partial = TRUE)
		  return( result )
	  } 
  }
  
  ## adapt object
  newSpectra                       <-  t( apply( oldSpectra , MARGIN , filterFunction )  ) 
  if(MARGIN == 2) newSpectra       <- t(newSpectra)
  
  newObject                        <-  addPreprocessingStep( object = object , newSpectra = newSpectra , arguments )
  return( newObject ) 
}



#' Apply Savitzky-Golay smoothing filter and optionally return smoothed spectra or dif
#' 
#' smoothing is applied along the spectral axis, not the time axis 
#' @param method character vector smoothing method, options are 'sg' (= default, Savitsky-Golay filter) or 'mean'.
#' @param order numeric value,  order of the polynomial used to interpolate (only used when \code{method = 'sg'}), should be larger than derivative order,
#'  defaults to 3 + derivative 
#' @param  window width of the smoothing  default value slightly higher than in the signal package, the user might consider a large value, otherwise smoothing has little effect
#' @param derivative derivative to be taken (only used when \code{method = 'sg'}), defaults to \code{0}
#' @param dim character string, specifying along which dimension smoothing should be applied. Options are "spectralAxis" (= default) or "time"
#' @rdname smooth
#' @examples 
#' \donttest{
#'     spectralEx     <-  getSpectraInTimeExample()
#'     smoothDefault   <-  smooth( spectralEx )
#'     timeRange       <-  range( getTimePoints( spectralEx ))
#'     timesToSelect   <-  e( seq( timeRange[1] , timeRange[2] , length.out = 5  )  )
#'     smoothALot      <-  smooth( spectralEx ,  order = 2 , window = 301  ) 
#'     derivative1     <-  smooth( spectralEx , derivative = 1 )   
#'     derivative2     <-  smooth( spectralEx , derivative = 2 )
#'     }
#' @note equal distances between wavelenght intervals are assumed
#' @importFrom signal sgolayfilt
setMethod( "smooth" , "SpectraInTime" ,  spectralSmoothAndDifferentiate )


#'@rdname  smooth
setMethod( "smooth" , "SpectraInTimeComp"  , definition = function( object , ... ){
      newObject          <-  as( object , Class = "SpectraInTime" )
      callNextMethod( object = newObject , ... )
    } 
)



#' function to use by preprocessing step
#' 
#' change the spectra and add preprocessing info, check whehther the object is valid 
#' @param object \code{\link{SpectraInTime-class}}
#' @param newSpectra numeric matrix new spectral data
#' @param preprocessingInfo list of preprocessing info
#' @return \code{\link{SpectraInTime-class}}
#' @keywords internal 
addPreprocessingStep               <-  function( object , newSpectra , preprocessingInfo ){
  newObject                        <-  object
  newObject@spectra                <-  newSpectra
  nPreprocessing                   <-  length( object@preprocessing ) + 1 # add current preprocessing step
  newObject@preprocessing[[ nPreprocessing ]] <- preprocessingInfo
  names( newObject@preprocessing ) <-  paste0( "step" , seq_len( nPreprocessing ) )
  validObject( newObject )
  return( newObject ) 
}


#' check method in list of allowd method
#' 
#' @param method character of method to check
#' @param allowedMethods character vector of allowed method
#' @return nothing just stop if method not allowed
#' @keywords internal 
checkMethod                      <-  function( method , allowedMethods ) {
  checkLength                    <-  length( method ) == 1 
  checkCharacter                 <-  is.character( method )
  if( ! ( checkLength && checkCharacter ) ) {
    stop( "specify only 1 method as a character" )
  }
  checkMethod                    <-  method %in% allowedMethods
  if( ! checkMethod  ) {
    stop( paste0( "'method' not recognized, choose method from :" , "\n" , "\t" , paste( allowedMethods , collapse = "; ") ) )
  } 
}


### preprocess (repeat preprocessing step)

#' internal function to wrap multiple preprocessing steps
#' @keywords internal
preprocessSpectraInTimeWithList          <-  function( object , with ){
  
  # TODO: input checking 
  preprocessingSteps                     <-  with
  nSteps                                 <-  length( preprocessingSteps )  
  objectPreprocessing                    <-  object # initialization of object 
  # iStep = 1 
  for( iStep in seq_len( nSteps) ){
    preprocessStep                       <-  preprocessingSteps[[ iStep ]]
    functionName                         <-  as.character( preprocessStep[[1]] )
    functionArguments                    <-  as.list( preprocessStep[ -1 ] ) # remove function
#    functionArguments[[1]]               <-  objectPreprocessing # replace object
    objectPreprocessing                  <-  do.call( functionName , c( list( object = objectPreprocessing ) , functionArguments ) )
  }
  newObject                              <-  objectPreprocessing
  newObject@preprocessing                <-  preprocessingSteps
  validObject( newObject )
  return( newObject )
}

## specific preprocessing methods 
#' @rdname preprocess
#' @export
setMethod( "preprocess" ,  c("SpectraInTime" , "list" ) , preprocessSpectraInTimeWithList )

#' @rdname preprocess
#' @export
setMethod( "preprocess" ,  c( object = "SpectraInTime" , with = "SpectraInTime" ) , function( object , with ){
      preprocessingList       <-  getPreprocessing( with )
      preprocess( object , preprocessingList )
    }
)

#' @rdname preprocess
setMethod( "preprocess" , "SpectraInTimeComp"  , function( object , with ){
      newObject                 <-  as( object, Class = "SpectraInTime" )
      callNextMethod( object = newObject , with = with )
    }
)


#' Local baseline correction
#' 
#' Substract a baseline either through 1 or 2 points
#' 
#' @param object  \code{\link{SpectraInTime-class}}
#' @param baseWavelengths numeric vector of 1 or 2 wavelength use to draw a baseline trough,
#' defaults to \code{NULL} when no baseline correction is performed
#' @return  \code{\link{SpectraInTime-class}} with baseline subset
#' @examples 
#' spectra              <-  getSpectraInTimeExample()
#' spectraConstCorrect  <-  localBaselineCorrect( spectra , baseWavelengths = 240  )
#' spectraLinCorrect    <-  localBaselineCorrect( spectra , c( 250 , 330 )  ) 
#' \donttest{
#' plot( spectra )
#' plot( spectraConstCorrect )
#' plot( spectraLinCorrect ) 
#' }
#' @author Adriaan Blommaert
#' @export
localBaselineCorrect           <-  function( object , baseWavelengths = NULL  ) {
  
  ## no baselines specified, return without correction 
  if( is.null( baseWavelengths) ){
    return( object ) # object returned no correction 
  }
  
  ## general calculations 
  baselineInfo                 <-  getSpectra( object[ , e( baseWavelengths ) ] )
  originalSpectra              <-  getSpectra( object )
  spectralDims                 <-  dim( originalSpectra )
  
  
  ##  1 baseline wavelength ( substract constanct in wavelength domain)
  nBaseWavelengths             <-  length( baseWavelengths )
  if( nBaseWavelengths == 1 ){
    baseSpectrumMat               <-  matrix( baselineInfo , nrow = spectralDims[1] , ncol = spectralDims[2] )
    correctedSpectrum          <-  originalSpectra -  baseSpectrumMat    
  } else if ( nBaseWavelengths == 2 ){
    ##  2 baseline wavelength ( substract line in wavelength domain)
    
    baseWL1                      <-  baseWavelengths[ 1 ]
    baseWL2                      <-  baseWavelengths[ 2 ]
    baseData1                    <-  baselineInfo[ , 1 ]
    baseData2                    <-  baselineInfo[ , 2 ]
    deltaX                       <-  baseWL2 - baseWL1
    deltaY                       <-  baseData2 - baseData1
    rico                         <-  deltaY/deltaX
    waveDiffVersusBase1          <-  getSpectralAxis( object ) -  baseWL1
    
    # convert everything to correct dimensions
    baseData1Mat                 <-  matrix( baseData1  , byrow = FALSE , nrow = spectralDims[1] , ncol = spectralDims[2] )
    ricoMat                      <-  matrix( rico  , byrow = FALSE , nrow = spectralDims[1] , ncol = spectralDims[2] )
    waveDiffVersusBase1Mat       <-  matrix( waveDiffVersusBase1  , byrow = TRUE , nrow = spectralDims[1] , ncol = spectralDims[2] )
    
    
    baseSpectrum                 <-  baseData1Mat +   ricoMat * waveDiffVersusBase1Mat

    
    correctedSpectrum            <-  originalSpectra - baseSpectrum 
   
  } else {
    stop( "'baseWavelenghts should be either NULL or a numeric vector of length 1 or 2'" )
  }
  
  
  ## output prep and check 
  
  
  baselineCorrectedSpectrum    <-  object 
  baselineCorrectedSpectrum@spectra  <-  correctedSpectrum 
  validObject                  <-  validObject( baselineCorrectedSpectrum  )
  if( validObject ) {
    return( baselineCorrectedSpectrum )
  } 
}




### multiplicative scatter correction

#' multiplicative scatter correction on 1 function
#' 
#' @param x numerical vector of measurements
#' @return corrected spectrum 
#' @importFrom stats lm
#' @keywords internal
multiplicativeScatterCorrect                   <-  function( x , xRef ){
	nObs                                       <-  length( x )			
	linMod                                     <-  lm( x ~ xRef )
	modelCoef                                  <-  coef( linMod )
	correctedSpectrum                          <-  ( x - modelCoef[1] )/ modelCoef[2]
	correctedSpectrum
}




#' perform multiplicative scatter correction on spectralData
#' 
#' @importFrom  plyr aaply 
#' @author Adriaan Blommaert
#' @keywords internal
scatterCorrectSpectra                          <-  function( object , referenceSpectra = NULL , referenceMethod = "mean" ){
	spectraOnly                                <-  getSpectra( object ) 
  arguments                                  <-  list( "scatterCorrect" , referenceSpectra = referenceSpectra ,  referenceMethod = referenceMethod )

	# getReference spectra
	if( is.null( referenceSpectra ) ){
		referenceSpectra                       <-  apply( spectraOnly , 2 , referenceMethod )
	}
	
	spectraCorrected                           <-  aaply( spectraOnly , 1 ,  multiplicativeScatterCorrect , xRef = referenceSpectra )
	newSpectra                                 <-  addPreprocessingStep( object = object , newSpectra =  spectraCorrected , preprocessingInfo = arguments )
	newSpectra           
}  



#' @rdname scatterCorrect
#' @examples
#' 
#'  object1   <-  getSpectraInTimeExample()
#'  object2   <-  scatterCorrect( object1 )
#'  \donttest{
#'    plot( object1 )
#'    plot( object2 )
#' }
#' @keywords internal
setMethod( scatterCorrect , "SpectraInTime" , scatterCorrectSpectra )





