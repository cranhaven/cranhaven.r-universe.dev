#' @include internalHelpers.R allGenericFunctions.R objectLinking.R   
NULL


#' align 1 SpectraInTime with 1 cooling information
#' 
#' @param x \code{\link{SpectraInTime-class}}
#' @param y \code{\link{ProcessTimes-class}}
#' @keywords internal 
timeAlign.SpectraInTime                   <-  function( x , y , cutCooling  , cutBeforeMinTemp  ){
  ##  compatibility times with spectral data
  checkCompatible( x , y ) 
  
  ##  start new spectralObject 
  
  newSpectralObject                       <-  x
 
  ## extract time elements
  experiment                              <-  getExperimentName( x )
  timePointsOrig                          <-  getTimePoints( x )
  nTimes                                  <-  length( timePointsOrig )
  maxTime                                 <-  max( timePointsOrig )
  minTime                                 <-  min( timePointsOrig )
  spectralStartTime                       <-  getStartTime( x )
  TEndHeating                             <-  y@timeStartReaction
  coolingTime                             <-  y@timeEndProcess
  timeAboveMin                            <-  y@timeHeatingAboveMin
  
  ## time Align
  timeDiffSeconds                         <-  difftime( TEndHeating, spectralStartTime , units = "secs" )
  setTimePointsAlt( newSpectralObject )   <-  as.numeric( timePointsOrig - timeDiffSeconds )
 
  ## cutting = selection
  diffStartCooling                        <-  as.numeric( difftime( coolingTime  , spectralStartTime , units = "secs" ) )
  diffStartMinTemp                        <-  as.numeric( difftime( timeAboveMin , spectralStartTime , units = "secs" ) )
#  boolTimeAFterCooling                    <-  timePointsOrig > diffStartCooling
#  boolTimeForeMinTemp                     <-  timePointsOrig < diffStartMinTemp
    #  warnings on missing values
    if( is.na( coolingTime ) ) {
      warning( "no cooling phase observed for experiment: " , experiment , "\n" )
      diffStartCooling                    <-  max( timePointsOrig )
    }
    if( is.na( cutBeforeMinTemp) ) {
      warning( "minimum temperature never reached for experiment: " , experiment , "\n" )
      diffStartMinTemp                    <-  min( timePointsOrig )  
    }
    ## range for time selection
    minSelect                             <-  ifelse( cutBeforeMinTemp  , diffStartMinTemp , minTime  )
    maxSelect                             <-  ifelse( cutCooling , diffStartCooling , maxTime )
    timeSelectionRange                    <-  r( minSelect , maxSelect )
    
    ## return
    cutObject                             <-  newSpectralObject[ i = timeSelectionRange , j = TRUE , timeUnit = "seconds", timePointsAlt = FALSE ]  
    return( cutObject )
}


#' Align all spectra in a list 
#'  
#' @keywords internal
timeAlign.listOfSpectra          <-  function( spectralDataList , processTimes, cutCooling  , cutBeforeMinTemp ) {
# internal function to call alignStartReaction
  timeInfo                       <-  processTimes@processTimes
  # xx <- spectralDataList[[2]] 
  matchAndCut                    <-  function(xx , times = timeInfo, internalCutCooling = cutCooling , internalCutBeforeMinTemp = cutBeforeMinTemp ){
    experimentName               <-  getExperimentName( xx )
    flagExperiment               <-  experimentName == times$experimentName
    checkExperimentFound         <-  sum( flagExperiment ) == 1 
    if( !checkExperimentFound ) { stop("could not align experiment, alignment info not found") }
    timeAlignment                <-  times[ flagExperiment , ]
    objectProcesstimes           <-  convertListToS4( timeAlignment , class = "ProcessTimes" ) 
#  alignStartReaction(xx, timeAlignment, cutCooling = internalCutCooling)     
    output                       <-  timeAlign( xx , objectProcesstimes , cutCooling = internalCutCooling ,  cutBeforeMinTemp = internalCutBeforeMinTemp )
    return( output )
  }

# cut spectra  and return list of spectralData-objects
alignedSpectra                   <-  lapply( spectralDataList , matchAndCut )
return( alignedSpectra )
}



### Time alignment methods


#' @rdname timeAlign
#' 
#' @param cutCooling logical indicator if \code{TRUE} observation after cooling starts are cut off, defaults to \code{FALSE}
#' @param  cutBeforeMinTemp logical indicator if \code{TRUE} observation before minimum temperature are cut off, defaults to \code{FALSE}
#' @examples 
#' 
#'  spectra             <-  getSpectraInTimeExample()
#'  listOfSpectra       <-  getListOfSpectraExample()
#'  processTimes        <-  getProcessTimesExample()
#'  processTimesFrame   <-  getProcessTimesFrameExample()
#'  pathProcessTimes    <-  getPathProcessTimesExample()
#'   
#' ex1  <-  timeAlign( x = spectra , y = processTimes ,
#'  cutCooling = TRUE , cutBeforeMinTemp = TRUE )
#' ex2  <-  timeAlign( x = listOfSpectra , y = processTimesFrame ,
#'  cutCooling = TRUE , cutBeforeMinTemp = TRUE )
#' ex3  <-  timeAlign( x = listOfSpectra , y = pathProcessTimes, 
#'  cutCooling = TRUE , cutBeforeMinTemp = TRUE  , timeFormat =  "%Y-%m-%d %H:%M:%OS" )
#' @return  \code{\link{SpectraInTime-class}} or list of spectra depending on input
#' @export
setMethod( timeAlign , signature = c( "SpectraInTime" , "ProcessTimes" ) , definition = function( x , y , cutCooling = FALSE , cutBeforeMinTemp = FALSE ) {
      timeAlign.SpectraInTime( x = x , y = y , cutCooling = cutCooling , cutBeforeMinTemp )
    }
)


#' @rdname timeAlign
setMethod( timeAlign  , signature = c( "list" , "ProcessTimesFrame" ) ,  
		function( x , y , cutCooling = FALSE , cutBeforeMinTemp = FALSE  ) {
			testListOfSpectra( x )
      timeAlign.listOfSpectra( spectralDataList = x , processTimes = y , cutCooling = cutCooling , cutBeforeMinTemp = cutBeforeMinTemp )
		} 
)


#' @rdname timeAlign
#' @param timeFormat character vector specifying time format \code{\link[base]{as.POSIXct}}
setMethod( timeAlign , signature = c( "list" ,  "character" ) ,
    function( x, y , cutCooling = FALSE , cutBeforeMinTemp = FALSE , timeFormat = "%Y-%m-%d %H:%M:%S" ) {
      alignmentFile             <-  readProcessTimes( path = y , timeFormat = timeFormat )
      timeAlign( x = x  , y = alignmentFile , cutCooling = cutCooling , cutBeforeMinTemp = cutBeforeMinTemp   ) 
    }
 )


#' @keywords internal
#' @importFrom stats spline 
 wavelengthAlign.SpectraInTime               <-  function( ref , toAlign ){
	
	 ##  start new spectralObjects 
	 newSpectralObject1                      <-  ref
	 newSpectralObject2                      <-  toAlign
	 
	 ## extract time elements
	 WL1 				<- getSpectralAxis(ref)
	 WL2 				<- getSpectralAxis(toAlign)
	 spectra1 			<- getSpectra(ref)
	 spectra2 			<- getSpectra(toAlign)
	 
	 if(min(WL1) > max(WL2) | max(WL1) < min(WL2)) {
		 stop( "Provided SpectraInTime objects have non-overlapping spectral axes" )
	 }
	 
	 ## wavelength align
	 spectra2_new 		<- apply(spectra2, 1, spline, x=WL2, xout=WL1)
	 spectra2_new 		<- lapply(spectra2_new,"[", 2)
	 spectra2_new 		<- matrix(unlist(spectra2_new), nrow = nrow(spectra2), byrow = T)
	 
	 ## make sure outer edges of the wavelength axes are set equal
	 if(min(WL1) < min(WL2)) {
		 spectra1 		<- spectra1[,-which(WL1<min(WL2))]
		 spectra2_new 	<- spectra2_new[,-which(WL1<min(WL2))]
		 WL1 			<- WL1[-which(WL1<min(WL2))]
	 }
	 
	 if(max(WL1) > max(WL2)) {
		 spectra1 		<- spectra1[,-which(WL1>max(WL2))]
		 spectra2_new 	<- spectra2_new[,-which(WL1>max(WL2))]
		 WL1 			<- WL1[-which(WL1>max(WL2))]
	 }
	 
	 ## store aligned SpectraInTime objects in a list
	 newSpectralObject1@spectralAxis 	<- WL1
	 newSpectralObject2@spectralAxis 	<- WL1
	 newSpectralObject1@spectra 		<- spectra1
	 newSpectralObject2@spectra 		<- spectra2_new
	 
	 ## return
	 return( list(ref=newSpectralObject1, aligned=newSpectralObject2) )
 }
 
 
#' @keywords internal
 wavelengthAlign.listOfSpectra          <-  function( ref, toAlign ) {
	spectralDataAlignedList <- lapply(toAlign, wavelengthAlign.SpectraInTime, ref = ref)
	ref_new <- spectralDataAlignedList[[1]]$ref
	spectralDataAlignedList <- lapply(spectralDataAlignedList, "[", 2)
	spectralDataAlignedList <- unlist(spectralDataAlignedList)
	spectralDataAlignedList[["ref"]] <- ref_new
	
	## check if all objects in the list have the same wavelength axis
	wavelength_list <- lapply(spectralDataAlignedList, getSpectralAxis)
	
	if(length(unique(lengths(wavelength_list))) != 1) stop( "List of SpectraInTime objects to be aligned should all have the same wavelength axis" )
	
	return( spectralDataAlignedList )
}
 
 
 
 ### Time alignment methods
 
 
#' @rdname wavelengthAlign
#' 
#' @param ref \code{\link{SpectraInTime-class}} object with the reference wavelength vector
#' @param toAlign \code{\link{SpectraInTime-class}} object(s) to be aligned. This can either be a single SpectraInTime object
#' or a list of SpectraInTime objects. In case of a list, all objects in the list should have the same wavelength axis.
#' @return List of aligned SpectraInTime objects, including the reference object. 
#' @examples 
#' 
#'   spectra             <-  getSpectraInTimeExample()
#'   listOfSpectra       <-  getListOfSpectraExample()
#'   
#' # Dummy alignment of spectrum with itself:
#'   ex1                 <-  wavelengthAlign( ref = spectra , toAlign = spectra )
#' # Alignment of list of spectra with a reference spectrum:
#'   ex2                 <-  wavelengthAlign( ref = spectra , toAlign = listOfSpectra )
#' @return one or a list of \code{\link{SpectraInTime-class}}
#' @export
 setMethod( wavelengthAlign , signature = c( "SpectraInTime" , "SpectraInTime" ) , definition = function( ref , toAlign ) {
			 wavelengthAlign.SpectraInTime( ref = ref , toAlign = toAlign )
		 }
 )
 
 
#' @rdname wavelengthAlign
#' 
setMethod( wavelengthAlign  , signature = c( "SpectraInTime" , "list" ) ,  
		 function( ref , toAlign ) {
			 wavelengthAlign.listOfSpectra( ref = ref, toAlign = toAlign )
		 } 
 )
 
 
 
 ### helper functions

#' internal function to check all elements are spectraInTime 
#' 
#' 
#' @keywords internal
testListOfSpectra                    <-  function( list ) {
  checkListClass                     <-  lapply( list ,
      function( object ) {
        class( object ) == "SpectraInTime"
      }
  )
  checkAllSpectra                     <-  all( unlist( checkListClass ) )
  if( !checkAllSpectra ) {
    stop( "not all elements in list are of class 'SpectraInTime'" )
  }
  experimentNames                     <-  lapply( list , function( x ) { getExperimentName( x) } )
  checkUniqueExperiments              <-  anyDuplicated( experimentNames  )  == 0
  if( ! checkUniqueExperiments ) {
    stop( "duplicate experiment names" )
  }
}




