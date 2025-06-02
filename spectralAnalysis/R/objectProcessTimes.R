#' S4 Class key process times   
#'
#' @slot experimentName character vector with name of the experiment
#' @slot timeHeatingAboveMin time when experiment above minimum temperature
#' @slot timeStartReaction time start reaction (end of heating ramp)
#' @slot timeEndProcess time timeEndProcess  time end of the process, when cooling down starts
#' @slot Tset the maximum temperature to indicate \code{timeStartReaction}
#' @slot comments character vector of comments when \code{NA} values are produced 
#' @author Adriaan Blommaert
#' @name ProcessTimes-class
#' @aliases TemperatureInfo temperatureInfo temperatureinfo
#' @export 
ProcessTimes                        <-  setClass( "ProcessTimes" , slots = c(
    experimentName       =  "character" ,
    timeHeatingAboveMin  =  "POSIXct" ,
    timeStartReaction    =  "POSIXct" ,
    timeEndProcess       =  "POSIXct" ,
    Tset                 =  "numeric" ,
    comments             =  "character"
    )
  )
  
#' get a minimal \code{\link{ProcessTimes-class}} example based on 
#' \code{\link{getSpectraInTimeExample}}
#' 
#' @author Adriaan Blommaert
#' @examples
#'  getProcessTimesExample()
#' @export  
#' @return \code{\link{ProcessTimes-class}}
#' @importFrom stats quantile
getProcessTimesExample              <-  function(){
  spectraInTime                     <-  getSpectraInTimeExample()
  timeAxis                          <-  getTimePoints( spectraInTime )
  startAndEndTime                   <-  quantile( timeAxis , probs = c( 0.25 , 0.75 ) )
  startTimeExperiment               <-  getStartTime( spectraInTime )
  new( "ProcessTimes" ,
    experimentName        =  getExperimentName( spectraInTime ) , 
    timeHeatingAboveMin   =  startTimeExperiment  + 0.5*60*60 ,
    timeStartReaction     =  unname( startTimeExperiment + startAndEndTime[ 1 ] ),
    timeEndProcess        =  unname( startTimeExperiment + startAndEndTime[ 2 ] ) ,
    Tset                  =  100 ,
    comments              = ""
    )
}
  
ProcessTimes.validity              <-  function( object ){
  errors                           <-  character()
   ## checks 
  checkEndAfterStart               <-  object@timeStartReaction    <  object@timeEndProcess
  checkHeatingForStart             <-  object@timeHeatingAboveMin  <  object@timeStartReaction
  checkStartHeatingForEnd          <-  object@timeHeatingAboveMin  <  object@timeEndProcess
  
  ## error message if not NA, (missing values are no errors)
  if( !is.na( checkEndAfterStart )       &  !checkEndAfterStart ) {
    errors                         <-  addMessage( errors, " 'timeStartReaction' should be before 'timeEndProcess'" )
  }
  if( !is.na( checkHeatingForStart )     &  !checkHeatingForStart ) {
    errors                         <-  addMessage( errors, "'timeHeatingAboveMin' should be before 'timeStartReaction'" )
  }
  if( !is.na( checkStartHeatingForEnd )  &  !checkStartHeatingForEnd ) {
    errors                         <-  addMessage( errors , "'timeHeatingAboveMin' should be before 'timeEndProcess'" )
  }
 
  ## return output
  if( length( errors ) == 0 ){
    return( TRUE ) 
  } else {
    cat(errors) 
    return( FALSE )
  }
}      

setValidity( "ProcessTimes" , ProcessTimes.validity )  



#' ProcessTimes-class
#' 
#' S4 Class key process times in a data frame, every line is convertable to a \code{\link{ProcessTimes-class} } 
#'
#' @slot processTimes data.frame with every line process times of an experiment 
#' @name ProcessTimesFrame-class
#' @author Adriaan Blommaert
#' @return \code{\link{ProcessTimes-class}}
#' @export 
setClass( "ProcessTimesFrame"  , slots = c( processTimes = "data.frame" )  )


#' get mimimal example \code{\link{ProcessTimesFrame-class}}
#' 
#' @author Adriaan Blommaert
#'@return \code{\link{ProcessTimes-class}}
#' @export
getProcessTimesFrameExample      <-  function() {
	times1                         <-  getProcessTimesExample()
	times2                         <-  times1 
	timeFormat                     <-  
	times2@timeStartReaction       <-  times1@timeStartReaction   + 20*60 # start 20 min later 
	times2@experimentName          <-  "ABLOMMAERT-02-00347"
	
	times1List                     <-  convertS4ToList( times1 )
	times2List                     <-  convertS4ToList( times2 )
	
	frame                          <-   rbind( data.frame( times1List , stringsAsFactors = FALSE ) , data.frame( times2List)  )
	dataColumns                    <-   c( "timeHeatingAboveMin" ,  "timeStartReaction" ,  "timeEndProcess" ) 
#	dataColumns                    <-  2
	formattedTimes                 <-  lapply( dataColumns , function( column ) {
				as.POSIXct(   as.numeric( frame[ , column ] )  , origin =  "1970-01-01 00:00.00 UTC" ) # default R origin used 
				
				
			}  
	) # needed because as.POSIXlt not vectorized on data.frame
	frame[ ,dataColumns ]          <-  formattedTimes
	new( "ProcessTimesFrame" , processTimes =  frame  )
	
}


#' get example list of spectra
#' 
#' @return list of \code{\link{SpectraInTime-class}}
#' @export
getListOfSpectraExample          <-  function(){
	spectra1                       <-  getSpectraInTimeExample()
	spectra2                       <-  spectra1
	spectra2@timePoints            <-  spectra1@timePoints  -  20*60 
	spectra2@spectra               <-  spectra1@spectra *1.25 # slightly different spectra 
	spectra2@experimentName        <-  "ABLOMMAERT-02-00347"
	output                         <-  list( obj1 =  spectra1 , obj2 = spectra2 )
	return( output )
}

# validity function
# object <-  getProcessTimesFrameExample()
# str( object )
processTimesFrame.validity       <-  function( object ) {
	errors                         <-  character()
  processTimes                   <-  object@processTimes
	objectNames                    <-  colnames( processTimes )
	defaultNames                   <-  slotNames( new("ProcessTimes") )
	checkNames                     <-  all( objectNames  %in% defaultNames )
	if( ! checkNames ) {
		errors                       <-  addMessage( errors , paste0("column names should be: " , paste(defaultNames , collapse = " ; ") ) )
  }
		
  ## check lines can be converted to object, individual object processing
  # iRow = 1 
  testErrorsInternalClass        <-  try( {
         test  <-  lapply( seq_along( nrow( processTimes ) ) , function( iRow ){
               listObject         <-   as.list( processTimes[ iRow , ] )  
               convertListToS4( listObject , class = "ProcessTimes"  )  
            }
        
         )
         NULL
	 } , silent = TRUE  )	
  errors          <-  c( errors , testErrorsInternalClass )

  ## return output
  if( length( errors ) == 0 ){
    return( TRUE ) 
  } else {
    cat(errors) 
    return( FALSE )
  }    
}
	

setValidity( "ProcessTimesFrame" , processTimesFrame.validity )



#' example path process times 
#' @return \code{\link{ProcessTimes-class}}
#' @export
getPathProcessTimesExample             <-  function( ){
  pathEx                                <-  system.file( "exampleData/exampleProcessTime.csv" , package = "spectralAnalysis" )
  pathEx  
}

#' read .csv file as process times 
#' 
#' @param path to the file containing process times information
#' @param timeFormat character specifying time format \code{\link[base]{as.POSIXct}}
#' @return \code{\link{ProcessTimesFrame-class}}
#' @examples
#'  readProcessTimes( getPathProcessTimesExample() , timeFormat =  "%Y-%m-%d %H:%M:%S" )
#' @importFrom utils read.csv
#' @return \code{\link{ProcessTimes-class}}
#' @export
readProcessTimes                      <-  function( path ,  timeFormat = "%Y-%m-%d %H:%M:%OS" ) {
  timeInfo                            <-  read.csv( path, stringsAsFactors = FALSE, row.names = NULL  )
  defaultNames                        <-  slotNames( new( "ProcessTimes" )  )
  checkNames                          <-  all( defaultNames %in% colnames( timeInfo ) )
  if( !checkNames ) {
    stop( paste0( "headers of file should be: ") , paste( defaultNames, collapse = "; ") )
  }
  
  timeInfoOrdered                     <-  timeInfo[ , defaultNames ]
  
  ## transform to time variables
  timeVariables                       <-  c( "timeHeatingAboveMin" , "timeStartReaction" , "timeEndProcess" )
  for( var in timeVariables ) {
        timeInfoOrdered[ , var ]      <-   as.POSIXct( timeInfoOrdered[ , var ]  , format = timeFormat )
      }
  timeInfoOrdered[ , "comments" ]     <-  as.character( timeInfoOrdered[ , "comments" ]   )
  processTimesFrame                   <-  new( "ProcessTimesFrame" , processTimes = timeInfoOrdered ) # checks automatically 
  return( processTimesFrame )
}

