#' @include allGenericFunctions.R objectProcessTimes.R objectSpectraInTime.R 
NULL


#' @rdname checkCompatible
#' @aliases [ProcessTimes,SpectraInTime-method [SpectraInTime,ProcessTimes-method
#' @return \code{TRUE} when the aer competible, otherwise it stops and prints a list of error messages
setMethod( "checkCompatible" , 
  signature = c( x = "SpectraInTime" , y = "ProcessTimes" ) , definition = function( x , y ){
    errors                             <-  character()
    
    ## checks
    checkEqualExperiment               <-  x@experimentName == y@experimentName 
    if( !checkEqualExperiment ){
      errors                           <-  addMessage( errors , "Unequal experiment names" )
    }
    checksExperimentStartForProcessStart  <-  x@startTime < y@timeHeatingAboveMin
   if( is.na( checksExperimentStartForProcessStart ) ) {
     warning( "times of objects of class 'SpectraInTime' , 'ProcessTimes' cannot be compared because of missing values" , call. = FALSE )
   }
   if( !is.na( checksExperimentStartForProcessStart) & ! checksExperimentStartForProcessStart ) {
     errors                          <-  addMessage( errors , "Time start experiment ('SpectraInTime-class') is later than timeHeatingAboveMin ('ProcessTime-class')   " )
   }
    
    ## error messages showing
    generalErrorMessage              <-  "Object 'SpectraInTime' and object 'ProcessTimes' are incompatible"
    if( length( errors ) >= 1L ){
      stop( generalErrorMessage, ": " , "\n" , errors , domain = NA , call. = FALSE  )
    } else {
      TRUE
    }
  }
)



#' @rdname checkCompatible
setMethod( "checkCompatible" , signature = c( x = "ProcessTimes"  , y = "SpectraInTime" ) , 
  definition = function( x , y ){
    checkCompatible( x = y , y = x  )
  }
)



