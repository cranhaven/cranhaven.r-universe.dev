#' add an message to list of messages for printing #
#' 
#' @keywords internal 
addMessage                      <-  function( messageList , message ) {
  msgNewLine                    <-  paste0( message , "\n" )
  c( messageList , msgNewLine )
}


processValididtyErrors          <-  function( errors ) {
  if( length( errors ) == 0 ){
    return( TRUE ) 
  } else {
    cat(errors) 
    return( FALSE )
  }
}


#' Function to overwrite default settings
#' 
#' Changes are printed print to the console when changing default settings and adding to default settings 
#' 
#' @param defaultSettings list of settings
#' @param replaceSettings list of settings to overwrite defaults or add to defaults
#' @return list
#' @keywords internal 
changeDevSettings                <-  function( defaultSettings , replaceSettings ){
  newSettings                    <-  defaultSettings # initialize 
  flagElementsToReplace          <-  names( defaultSettings ) %in% names( replaceSettings )
  elementsToReplace              <-  names( defaultSettings )[ flagElementsToReplace ]
  newSettings[ elementsToReplace ] <-  replaceSettings[ elementsToReplace ]
  # note changes 
  if( length( elementsToReplace ) > 0  ) {
    cat("deviation from default settings:" , paste(  elementsToReplace , " = " , newSettings[elementsToReplace]  , sep = "" , collapse = ";\n")   , "\n" )
  }
  indSettingsNotInDefault        <-  names( replaceSettings )[ !( names( replaceSettings ) %in% names( defaultSettings ) ) ]
  newSettingsFull                <-  c( newSettings , replaceSettings[ indSettingsNotInDefault ]  )
  return( newSettingsFull )
}


#' flag vector elements within interval (closed interval )
#' 
#' @param vector numeric vector
#' @param interval numeric vecor with 2 values
#' @note if interval is \code{NULL} then no selection  is performed  
#' @keywords internal
#' @author Adriaan Blommaert 
flagVectorInInterval               <-  function( vector , interval ) {
  if( is.null(interval) ) {
    return( rep( TRUE , length( vector ) ) )
  }
  flagAboveLower                   <-  vector >= min( interval )
  flavBelowUpper                   <-  vector <= max( interval )
  flagInInterval                   <-  flagAboveLower &  flavBelowUpper
  flagInInterval
}


#' return first element (of a vector)
#' @keywords internal
first                      <-  function( x ) {
  x[ 1 , drop = FALSE ]
}



#' return laste element( of a vector )
#' @keywords internal 
last                       <-  function( x ) {
  x[ length( x ) , drop = FALSE ]
}


if( 0 == 1 ) {
  elements  =  c( 1.25 , 2.6 )
  vector    =  1:10
}
#' get closest element for elements looked up in a vector 
#' 
#' @param vector numeric vector values to match to
#' @param elements numeric vector 
#' @keywords internal
#' @author Adriaan Blommaert 
getClosestElements            <-  function( vector , elements ) {
  
  indClosetElement            <-  vapply( elements , function( x , vec = vector ) {
      which.min( abs( x - vec ) )  
    }  ,  FUN.VALUE =  1 )
  
}
### help functions for plotting 


#' check timeeUnit  and produce error if not correct unit
#' 
#' @param timeUnit character vector
#' @keywords internal 
checkTimeUnit                  <-  function( timeUnit ) {
  allowedTimeUnits             <-  c( "seconds" , "minutes" , "hours" )
  checkUnitAllowed             <-  timeUnit %in% allowedTimeUnits 
  checkOneUnit                 <-  length( timeUnit ) == 1 
  if( !(checkOneUnit) | ! checkUnitAllowed ) {
    stop( "'timeUnit' should be only one choice  in " , paste( allowedTimeUnits , colapse = "; " )  )
  } 
}

#' transform timeUnit to convert from seconds to specified unit
#'
#'  @param timeUnit character vector 
#' @keywords internal
getTimeConversionFactor          <-  function( timeUnit ) {
  checkTimeUnit( timeUnit )
  unitTable                      <-  c( seconds = 1 , minutes = 1/60 , hours = 1/60^2  )
  timeConversionFactor           <-  unitTable[ timeUnit ]
  timeConversionFactor
}



### help with S4 objects

#' convert an S4 object into a list
#' 
#' @param object and S4 object  
#' @keywords internal
convertS4ToList               <-  function( object ){
	slotsToExtract              <-  slotNames( object )
	slotList                    <-  lapply( slotsToExtract , function( slot ) {
				slot( object = object, name = slot )
			} 
	)
	names( slotList )           <- slotsToExtract  
	return( slotList )
}


if( 0 == 1 ) {
  object                      <-  getProcessTimesExample()
	list                        <-  convertS4ToList( object  )
  lapply( list , "class" )
  object2                     <-  convertListToS4( list , class = "ProcessTimes" )
  identical( object , object2 )
  
  # debugging 
  list  = listObject 
  class = "ProcessTimes" 
  convertListToS4( listObject , class = "ProcessTimes"  )    
  
  # listObject  is [1] "POSIXlt" "POSIXt" 
  # list is       [1] "POSIXct" "POSIXt" 
 
}  


#' convert an a named list to S4 object
#' 
#' @param list list
#' @param class character vector indicating S4 class  
#' @keywords internal
#' @importFrom methods slot slotNames slot<-
convertListToS4               <-  function( list , class ) {  
	S4Object                    <-  new( class ) 
	slotNames                   <-  slotNames( S4Object )
  listNames                   <-  names( list )
  checkNames                  <-  all( listNames   %in%  slotNames ) 
  checkEqualNameLength        <-  length( listNames ) == length( slotNames  )
  if( ! checkNames ) {
    stop( "names of 'list' do not match 'class'" )
  }
  if( ! checkEqualNameLength ) {
    warning( "some elements will be lost by conversion to S4 object" )
  }
	for( slot in slotNames) {
        slot( S4Object  , slot, check = FALSE ) <-  list[[ slot ]]  # check whole object at once 
      }
  isValidObject               <-  validObject( S4Object )
    return( S4Object )	    
}




if( 0 == 1 ){
  listOfObjects = listOfSpectra
  class     <-  "spectraInTime"
}

#' check wether all elements of of the same class
#' 
#' @param listOfObjects a list of S4 objects to check
#' @param class a class to compare with 
#' @return logical value \code{TRUE} if all objects are of the correct class
#' @author Adriaan Blommaert
#' @export
checkIdenticalClass      <-  function( listOfObjects , class ){
  classPerObject         <-  sapply( listOfObjects , function(x){ class( x ) == class }  )
  all( classPerObject  )
}