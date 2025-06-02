# Project: spectralAnalysis-R-package
# 
#  Subsetting methods with addition of range subsetting, and closest element matching
#
# Author: ablommaert
###############################################################################



#' @include internalHelpers.R allGenericFunctions.R objectSpectraInTime.R
NULL




#' Elements S4 class useful for closest elements subsetting
#' 
#' 
#' @slot elements numeric vector of elements
#' @author Adriaan Blommaert 
#' @name ElementsToSelect-class
#' @export 
setClass( "ElementsToSelect" , slots = c( elements = "numeric" ) )

 
#' Create an \code{\link{ElementsToSelect-class}} from a numeric vector or multiple numeric values or vectors
#' 
#' @param x numeric  vector
#' @param ... additional numeric vectors
#' @return \code{\link{ElementsToSelect-class}} with unique elements
#' @export
#' @examples
#' e( 1 , 5, 4.5  )
#' e( 1:10 , c(4 , 5 , 6 ) , 7 )
e          <-   function( x , ... ) { 
    
    new( "ElementsToSelect" , elements = unique( as.numeric( c(x , ...) ) )  )
}



#' RangeToSubset-class
#' 
#' Allows for subsetting a range of actual values instead of a range of indicators
#' 
#' @slot range numeric vector with min and max value 
#' @name RangeToSubset-class
#' @aliases RangeToSubset rangetosubset Rangetosubset 
#' @author Adriaan Blommaert 
#' @export 
setClass( "RangeToSubset" , slots = list( range = "numeric"  ) )


Range.validity           <-  function( object ) {
  ## sett abject
  errors                 <-  character()
  range                  <-  object@range
  min                    <-  range[ 1 ]
  max                    <-  range[ 2 ]
  ## tests 
  checkLength            <-   length( range ) == 2
  if( ! checkLength  ) {
    errors               <-  addMessage( errors ,  "Range object should have 2 values" ) } 
  checkMinMax            <-  min <= max
  if( ! checkMinMax  ) { 
    errors               <-  addMessage( errors ,  "'min' should be smaller then 'max'" ) } 
  ## produce error message 
  processValididtyErrors( errors )
}

setValidity( "RangeToSubset" ,  Range.validity )


#' @rdname getRange
#' @export
setMethod( f = "getRange" , signature = "RangeToSubset" , 
  definition = function( object ) { 
    return( object@range ) 
  }
)

#' @rdname getElements
#' @export
setMethod( f = "getElements" , signature = "ElementsToSelect",
  definition = function( object ){
    return( object@elements )
    
  }
)




#' create a \code{\link{RangeToSubset-class}} object from 2 elements or from a vector
#' 
#' @param x numeric value or vector of numeric values
#' @param y numeric value missing when x is a vector of values 
#' @name r
#' @return \code{\link{RangeToSubset-class}}
#' @export
setGeneric( name = "r" , def = function( x , y  ) { 
    rangeVec             <-  range( x )
    new( "RangeToSubset" , range = c( min = rangeVec[ 1 ]  , max = rangeVec[ 2 ] ) )
  }
)


#' @rdname r
setMethod( "r" , signature( x = "numeric" , y = "numeric" ) , definition = function( x , y ) {
    new( "RangeToSubset" , range = c( min = min( x, y ) ,  max = max( x , y ) )  )
  } 
)

#' @rdname r
setMethod( "r" , signature( x = "RangeToSubset" , y = "missing" ) , definition = function( x , y ) {
      return( x )
    } 
)



#' internal function for subsetting 
#' @keywords internal 
subset.SpectraTime              <-  function( x , i  , j , timeUnit = "seconds" , timePointsAlt = FALSE , drop = "" ) { 

  ## extract basic elements 
  times                        <-  getTimePoints( x , timePointsAlt = timePointsAlt , timeUnit = timeUnit  )
  spectralAxis                  <-  getSpectralAxis( x )
  specialSubsetClasses         <-  c( "RangeToSubset" , "ElementsToSelect" )
  checkIIsSpecialSubsetting    <-  class( i ) %in% specialSubsetClasses
  checkJIsSpecialSubsetting    <-  class( j ) %in% specialSubsetClasses
  
  ## Range subsetting (convert range to indicators ) # TODO 
  if( is( i, "RangeToSubset") ) {
    iTimes                     <- flagVectorInInterval( times ,  getRange( i  )  )
  } 
  if( is( j,  "RangeToSubset") ) {
    jSpectralVals               <- flagVectorInInterval( spectralAxis , getRange( j ) )
  }
  
  ## Closest element subsetting  (convert ElementsToSelect to indicators )
  if( is( i,  "ElementsToSelect")  ) {
    iTimes                     <- unique( getClosestElements( times ,  getElements( i  )   ) )
  } 
  if( is( j, "ElementsToSelect")  ) {
    jSpectralVals               <- unique( getClosestElements( spectralAxis , getElements( j )  ) )
  } 
  
#    ## if not range or elements, pass trough ordinary subsetting methods
  if( !checkIIsSpecialSubsetting ) {
    iTimes                     <-  i
  }
  if( !checkJIsSpecialSubsetting ) {
    jSpectralVals               <-  j     
  }
  
  ## create new object by subsetting every time and wavelength dependent slot
  newSpectra                  <-  x
  newSpectra@spectra          <-  x@spectra[ iTimes , jSpectralVals , drop = FALSE ]
  newSpectra@spectralAxis      <-  x@spectralAxis[ jSpectralVals , drop = FALSE ]
  newSpectra@timePoints       <-  x@timePoints[ iTimes , drop = FALSE ]
  newSpectra@timePointsAlt    <-  x@timePointsAlt[ iTimes , drop = FALSE ] 
#    newSpectra@preprocessing    <-  append( x@preprocessing , list( timeSelect = i , wavelengthSelect = j ) ) # not necessary you see when timepoint do not start with one 
  validObject( newSpectra )
  newSpectra
}

## Subsetting methods: taken care of missing values


#'  Subsetting  \code{\link{SpectraInTime-class}}
#' 
#' @param x object to subset
#' @param i subsetting rows ( timePoints )
#' @param j subsetting columns ( spectral axis )
#' @param  ... additional parameters 
#'   \itemize{
#'      \item timeUnit unit at which subsetting should be done choose between \code{seconds} , \code{minutes} or \code{hours} 
#' defaults to \code{seconds}
#'      \item   timePointsAlt logical indicators whater alternative timePoints should be used 
#' }
#' @param  drop for consistancy, not used 
#' @name subset-methods
#' @return \code{\link{SpectraInTime-class}}
#' @rdname subset-methods
#' @aliases [,SpectraInTime-method  [,SpectraInTime,ANY,ANY,ANY-method  [,SpectraInTime,ANY,ANY-method [,SpectraInTime,missing,ANY-method  [,SpectraInTime,ANY,missing-method  [,SpectraInTime,missing,missing-method
#' @examples 
#'  ### subsetting [ time , spectral axis, options ]
#' 
#'  spectralEx                <-  getSpectraInTimeExample()
#'  spectraSubset             <-  spectralEx[ r( 1000 , 30000 ) , r(130 , 135 ) ]
#'  spectraSubsetTime         <-  spectralEx[ r( 1000 , 30000 ) ,  ]
#'  spectraSubsetSpectralVals  <-  spectralEx[  ,  r(130 , 135 ) ]
#'  spectraSubsetHours        <-  spectralEx[ r( 1 , 3 ) , r(130 , 135 ) , timeUnit = "hours" ]
#'  closestSpectralVals        <-  spectralEx[ , e( 150, 4, 300, 500 ) ] # remark only unique values 
#'  spectraSubsetLogical      <-  spectralEx[ getTimePoints( spectralEx ) > 300   ,
#'  getSpectralAxis( spectralEx ) <= 500 ]
#' @importFrom BiocGenerics as.data.frame
#' @export
setMethod( "[",  signature = c( "SpectraInTime" )  ,
    definition =  function( x , i  , j , ... , drop = "" ) {
      extraSettings                  <-  list( ... )
      fullSettings                   <-  c( list( x = x , i = i , j = j ) , extraSettings )
      do.call( subset.SpectraTime , fullSettings   )
    }
)


## define methods for missing i and j (then no subsetting)

#' @rdname subset-methods
setMethod( "[", c( "SpectraInTime" , "missing" , "ANY" ) , 
  function( x , i  , j ,  ... , drop = "" ) {
    extraSettings                  <-  list( ... )
    fullSettings                   <-  c( list( x = x , i = TRUE , j = j ) , extraSettings )
    do.call( subset.SpectraTime , fullSettings  )
  }
)



## define methods for missing i and j (then no subsetting)


#' @rdname subset-methods
setMethod( "[", c( "SpectraInTime" , "ANY" , "missing" ) , 
    function( x , i  , j ,  ... , drop = "" ) {
      extraSettings                  <-  list( ... )
      fullSettings                   <-  c( list( x = x , i = i , j = TRUE ) , extraSettings )
      do.call( subset.SpectraTime , fullSettings  )
    }
)


#' @rdname subset-methods
setMethod( "[", c( "SpectraInTime" , "missing" , "missing" ) , 
  function( x , i  , j ,... ,  drop = "" ) {
    x   # no subsetting if everything is missing 
  }
)



## subsetting for SpectraInTimeComp
#' @rdname subset-methods
setMethod( "[" , "SpectraInTimeComp" , function(x , i , j , ..., drop = "" ){
      x                     <-  as( x , Class = "SpectraInTime" )
      callNextMethod( x =x , i= i, j = j , ... )
    } 
)

#' @rdname subset-methods
setMethod( "[" , c( "SpectraInTimeComp", "missing" , "ANY" ), function(x , i , j , ..., drop = "" ){
      x                     <-  as( x , Class = "SpectraInTime" )
      callNextMethod( x =x , i= i, j = j , ... )
    } 
)

##' @rdname subset-methods
#setMethod( "[" , c( "SpectraInTimeComp", "missing" , "missing" ), function(x , i , j , ..., drop = "" ){
#      x                     <-  as( x , Class = "SpectraInTime" )
#      callNextMethod( x =x , i= i, j = j , ... )
#    } 
#)


#' @rdname subset-methods
setMethod( "[" , c( "SpectraInTimeComp", "ANY" , "missing" ), function(x , i , j , ..., drop = "" ){
      x                     <-  as( x , Class = "SpectraInTime" )
      callNextMethod( x =x , i= i, j = j , ... )
    } 
)

