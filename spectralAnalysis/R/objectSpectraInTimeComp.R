#' @include allGenericFunctions.R
NULL



#' SpectraInTimeComp-class (time resolved spectra )
#' 
#' Spectral-time data for 1 experiment with dimension reduction techique NMF and/or PCA decomposition included
#'
#' @slot dimensionReduction list containing dimension reduction techique, either PCA or NMF, but only one per kind. 
#' @author Adriaan Blommaert
#' @importClassesFrom NMF NMF
#' @name SpectraInTimeComp-class
#' @examples
#' 
#' \donttest{
#' 
#'   #  generate example 
#'  exampleSpectra     <-  getSpectraInTimeCompExample()
#' 
#'   # methods
#'      PCAResult      <-  getDimensionReduction( exampleSpectra, type = "PCA" )
#'      NMFResult      <-  getDimensionReduction( exampleSpectra, type = "NMF" )
#'      
#'      dimensionReductions  <-  getDimensionReduction( exampleSpectra )
#'      str(dimensionReductions  )
#' 
#'   # subsetting works by reducing to \code{\link{SpectraInTime-class}}
#'   subsetting     <-  exampleSpectra[1:3 , r(400, 450)]
#'   # preprocessing methods also reduce the object to \code{\link{SpectraInTime-class}}
#' 
#' }
#' 
#' @aliases SpectraInTimeComp spectraInTimeComp  spectraintimecomp
#' @export
SpectraInTimeComp      <-  setClass ( "SpectraInTimeComp",
    slots = list(
        dimensionReduction  =  "list" 
    ) , 
    contains = "SpectraInTime"
)


validitySpectraInTimeComp     <-  function( object ) {
  errors                      <-  character()
  ## check dimension reduction (the rest is for spectralAnalysis ) 
  dimensionReduction          <-  object@dimensionReduction
  namesDimReduction           <-  names( dimensionReduction )
  
    # names
  checkNames                  <-  all( namesDimReduction %in% c("NMF" , "PCA", "PLS") )
  if( !checkNames ){
    errors                    <-  addMessage( errors, "dimensionReduction should have 'PCA', 'NMF' and 'PLS' as element names" )
  }
  
    # PCA slot 
  PCAObject                   <-   dimensionReduction$PCA
  if( !is.null(PCAObject) ) {
    # only additional checking when object is present
       # class 
    checkPCAClass             <-  is( PCAObject , "prcomp")
    if( !checkPCAClass ){
      errors                  <-  addMessage( errors, "'dimensionReduction$PCA'is not of class 'prcomp'" )
    }
        # dimensions
    checkPCADim               <-  all( dim( getSpectra(object) ) == rev( dim( PCAObject$rotation )  ) )
    if( !checkPCADim ){
      errors                  <-  addMessage( errors, "Dimensions of 'dimensionReduction$PCA' do not match spectral dimensions" )
    }
  }
  
   # NMF slot 
  NMFObject                   <-  dimensionReduction$NMF
  if( !is.null( NMFObject ) ){
    # only check NMF object when present 
       # class
    
    checkClassList            <-  is( NMFObject , "list" )
    if(!checkClassList){
      errors                  <-  addMessage( errors, "NMF slot should be of type list" )  
    }
    
    nmfNames                  <-  c( "NMF" , "settings" , "experimentsUsed" )
    nmfClasses                <-  c( "NMF" , "list" , "character" )
    checkNMFNames             <-  all( names(  NMFObject ) == nmfNames )
    if( !checkNMFNames ){
      errors                  <-  addMessage( errors , paste0("NMF slot names should match: " , "'" ,  paste( nmfNames , collapse = "' ,  '" ) , "'" ) )  
    }
    checkNmfClasses           <-  all( mapply( FUN = function(x , y){ is( x , y ) } , x = NMFObject , y = nmfClasses ) )
    if( ! checkNmfClasses ){
      errors                  <-  addMessage( errors, paste0( "'dimensionReduction$NMF' is not a list containing classes:" , "'" ,  paste( nmfClasses , collapse = "' ,  '" ) , "'" )   )
    }
      # dimensions
    checkNmfDim               <-  all( dim( NMFObject$nmfResult )[ - 3 ] == rev( dim( getSpectra( object ) ) ) )
    if( !checkNmfDim ){
      errors                  <-  addMessage( errors, "dimensions of slot  'dimensionReduction$NMF' do not match spectral dimensions" )
    } 
  }
  
  ## return validity conclusion + error messages
  if( length( errors ) == 0 ){
    return( TRUE ) 
  } else {
    cat(errors) 
    return( FALSE )
  }
}

#setValidity( "SpectraInTimeComp" , validitySpectraInTimeComp )


#' Artificial example of \code{\link{SpectraInTimeComp-class}} 
#' 
#' Example \code{\link{SpectraInTime-class}} with nmf result 
#' using random initialization with rank 2
#' 
#' @return \code{\link{SpectraInTimeComp-class}}
#' @examples 
#' \donttest{
#' 	test <-  getSpectraInTimeCompExample() 
#' }
#' @author Adriaan Blommaert
#' @export
getSpectraInTimeCompExample     <-  function(){
  spectraInTimeEx               <-  getSpectraInTimeExample()
  nmfResulspectraInTimeExNMF    <-  spectralNMF( spectraInTimeEx , rank = 2 , nruns = 5 , subsamplingFactor = 5 )
  pcaResult                     <-  prcomp( getSpectra( spectraInTimeEx ) )
  NMFPart                       <-  getDimensionReduction( nmfResulspectraInTimeExNMF , type = "NMF")
  dimensionReduction            <-  list( PCA = pcaResult, NMF = NMFPart )
  SpectraInTimeComp( dimensionReduction = dimensionReduction,  spectraInTimeEx )
}



### Methods 


#' @rdname SpectraInTimeComp-class
#' @param object of class SpectraInTimeComp-class
#' @param type type of regression method specified, if NULL the entire slot is returned as a list
setMethod( "getDimensionReduction" , signature = "SpectraInTimeComp" ,
    definition = function( object , type = NULL ){
      if( is.null(type) ){
        return( object@dimensionReduction)
      } else {
        return( object@dimensionReduction[[ type ]] )
      }
    } 
)

#
##' @rdname SpectraInTimeNmf-class
##' @importFrom NMF coef
#setMethod( "coef" , signature = "SpectraInTimeNmf" ,
#    definition = function( object ){
#      NMF::coef( nmfResult( object ) )
#    } 
#)
#
#
##' @rdname SpectraInTimeNmf-class
##' @importMethodsFrom NMF basis
#setMethod( "basis" , signature = "SpectraInTimeNmf" ,
#    definition = function( object ){
#      NMF::basis( nmfResult( object ) )
#    } 
#)
#
## TODO modifying spectra == rerunning PCA/NMF analysis?
#
