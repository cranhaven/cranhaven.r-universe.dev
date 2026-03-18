#'
#' @rdname compileAOI
#' 
#' @name compileAOI
#' 
#' @title compile AOI columns in one column
#' 
#' @description Compiles the information on AOI's in separate variables to one variable
#' 
#' @param data A data frame containing fixation information of an eye tracking 
#'   experiment. Each row indicates a fixation.
#' @param AOI A vector containing the name or number of the columns in \code{data} 
#'   indicating if the respective AOI was fixated (1) or not (0).
#' @param labels [optional] A vector containing the names of the AOI in the same 
#'   order as the column names or -numbers provided to \code{AOI}
#' 
#' @details This function can be used to convert a wide format eye tracking data 
#'   frame to a long format eye tracking data frame. It takes a data frame with 
#'   multiple binary variables that indicate whether an AOI is fixated on (=1) or 
#'   not (=0) and returns a vector with the AOI's that were fixated on.  
#'   
#'   NOTE: if the names of the AOI columns passed to the function are just numbers, 
#'   make sure to pass these as character for the function to work properly. The 
#'   function does not check for this case.
#' 
#' @return A factor that contains the AOI fixated on for each row in
#'   the data frame whereby absence of a fixation on AOI is coded as 0.
#' 
#' @examples 
#'   data( SimData )
#'   
#'   # compileAOI accepts AOI's as names
#'   compileAOI( data = SimData, AOI = c( "AOI1", "AOI2", "AOI3" ) )
#'   
#'   # and as column numbers
#'   compileAOI( data = SimData, AOI = 2:4 )
#'   
#'   # and it returns 0 if some fixations are oitside the provided AOI's
#'   compileAOI( data = SimData, AOI = c( "AOI1", "AOI2" ) )
#'   
#'   # it is also possible to suply different lables for the AOI's
#'   compileAOI( data = SimData, AOI = c( "AOI1", "AOI2", "AOI3" ),
#'               labels = c( "1", "2", "3" ) )
#'   
#' @author
#'   Tine van Daal [aut], \email{tine.vandaal@@uantwerpen.be}
#'   
#'   San Verhavert [ctb], \email{san.verhavert@@uantwerpen.be}
#' 
#' @export compileAOI
#' 

compileAOI <- function( data, AOI, labels = NULL )
{
  if( !is.data.frame( data ) ) stop( "data should be a data frame" )
  
  if( is.character( AOI ) )
  {
    if( !all( AOI %in% colnames( data ) ) )
      stop( "not all values provided to AOI are column names for data" )
    
    data <- data[ , match( AOI, colnames( data ) ) ]
    AOI <- match( AOI, colnames( data ) )
  }
  
  if( any( rowSums( data[ ,AOI ] ) > 1 ) ) 
    warning( paste0( "Some rows have a fixation in more than one AOI. Only the ",
                    "first will be used" ) )
  
  if( !is.null( labels ) )
  { 
    if( length( AOI ) != length( labels ) ) stop( "AOI and labels lengts differ" )
    colnames( data )[ AOI ] <- labels
  } else labels <- colnames( data )[ AOI ]
  
  data <- data[ , AOI ]
  
  out <- character( nrow( data ) )
  
  for(i in 1:nrow( data ) )
  {
    out[i] <- ifelse( ( rowSums( data[ i, ] ) ) == 0,
                      yes = 0,
                      no = colnames( data[ i, ] )[ which( data[ i, ] == 1 ) ] )
  }
  
  if( any( out == 0 ) ) labels <- c( "0", labels )
  
  return( factor( out, levels = labels ) ) #Factor to allow not fixed AOI to be included in duration table
}
