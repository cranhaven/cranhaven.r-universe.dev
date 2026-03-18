#'
#' @rdname AOItransition
#' 
#' @name AOItransition
#' 
#' @title calculate transitions for eye-tracking data
#' 
#' @description function to calculate transitions between AOI's for eye-tracking data
#' 
#' @param AOI character vector with one entry per fixation, containing AOI's upon 
#'   which was fixated
#' 
#' @details Indicates each time a participant's fixation moves from one AOI to 
#'   r AOI whereby absence of transition is coded as 0 and transitions are 
#'   coded as X-Y whereby
#'   
#'   X = ID of AOI left
#'   
#'   Y = ID of AOI entered
#'   
#' @examples # The function accepts a single AOI column
#'   data(SimData )
#'   
#'   AOItransitions( SimData$AOI )
#' 
#' @author Tine van Daal [aut], \email{tine.vandaal@@uantwerpen.be}
#'   San Verhavert [ctb], \email{san.verhavert@@uantwerpen.be}
#' 
#' @export AOItransitions
#' 

AOItransitions <- function( AOI )
{
  if( length( AOI ) <= 1 )
    stop( "AOI should be a vector containing more than 1 element" )
  
  out <- character( length( AOI ) )
  out[1] <- 0
  
  for( i in 1:( length( AOI ) - 1 ) )
  {
    if( AOI[ i+1 ] == AOI[i] )
    {
      out[ i+1 ] <- 0
    } else
    {
      out[i+1] <- paste( AOI[i], AOI[ i+1 ], sep = "-" )
    }
  }
  rm(i)
  
  return( out )
  }
