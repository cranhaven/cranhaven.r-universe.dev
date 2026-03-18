#'
#' @rdname fixDur
#' 
#' @name fixDur
#' 
#' @title Fixation Duration
#' 
#' @description Calculates the fixation durations for the passes (Hyönä, Lorch, 
#'   and Rinck, 2003; Hyönä, and Lorch, 2004) or the AOI's
#' 
#' @param data A data frame containing fixation information of an eye tracing 
#'   experiment and the coded passes. Each row indicates a fixation.
#' @param fixTime The name or number of the column containing the time per fixation.
#' @param passes The name or number of the column containing the coded passes.
#' @param AOI The name or number of the column in \code{data} containing the name
#'   of the area of interest (AOI) that was fixated.
#' 
#' @details This function is a wrapper for  \code{\link[stats]{aggregate}} 
#'   
#'   The function will only return the fixation duration of the values in the 
#'   \code{passes} column. The passes column is the vector returned by the 
#'   \code{link{codePasses}} function. It is also possible to provide the column 
#'   name of the column containing the AOI's if you require the fixation durations 
#'   for the AOI's only.
#'   The column of which the name or number is passed to \code{passes},will be 
#'   converted to a factor if it is not yet the case.
#'   
#'   You can provide the names of the AOI's to \code{AOI_label} and indicate if 
#'   rereading passes were coded in \code{rereading}. This ensures that the output 
#'   contains all types of passes for each AOI, even if they did not occur. In 
#'   that case the value in the output will be 0.
#' 
#' @return A data frame with the following columns  
#'  If the AOI column is provided to \code{passes}:  
#'   $AOI: containing the AOI names  
#'   $duration: containing the aggregated durations  
#'  
#'  If the passes column is provided to \code{passes} and there is no rereading:  
#'   $AOI: containing the AOI names  
#'   $FirstPass: containing the aggregated first pass durations  
#'   $SecondPass: containing the aggregated second pass durations  
#'   
#'  If the passes column is provided to \code{passes} and there is rereading:  
#'  $AOI: containing the AOI names  
#'   $FirstPassForward: containing the aggregated first pass forward durations  
#'   $FirstPassRereading: containing the aggregated first pass rereading durations  
#'   $SecondPass: containing the aggregated second pass durations  
#'   
#'   The result will be in the same unit as the duration input.
#'   
#'   If the data contains fixations that were outside the AOI, the first line of 
#'   the results will contain the total fixation duration outside the AOI's.
#' 
#' @examples
#'   data( SimData )
#'                          
#'   ### This function compiles fixation durations
#'   ## for first and second passes
#'   # when the column name is given
#'   fixDur( data = SimData, fixTime = "fixTime", 
#'           passes = "passes" )
#'           
#'   # and when column number is given
#'   fixDur( data = SimData, fixTime = 8, passes = 9 )
#'   
#'   ## for forward and rereading passes
#'   fixDur( data = SimData, fixTime = "fixTime",
#'           passes = "passesReread" )
#'           
#'   ## and for AOI's
#'   fixDur( data = SimData, fixTime = "fixTime", 
#'           passes = "AOI" )
#' 
#' @seealso \code{\link[stats]{aggregate}}, \code{\link[base]{by}}, 
#'   \code{\link[base]{tapply}}
#' 
#' @references
#'   Hyönä, J., Lorch, R. F., & Rinck, M. (2003). Eye movement measures to study 
#'   global text processing. In J. Hyönä, R. Radach, & H. Deubel (Eds.), \emph{The mind's 
#'   eye: cognitive and applied aspects of eye movement research} (pp. 313-334). 
#'   Amsterdam: Elsevier Science.  
#'     
#'   Hyönä, J., & Lorch, R. F. (2004). Effects of topic headings on text processing: 
#'   evidence from adult readers’ eye fixation patterns. \emph{Learning and Instruction, 
#'   14}, 131-152. doi:10.1016/j.learninstruc.2004.01.001
#'
#' @importFrom stats aggregate
#' @importFrom tidyr spread
#' @importFrom tibble is_tibble
#' @export fixDur
#' 

fixDur <- function( data, fixTime, passes, AOI = NULL )
{
  if( is_tibble( data ) )
  {
    data <- as.data.frame( data )
  }
  
  fixDur.inputChecks( data = data, fixTime = fixTime, passes = passes, AOI = AOI )
  
  if( !is.null( AOI ) )
  {
    AOI <- data[ , AOI ]
    
    if( is.factor( AOI ) )
    {
      AOI <- data.frame( AOI = levels( AOI ),
                       stringsAsFactors = F )
    } else AOI <- data.frame( AOI = unique( AOI ), stringsAsFactors = F )
  }
  
  if( any( data[ , passes] == 0 ) )
    data0 <- data[ data[ , passes] == 0, ]
  
  data <- data[ data[ , passes] != 0, ]
  
  if( !is.character( data[ , passes ] ) )
    data[ , passes ] <- as.character( data[ , passes ] ) #needed for odd behaviour of factors
  
  splitted_pass <- transpose( 
    as.data.frame( 
      strsplit( data[, passes], split = "_", fixed = T )
    )
  )
  
  if( ncol( splitted_pass ) < 2 )
  {
    result <- aggregate( list( duration = data[ , fixTime ] ),
                         by = list( AOI = splitted_pass[ , 1 ]),
                         FUN = sum )
    if( exists( "data0" ) )
    {
      result0 <- sum( data0[ , fixTime ] )
      result <- rbind( data.frame( AOI = 0, duration = result0 ),
                       result )
    }
  }else 
  {
    result <- aggregate( list( duration = data[ , fixTime ] ),
                       by = list( AOI = splitted_pass[ , 2 ],
                                  passes = splitted_pass[ , 1 ]),
                       FUN = sum )
    
    result <- spread( result, key = "passes", value = "duration", fill = 0, drop = F )
    
    if( exists( "data0" ) )
    {
      result0 <- sum( data0[ , fixTime ] )
      result <- rbind( data.frame( AOI = 0, FPF = result0, FPR = NA, SP = NA ),
                       result )
    }
    
    if( !is.null( AOI ) )
      result <- merge( result, AOI, by = "AOI", all = T )
    
    result[ is.na( result ) ] <- 0
    
    if( any( splitted_pass [ , 1 ] == "FPF" ) )
    {
      names( result )[ 2:4 ] <- c( "FirstPassForward", "FirstPassRereading", "SecondPass" )
    }else names( result )[ 2:3 ] <- c( "FirstPass", "SecondPass" )
  }
  
  return( as.data.frame( result ) )
  
}

fixDur.inputChecks <- function( data, fixTime, passes, AOI )
{
  if( !is.data.frame( data ) ) stop( "data should be a data frame" )
  
  if( length( fixTime ) > 1 ) stop( paste0("fixTime should be a single number or ",
                                          "character, indicating the fixation time ",
                                          "column" ) )
  
  if( is.character( fixTime ) & !( fixTime %in% colnames( data ) ) )
    stop( "fixTime is not a column of data" )
  
  if( length( passes ) > 1 ) stop( paste0("passes should be a single number or ",
                                          "character, indicating the coded passes ",
                                          "column" ) )
  
  if( is.character( passes ) & !( passes %in% colnames( data ) ) )
    stop( "passes is not a column of data" )
  
  if( !is.null( AOI ) & is.character( AOI ) & length( AOI ) == 1 )
  {
   if(  !( AOI %in% colnames( data ) ) )
        stop( "the value provide to AOI is not a column name of data" )
  }
  
}