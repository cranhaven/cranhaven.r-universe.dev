#'
#' @rdname codePasses
#' 
#' @name codePasses
#' 
#' @title Codes the Fixations as First Pass and Second Pass
#' 
#' @description The fixations are coded as first pass and second pass with or without rereading 
#'   as discussed in Hyönä, Lorch, and Rinck (2003) and Hyönä, and Lorch (2004).
#' 
#' @param data A data frame containing fixation information of an eye tracing 
#'   experiment. Each row indicates a fixation.
#' @param AOI The name or number of the column in \code{data} containing the name
#'   of the area of interest (AOI) that was fixated. If \code{data} contains one 
#'   column per AOI, also a vector is accepted (see Details).
#' @param rereading Logical. Indicating if the first pass fixations should be 
#'   split according to forward and rereading (\code{TRUE}) or not 
#'   (\code{FALSE} [Default])
#' @param fpx The name or number of the column containing the x coordinate of the 
#'   fixation point. Required if \code{Rereading} is \code{TRUE}.
#' @param fpy The name or number of the column containing the y coordinate of the 
#'   fixation point. Required if \code{Rereading} is \code{TRUE}.
#' @param origin Character string specifying where the origin of the fixation 
#'   coordinates \code{fpx} and \code{fpy} is located. The following values are
#'   possible: "topLeft" (default), "bottomLeft", "center", "topRight", "bottomRight".
#' @param fix_size The size or acuity of the saccade. (default = 42; see Details)
#' @param fix_min [optional] minimal number of fixations for first pass. (default = 3; 
#'   see Details)
#' 
#' @details This function takes a data frame containing information of an eye 
#'   tracking reading exercise Each row indicates a fixation and the columns at 
#'   least indicate the AOI that was fixated or if a specific AOI was fixated or not. 
#'   Optionally it can indicate the x and y coordinates of the fixation point.  
#'   The fixations in this dataset are then coded according to their pass status: 
#'   first pass and second pass.
#'   
#'   If \code{AOI} is a single value it indicates the name or the number of the 
#'   column with the name of the respective area of interest (AOI) that was fixated. 
#'   In this case, the AOI column consists of names or numbers identifying the AOI's. 
#'   If \code{AOI} is a vector it indicates the name or number of the AOI columns. 
#'   In this case the AOI columns indicate if the respective fixation was in the 
#'   corresponding AOI (1) or not (0).  
#'   NOTE: if the names of the AOI columns passed to the function are just numbers, 
#'   make sure to pass these as character for the function to work properly. The 
#'   function does not check for this case.
#'   
#'   First pass fixations are further divided into forward and rereading 
#'   fixations if \code{rereading} is set to \code{TRUE}. In this case the names 
#'   of the columns containing the x and y coordinates of the fixation point 
#'   should be supplied by \code{fpx} and \code{fpy} respectively. The unit of 
#'   these coordinates does not matter as long as it is the same for both and for the 
#'   value of \code{fix_size}.
#'   
#'   It is important to set the minimal distance between fixations (or the visual 
#'   acuity) via \code{fix_size}. This value is used to determine if two 
#'   fixations are on the same line and/or on the same position in the line. 
#'   When this value is to small it is possible that some first-pass fixations 
#'   are falsely categorized as rereading fixations. Specifically \code{fix_size} 
#'   determines what the minimal distance between fixations should be in order 
#'   for fixations to be considered in a different  position on the line or on 
#'   a different line. The default value is specified in pixels(px). The value is 
#'   the number of pixels equivalent to 2 visual degrees, taken a screen of 
#'   1020px and about 54cm in width and a viewing distance of 60cm. 
#'   (see \code{\link{px2deg}} for conversions). The value of 2 visual degrees 
#'   is the average visual angle of the fovea (Llewellyn-Thomas, 1968; Haber & 
#'   Hershenson, 1973). It is recommended to play around with the \code{fix_size} 
#'   value for every participant.
#'   
#'   By default this function considers the first three (3) fixations in any AOI 
#'   as first pass fixations. And it does this regardless of whether the fixations
#'   are consecutive or interrupted by fixations in a different AOI. The minimal 
#'   number of fixations considered as first pass can be changed through 
#'   \code{fix_min}.
#' 
#' @return The function returns a character vector of the same length as the 
#'   number of rows in data. Depending on the respective settings it contains 
#'   the following values with their respective meanings.  
#'   
#'   \itemize{
#'     \item \code{rereading} is \code{FALSE}:
#'     \itemize{
#'       \item \code{FP_\#} First Pass
#'       \item \code{SP_\#} Second Pass
#'      }
#'     \item \code{rereading} is \code{TRUE}
#'     \itemize{
#'       \item \code{FPF_\#} First Pass Forward
#'       \item \code{FPR_\#} First Pass Rereading
#'       \item \code{SP_\#} Second Pass
#'      }
#'   }
#'   
#'   Where \# stands for the name of the respecitve AOI.  
#' 
#' @examples
#'   data( "SimData" )
#'   
#'   ### codePasses calculates first and second  passes
#'   ## if a single AOI column is provided
#'   # by name
#'   codePasses( data = SimData, AOI = "AOI" )
#'   
#'   # by column number
#'   codePasses( data = SimData, AOI = 5 )
#'   
#'   ## and if multiple AOI columns are provided
#'   # by name
#'   resultA <- codePasses( data = SimData, 
#'                          AOI = c( "AOI1", "AOI2", "AOI3" ) )
#'   resultA
#'   
#'   # by number
#'   codePasses( data = SimData, AOI = 2:4 )
#'   
#'   ## \code{fix_min} influences how many fixations are needed in an AOI  
#'   ## independent of any fixations in between.  
#'   resultB <- codePasses( data = SimData, 
#'                          AOI = c( "AOI1", "AOI2", "AOI3" ), fix_min = 1 )
#'                          
#'   data.frame( fix_min3 = resultA, fix_min1 = resultB )
#'   
#'   rm( resultA, resultB )
#'   
#'   ### it also calculates forward and backward first passes if the x and y
#'   ### coordinates of the fixations are provided and \code{rereading} is \code{TRUE}
#'   resultA <- codePasses( data = SimData, AOI = "AOI",
#'                          rereading = TRUE, fpx = "xcoord", fpy = "ycoord",
#'                          fix_size = 20 )
#'   resultA
#'   
#'   # and allows for different coordinate origins
#'   resultB <- codePasses( data = SimData, AOI = "AOI", rereading = TRUE,
#'                          fpx = "xcoord", fpy = "ycoord", origin = "bottomLeft",
#'                          fix_size = 20 )
#'    data.frame( topLeft = resultA, bottomLeft = resultB )
#'   
#'   ## mind that fix_size can influence the results
#'   resultB <- codePasses( data = SimData, AOI = "AOI",
#'                          rereading = TRUE, fpx = "xcoord", fpy = "ycoord",
#'                          fix_size = 10 )
#'   
#'   data.frame( fix_size20 = resultA, fix_size10 = resultB )
#'   
#' @references
#'   Haber, R. N., & Hershenson, M. (1973) \emph{The psychology of visual perception.} 
#'   New York: Holt, Rinehart, and Winston.
#'     
#'   Llewellyn-Thomas, E. (1968) Movements of the eye. \emph{Scientific American, 219}(2), 
#'   88-95.  
#'     
#'   Hyönä, J., Lorch, R. F., & Rinck, M. (2003). Eye movement measures to study 
#'   global text processing. In J. Hyönä, R. Radach, & H. Deubel (Eds.), \emph{The mind's 
#'   eye: cognitive and applied aspects of eye movement research} (pp. 313-334). 
#'   Amsterdam: Elsevier Science.  
#'     
#'   Hyönä, J., & Lorch, R. F. (2004). Effects of topic headings on text processing: 
#'   evidence from adult readers’ eye fixation patterns. \emph{Learning and Instruction, 
#'   14}, 131-152. doi:10.1016/j.learninstruc.2004.01.001
#' 
#' @importFrom tibble is_tibble
#' @importFrom data.table transpose
#' 
#' @export codePasses
#' 

codePasses <- function( data, AOI, rereading = FALSE, fpx = NULL, fpy = NULL,
                        origin = c( "topLeft", "bottomLeft", "center", "topRight",
                                    "bottomRight" ), fix_size = 42, fix_min = 3 )
{
  if( is_tibble( data ) )
  {
    data <- as.data.frame( data )
  }
  
  
  inputCheck_codePasses( data = data, AOI = AOI, rereading = rereading,
                         fpx = fpx, fpy = fpy, fix_min = fix_min )
  
  if( length( AOI ) > 1 )
  {
    data <- data.frame( data, AOI = compileAOI( data = data, AOI = AOI ),
                        stringsAsFactors = F )
    AOI <- "AOI"
  } else if( is.factor( data[ , AOI ] ) )
  {
    data[ , AOI ] <- as.character( data[ , AOI ] )
  }
  
  origin <- match.arg( origin )
  
  if( origin == "bottomLeft" )
  {
    data[ , fpy ] <- abs( data[ , fpy ] - max( data[ , fpy ] ) )
  }else if( origin == "center" )
  {
    data[ , fpx ] <- data[ , fpx ] - min( data[ , fpx ] )
    data[ , fpy ] <- abs( data[ , fpy ] - max( data[ , fpy ] ) )
  }else if( origin == "topRight" )
  {
    data[ , fpx ] <- abs( data[ , fpx ] - max( data[ , fpx ] ) )
  }else if( origin == "bottomRight" )
  {
    data[ , fpx ] <- abs( data[ , fpx ] - max( data[ , fpx ] ) )
    data[ , fpy ] <- abs( data[ , fpy ] - max( data[ , fpy ] ) )
  }
  
  passes <- character( nrow( data ) )
  
  lastPass <- rep( "FP", times = length( unique( data[ , AOI ] ) ) )
  names( lastPass ) <- unique( data[ , AOI ] )
  
  fixCount <- rep( 0, times = length( unique( data[ , AOI ] ) ) )
  names( fixCount ) <- unique( data[ , AOI ] )
  fixCount[ data[ 1, AOI ] ] <-  fixCount[ data[ 1, AOI ] ] + 1
  
  if( rereading )
  {
    passes[1] <-  paste0( "FPF_", data[ 1, AOI ] )
    
    firstPass <- F
    
    prevCoords <- data.frame( x = numeric( length( unique( data[ , AOI ] ) ) ),
                              y = numeric( length( unique( data[ , AOI ] ) ) ),
                              row.names = unique( data[ , AOI ] ) )
    
    AOIrow <- which( row.names( prevCoords ) == data[ 1, AOI ] )
    
    prevCoords$x[ AOIrow ] <- data[ 1, fpx ]
    prevCoords$y[ AOIrow ] <- data[ 1, fpy ]
    
    rm( AOIrow )
  } else  passes[1] <- paste0( "FP_", data[ 1, AOI ] )
  
  for( i in 2:length( passes ) )
  { 
    if( lastPass[ data[ i, AOI ] ] == "SP" )
    {
      passes[i] <- paste0( "SP_", data[ i, AOI ]  ) 
    } else if( data[ i, AOI ] == data[ i - 1, AOI ] )
    {
      passes[i] <- paste0( "FP_", data[ i, AOI ] )
      
      firstPass <- T
    } else if( fixCount[ data[ i, AOI ] ] < fix_min )
    {
      passes[i] <- paste0( "FP_", data[ i, AOI ] )
      
      firstPass <- T
    } else
    {
      passes[i] <- paste0( "SP_", data[ i, AOI ] )
      
      lastPass[ data[ i, AOI ] ] <- "SP"
    }
    
    if( rereading )
    {
      AOIrow <- which( row.names( prevCoords ) == data[ i, AOI ] )
      
      if( firstPass )
      {
        if( abs( prevCoords$y[ AOIrow ] - data[ i, fpy ] ) <= fix_size  )
        {
          if( abs( prevCoords$x[ AOIrow ] - data[ i, fpx ] ) <= fix_size )
          {
            passes[i] <- paste0( "FPF_", data[ i, AOI ] )
          }else if( prevCoords$x[ AOIrow] < data[ i, fpx ] )
          {
            passes[i] <- paste0( "FPF_", data[ i, AOI ] )
            prevCoords$x[ AOIrow ] <- data[ i, fpx ]
            
          } else if( prevCoords$x[ AOIrow ] > data[ i, fpx ] )
          {
            passes[i] <- paste0( "FPR_", data[ i, AOI ] )
          }
        }else if( prevCoords$y[ AOIrow ] < data[ i, fpy ] )
        {
          passes[i] <- paste0( "FPF_", data[ i, AOI ] )
          prevCoords$y[ AOIrow ] <- data[ i, fpy ]
          
          if( prevCoords$x[ AOIrow ] > data[ i, fpx ] )
          {
            prevCoords$x[ AOIrow ] <- data[ i, fpx ]
          }
        } else if( prevCoords$y[ AOIrow ] > data[ i, fpy ] )
        {
          passes[i] <- paste0( "FPR_", data[ i, AOI ] )
          
        }
        
        if( prevCoords$x[ AOIrow ] < data[ i, fpx ] )
        {
          prevCoords$x[ AOIrow ] <- data[ i, fpx ]
        }
        
        firstPass <- F
        rm( AOIrow )
      }
    }
    
    fixCount[ data[ i, AOI ] ] <- fixCount[ data[ i, AOI ] ] + 1
  }
  rm(i)
  
  splitted_pass <- transpose( 
    as.data.frame( 
      strsplit( passes, split = "_", fixed = T )
      )
  )
  
  passes[ splitted_pass[ , 2 ] == 0 ] <- 0
  
  return( passes )
  
}

inputCheck_codePasses <- function(data, AOI, rereading, fpx, fpy, fix_min)
{
  if( !is.data.frame( data ) ) stop( "data should be a data frame" )
  
  if( length( AOI ) > 1 )
  {
    if( !is.numeric( AOI ) & !all( AOI %in% colnames( data ) ) )
    stop( "not all values provided to AOI are column names for data" )
  } else if( !is.numeric( AOI ) & !( AOI %in% colnames( data ) ) )
    stop( "the value provide to AOI is not a column name of data" )
  
  if( fix_min <= 0 ) stop( "Fix min should be bigger than 0" )
  
  if( rereading )
  {
    if( is.null( fpx ) )
    {
      stop( "If rereading is TRUE, then fpx should be provided" )
    } else if( is.null( fpy ) )
    {
      stop( "If rereading is TRUE, then fpy should be provided" )
    } else if ( is.character (fpx ) & !( fpx %in% colnames( data ) ) )
    {
       stop( "The value provided to fpx is not a column name of data" )
    } else if ( is.character (fpy ) & !( fpy %in% colnames( data ) ) )
    {
      stop( "The value provided to fpy is not a column name of data" )
    } else if( !is.numeric( data[ , fpx ] ) )
    {
      stop( "the column specified in fpx is not of type numeric" )
    } else if( !is.numeric( data[ , fpy ] ) )
    {
      stop( "the column specified in fpy is not of type numeric" )
    }
  }
}