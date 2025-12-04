#' Assembles a 3D-array
#' 
#' This function returns a 3D-array which is an auxiliary
#' object when invoking \code{\link[igapfill]{applyGapfill}}.
#' 
#' @param path character with full path name of a directory containing files that
#'             can be read as RasterStacks
#'             
#' @export             
#'             
#' @importFrom raster stack
#' @importFrom raster nrow
#' @importFrom raster ncol
#' @importFrom raster nlayers
#' @importFrom raster getValues
#' @importFrom raster subset
#' 
#' @note This function may be useful when employing \code{\link[gapfill]{Gapfill}}
#' independently of the current package.
#'             
#' @seealso \code{\link[base]{array}}, \code{\link[gapfill]{Gapfill}}
#' 
#' @return An array with three dimensions             
#'             
get_3Darray <- function(path){
  STACK <- stack(path)
  ARRAY <- array(NA, dim = c(nrow(STACK), ncol(STACK), nlayers(STACK)))
  for( i in 1:nlayers(STACK) ){
    TEMP <- getValues(subset(STACK, i))
    ARRAY[,,i] <- TEMP
  }
  ARRAY
}
