#' Gets RasterStack's longitude information
#' 
#' This function constructs the input for one of the four dimensions 
#' required by \code{\link[gapfill]{Gapfill}}, namely, \emph{longitude}. 
#' 
#' @param stack RasterStack 
#' 
#' @export
#' 
#' @importFrom raster xmin
#' @importFrom raster xmax
#' @importFrom raster ncol 
#' 
#' @note This function may be useful when employing \code{\link[gapfill]{Gapfill}}
#' independently of the current package.
#' 
#' @seealso \code{\link[igapfill]{get_3Darray}}, \code{\link[igapfill]{get_LAT}}
#' 
#' @return Character vector of length equal to \code{ncol(stack)}.
#' 
get_LON <- function(stack){
  as.character( seq( xmin(stack), xmax(stack), length.out = ncol(stack) ) )
}
