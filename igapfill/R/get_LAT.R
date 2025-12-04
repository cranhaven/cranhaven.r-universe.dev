#' Gets RasterStack's latitude information
#' 
#' This function constructs the input for one of the four dimensions 
#' required by \code{\link[gapfill]{Gapfill}}, namely, \emph{latitude}. 
#' 
#' @param stack RasterStack 
#' 
#' @export
#' 
#' @importFrom raster ymin
#' @importFrom raster ymax
#' @importFrom raster nrow 
#' 
#' @note This function may be useful when employing \code{\link[gapfill]{Gapfill}}
#' independently of the current package.
#' 
#' @seealso \code{\link[igapfill]{get_3Darray}}, \code{\link[igapfill]{get_LON}}
#' 
#' @return Character vector of length equal to \code{nrow(stack)}.
#' 
get_LAT <- function(stack){
  as.character( seq( ymin(stack), ymax(stack), length.out = nrow(stack) ) )
}
