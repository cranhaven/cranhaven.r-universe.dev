#' @title Smooth Carpet (Single Image)
#' 
#' @description Generates a smooth carpet.
#' @aliases smoothCarpet
#' @usage smoothCarpet(img, dfV = 0, dfH = 0)
#' @param img The image to analyze.
#' @param dfV The desired equivalent number of degrees of freedom in the vertical axis.
#' @param dfH The desired equivalent number of degrees of freedom in the horizontal axis.
#' @details The smoothCarpet function makes use of the smooth.spline method to smooth the vertical and horizontal axes of an image.
#' The magnitude of the smoothing depends on the degrees of freedom set for and vertical ('dfV') and horizontal ('dfH') axes of the image.
#'
#' @export
#' @importFrom stats smooth.spline
#' @return Smooth Carpet   A smooth image.
#' @author Raúl Pinto Cámara.
#' 
#' @seealso \code{\link{pcomb}, \link{smooth.spline}}
#' 
#' @examples 
#' \donttest{
#' ### Load the FCSlib package
#' 
#' library(FCSlib)
#' 
#' ### As an example, we will use a data set that corresponds to a population of Venus dimers
#' # diffusing in HEK-293 cells. Use the readFileTiff() function to extract the information
#' # from the '.tiff' files.
#' 
#' v2 <- data.matrix(V2)
#' nbv2 <- nbline(img = v2, S=3.5, sigma0 = 1,offset = 0, wSigma = 100);
#' sC <- smoothCarpet(img = nbv2$number, dfV = 5, dfH = 5)
#' }


smoothCarpet <- function(img, dfV = 0, dfH = 0){
  di <- dim(img);
  result <- smoothV <- smoothH <- img
  if(dfV != 0){
    smoothV <- array(data = NA, dim = di)
    for(i in 1:di[1]){
      fit <- smooth.spline((1:di[2]),img[i,], df = dfV)
      smoothV[i,] <- fit$y
    }
    result <- smoothV
  }
  if(dfH != 0){
    smoothH <- array(data = NA, dim = di)
    for(i in 1:di[2]){
      fit<-smooth.spline((1:di[1]),smoothV[,i], df = dfH)
      smoothH[,i] <- fit$y
    }
    result <- smoothH
  }
  return(result)
}