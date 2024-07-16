#' @title Read File Tiff
#' 
#' @description Reads a TIFF file and converts it into a 2D-array. If the file contains multiple pages, a 3D-array will be then returned.
#' @aliases readFileTiff
#' @usage readFileTiff(filename, invert = TRUE)
#' @param filename Either name of the file to read from or a raw vector representing the TIFF file content.
#' @param invert If set to TRUE then the order of the data will be reversed. Default TRUE.
#' @details Read a TIFF file image using readTIFF and converts it to a matrix with n-dimensions.
#' @note This function must be used in order to extract the information from the TIFF files needed to test the functions in this package. The TIFF file must be grayscale.
#' 
#' @export
#' @import tiff
#' @return A matrix containing the image data.
#' @author Adan O. Guerrero Cardenas.
#' 
#' @seealso \code{\link{readTIFF}} \code{\link{writeFileTiff}}
#' 
#' @examples 
#' \donttest{
#' raw <- readFileTiff(FileName)
#' }

readFileTiff<-function(filename, invert = TRUE){ # modified 2016-06-23
  imraw<-readTIFF(filename, as.is = TRUE, all = TRUE)
  nFrames<-length(imraw)
  X<-length(imraw[[1]][,1])
  Y<-length(imraw[[1]][1,]) 
  img = array(NA, dim=c( X, Y, nFrames))
  for(i in 1:nFrames){
    img[,,i] <-imraw[[i]]
  }
  if(invert)  {
    imgi = array(NA, dim=c( Y, X, nFrames))
    for(i in 1:nFrames){
      imgi[,,i] <-t(img[,,i])
      imgi[,X:1,i]<-imgi[,1:X,i]
    }
    return(imgi)
  }   else return(img)
}
