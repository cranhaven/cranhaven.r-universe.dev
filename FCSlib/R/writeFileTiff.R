#' @title Write File Tiff
#' 
#' @description Create a TIFF file from a 3D-array.
#' @aliases writeFileTiff
#' @usage writeFileTiff(img,  file.name, invert = TRUE, bits.per.sample = NULL)
#' @param img Either an image or a list of images. An image is a real matrix or array of three dimensions.
#' @param file.name Either the name of the file or the name of a raw vector.
#' @param invert If set to TRUE then the order of the data will be reversed. Default TRUE.
#' @param bits.per.sample Number of bits per sample (numeric scalar). Supported values in this version are 8, 16, and 32.
#' @details Create a TIFF file using writeTIFF, converting a 2D-array. If the file contains multiple pages, a 3D-array is turned into a 2D-array to implement the aforementioned function.
#' @export
#' @import tiff
#' @return None.
#' @references None
#' @author Adan O. Guerrero Cardenas.
#' 
#' @seealso \code{\link{writeTIFF}} \code{\link{readFileTiff}}
#' @examples
#' imagsave <- array(data = 1:10, dim = c(100,100,10))
#' writeFileTiff(imagsave, paste(tempdir(), "/image_Test.tif", sep = ""))


writeFileTiff<-function(img,  file.name, invert = TRUE, bits.per.sample = NULL){ # modified 2016-08-24
  di<-dim(img)
  X<-di[1]
  Y<-di[2]
  nFrames<-di[3]
  imraw<-vector("list",nFrames)
  m<-max(img, na.rm=TRUE)
  bps<-c(8,16,32)
  m.id<-NULL
  if(is.null(bits.per.sample)){
    if(m/2^bps[1]<1){m.id <-1
    } else if (m/2^bps[2]<1) {m.id <-2
    } else if (m/2^bps[3]<1) m.id <-3    
    bits.per.sample<-bps[m.id]
  }
  img<-img/2^bits.per.sample
  if(invert)  {
    imgi = array(NA, dim=c( Y, X, nFrames))
    for(i in 1:nFrames){
      imgi[,,i] <-t(img[,,i])
      imgi[Y:1, ,i]<-imgi[1:Y,,i]
      imraw[[i]]<-imgi[,,i]
    }
  } else {
    for(i in 1:nFrames){
      imraw[[i]]<-img[,,i]
    }
  }
  try(writeTIFF(imraw, file.name, bits.per.sample, compression = "none", reduce = FALSE))
}