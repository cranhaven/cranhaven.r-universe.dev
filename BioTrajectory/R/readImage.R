#' Read and Resize an Image
#'
#' This function reads an image from a file path and resizes it to a specified number of rows. 
#' It supports several image formats, including JPG, JPEG, PNG, TIFF, and TIF. The function
#' also converts the image into a suitable format for further processing.
#'
#' @param path The file path of the image to be read.
#' @param resizeRows The desired number of rows (height) to resize the image. 
#'                   The aspect ratio of the image will be maintained during resizing.
#'
#' @return A cimg object containing the resized image.
#' 
#' @details
#' The function detects the image format based on the file extension. It currently supports 
#' the following formats: JPG, JPEG, PNG, TIFF, and TIF. If the image has more than 3 
#' dimensions (such as an RGBA image with an alpha channel), the alpha channel is discarded.
#' The image is resized only if its height exceeds the specified `resizeRows`.
#'
#' @examples
#' # Example usage
#' img_path <- system.file('extdata/data.tiff', package='BioTrajectory')
#' img <- readImage(img_path, resizeRows = 500)
#' plot(img)  # Visualizes the resized image
#' 
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
#' @importFrom tiff readTIFF
#' @export

readImage <- function(path, resizeRows) {
  extension <- tolower(tools::file_ext(path))  # Get the file extension and convert it to lowercase
  image <- switch(extension,
                   "jpg" = as.cimg(readJPEG(path)),    # Read JPG files
                   "jpeg" = as.cimg(readJPEG(path)),  # Read JPEG files
                   "png" = as.cimg(readPNG(path)),    # Read PNG files
                   "tiff" = as.cimg(readTIFF(path)),  # Read TIFF files
                   "tif" = as.cimg(readTIFF(path)),   # Read TIF files
                   stop("Unsupported image format. Use PNG, JPG, TIFF.")  # Error message for unsupported formats
  )
  
  if (length(dim(image)) == 4) 
    image <- image[,,,1]
  
  if (length(dim(image)) == 3) 
    image <- image[,,1]
  
  image <- as.cimg(t(image))
  
  if (nrow(image) > resizeRows) {
    image <- resize(image, resizeRows, (ncol(image) * (resizeRows / nrow(image))))
  }
  
  return(image)
}
