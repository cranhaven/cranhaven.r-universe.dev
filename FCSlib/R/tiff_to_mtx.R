#' @title Transformation of multiple-image TIFF files or arrays into a matrix
#' 
#' @description Transforms multiple-image TIFF files or 3D arrays into 2D matrices with a user-specified number of columns
#' @aliases tiff_to_mtx
#' @usage tiff_to_mtx(data, columns)
#' @param data A character string indicating the name of a TIFF file or a 3D array
#' @param columns The number of columns of the resulting matrix
#' @details Creates a matrix with a user-specified number of columns and a number of rows equal to the total amount of points in 'data' divided by 'columns'.
#' 
#' @export
#' @return A matrix
#' @author Alejandro Linares
#' 
#' @seealso \code{\link{binMatrix}}
#' 
#' @examples
#' \donttest{
#' ### Please navigate to
#' ### (https://github.com/FCSlib/FCSlib/tree/master/Sample%20Data)
#' ### to find this sample data
#' 
#' x <- readFileTiff("Example_file_name.tif")
#' class(x); dim(x)
#' 
#' x.m <- tiff_to_mtx(data = x, columns = 64)
#' class(x.m); dim(x.m)
#' }

tiff_to_mtx <- function(data, columns){
  if (is.character(data)){
    arr <- readFileTiff(data)
  } else arr <- data
  
  if (is.array(arr) == FALSE || length(dim(arr)) < 3 || dim(arr)[3] == 1){
    stop("Data must be a TIFF image series or a 3-dimensional array")
  }
  
  mat <- matrix(0, ncol = (floor(length(arr)/columns)), nrow = columns)
  for (i in 1:length(mat)){
    mat[i] <- arr[i]
  }
  return(mat)
}
