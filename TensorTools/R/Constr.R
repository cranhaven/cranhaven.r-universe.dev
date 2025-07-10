#' Creates an S3 class for a tensor
#' @param data Numeric numbers in the tensor
#' @param x, mode 1 dimension
#' @param y, mode 2 dimension
#' @param z, mode 3 dimension
#' @return S3 class tensor

Tensor <- function(data, x, y, z)
{
  structure(list(modes = c(x,y,z), data = array(data, c(x,y,z))), class = "TensorClass")
}
