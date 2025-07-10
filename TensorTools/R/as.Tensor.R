#' Converts an array to an S3 tensor
#' @description This will converts array to S3 object tensor.  Vectors and matrices must first be converted to an array before applying as.Tensor.
#' @param t Numeric, array of numbers
#' @return S3 class tensor
#' @examples
#' indices <- c(2,3,4)
#' arr <- array(runif(prod(indices)), dim = indices)
#' arrT <- as.Tensor(arr); arrT
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo

as.Tensor <- function (t)
{
  stopifnot(is.array(t))
  stopifnot(length(dim(t)) == 3)
  structure(list(modes = dim(t), data = t), class = "Tensor3")
}
