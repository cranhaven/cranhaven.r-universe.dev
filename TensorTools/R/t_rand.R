#' Create a random tensor
#' @description Generate a Tensor with specified modes whose entries are iid normal(0,1).
#' @param modes, the 3 modes of the output Tensor
#' @return an S3 Tensor object
#' @examples
#'  t_rand(c(4,4,4))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references Imported from rTensor2 package version 2.0.0.
t_rand <- function (modes = c(3, 4, 5))
{
  as.Tensor(array(rnorm(prod(modes)), dim = modes))
}
