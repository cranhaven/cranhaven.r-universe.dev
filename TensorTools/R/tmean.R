#' Determines the mean of a 3D tensor along mode 2
#' @param tnsr, a 3D tensor of dimensions n1,n2,n3
#' @return S3 tensor class object
#' @examples
#' tnsr <- t_rand(modes=c(3,4,5))
#' tmean(tnsr)
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization of the matrix svd as a product of third-order tensors,” Tufts University, Department of Computer Science, Tech. Rep. TR-2008-4, 2008
#'
#' K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.


tmean <- function(tnsr) {

  # Determines the mean of a 3D tensor
  # along mode 2

  # Input: a 3D tensor of dimensions n1,n2,n3
  # Output: a 3D tensor of dimensions n1,1,n3

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]

  # Initialize mean tensor
  mu_arr <- array(0, dim = c(n1, 1, n3))
  mu <- as.Tensor(mu_arr)

  # Determine the mean of all samples in a tensor.
  for (i in 1:n2) {
    mu$data[,1,] = mu$data[,1,] + tnsr$data[,i,]
  }
  mu$data = mu$data/n2

  return(mu)
}
