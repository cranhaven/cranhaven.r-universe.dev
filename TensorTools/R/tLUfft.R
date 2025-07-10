#' LU decomposition of a 3D tensor using the discrete fast fourier transform
#' @param tnsr, a 3-mode S3 tensor class object
#' @return L, The lower triangular S3 tensor object
#' @return U, The upper triangular S3 tensor object
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' tLUfft(T)
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization of the matrix svd as a product of third-order tensors,” Tufts University, Department of Computer Science, Tech. Rep. TR-2008-4, 2008
#'
#' K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tLUfft <- function (tnsr)
{
  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dftz <- aperm(apply(tnsr$data, MARGIN = 1:2, fft), c(2,3,1))
  L_arr <- array(0, dim = c(n1, n2, n3))
  U_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- LU(dftz[, , j])
    L_arr[, , j] <- decomp$L
    U_arr[, , j] <- decomp$U
  }
  L <- as.Tensor(aperm(apply(L_arr, MARGIN = 1:2, ifft), c(2,3,1)))
  U <- as.Tensor(aperm(apply(U_arr, MARGIN = 1:2, ifft), c(2,3,1)))
  invisible(list(L = L, U = U))
}
