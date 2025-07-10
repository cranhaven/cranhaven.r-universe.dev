#' QR decomposition of a 3D tensor using the fast fourier transform
#' @param tnsr, a 3-mode S3 tensor class object
#' @return Q, The left singular value S3 tensor class object (\eqn{n \times n \times k})
#' @return R, The right singular value Se tensor class object (\eqn{n \times n \times k})
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' tQRfft(T)
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization of the matrix svd as a product of third-order tensors,” Tufts University, Department of Computer Science, Tech. Rep. TR-2008-4, 2008
#'
#' K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.


tQRfft <- function (tnsr)
{
  # Performs a tensor QR decomposition on any 3-mode tensor
  # using the discrete fast fourier transform.

  # Input: A, 3-mode tensor
  # Output: Tensors Q and R so that A=QR.

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dftz <- aperm(apply(tnsr$data, MARGIN = 1:2, fft), c(2,3,1))
  Q_arr <- array(0, dim = c(n1, n2, n3))
  R_arr <- array(0, dim = c(n2, n2, n3))
  for (j in 1:n3) {
    decomp <- QR(dftz[, , j])
    Q_arr[, , j] <- decomp$Q
    R_arr[, , j] <- decomp$R
  }
  Q <- as.Tensor(aperm(apply(Q_arr, MARGIN = 1:2, ifft), c(2,3,1)))
  R <- as.Tensor(aperm(apply(R_arr, MARGIN = 1:2, ifft), c(2,3,1)))
  invisible(list(Q = Q, R = R))
}
