#' Eigenvalue decomposition of 3-mode tensor using the discrete fast fourier transform.
#' @param tnsr, a 3-mode S3 tensor class object (\eqn{n} x \eqn{n} x \eqn{k})
#' @return P, tensor of Eigenvectors (\eqn{n} x \eqn{n} x \eqn{k})
#' @return D, diagonal tensor of Eigenvalues (\eqn{n} x \eqn{n} x \eqn{k})
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' print(tEIGfft(T))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tEIGfft <- function (tnsr)
{
  # Performs a Eigenvalue decomposition of 3-mode tensor
  # using the discrete fast fourier transform.

  # Input: tnsr, a 3D tensor
  # Output: A tensor P of eigenvectors and a tensor D
  # eigenvalues so that tnsr = P D P^-1

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dfftz <- aperm(apply(tnsr$data, MARGIN = 1:2, fft), c(2,3,1))
  P_arr <- array(0, dim = c(n1, n2, n3))
  D_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- eigen(dfftz[, , j])
    P_arr[, , j] <- decomp$vectors
    D_arr[, , j] <- diag(decomp$values)
  }
  P <- as.Tensor(aperm(apply(P_arr, MARGIN = 1:2, ifft), c(2,3,1)))
  D <- as.Tensor(aperm(apply(D_arr, MARGIN = 1:2, ifft), c(2,3,1)))
  invisible(list(P = P, D = D))
}
