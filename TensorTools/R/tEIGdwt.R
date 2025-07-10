#' Eigenvalue decomposition of 3-mode tensor using the discrete wavelet transform.
#' @param tnsr, a 3-mode S3 tensor class object (\eqn{n} x \eqn{n} x \eqn{k})
#' @return P, tensor of Eigenvectors (\eqn{n} x \eqn{n} x \eqn{k})
#' @return D, diagonal tensor of Eigenvalues (\eqn{n} x \eqn{n} x \eqn{k})
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' print(tEIGdwt(T))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tEIGdwt <- function (tnsr)
{
  # Performs a Eigenvalue decomposition of 3-mode tensor
  # using the discrete wavelet transform.

  # Input: tnsr, a 3D tensor
  # Output: A tensor P of eigenvectors and a tensor D
  # eigenvalues so that tnsr = P D P^-1

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (sum(as.numeric(intToBits(n3))) != 1)
    stop("Mode 3 must be a power of 2 otherwise using 0 padding")
  dwtz <- tDWT(tnsr)
  P_arr <- array(0, dim = c(n1, n2, n3))
  D_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- eigen(dwtz$data[, , j])
    decompp <- polar(decomp$vectors,decomp$values)
    P_arr[, , j] <- decompp$P
    D_arr[, , j] <- decompp$D
  }
  # Inverse DWT
  P <- tIDWT(as.Tensor(P_arr))
  D <- tIDWT(as.Tensor(D_arr))
  invisible(list(P = P,D = D))
}
