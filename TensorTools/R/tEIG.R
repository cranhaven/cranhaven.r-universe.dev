#' Tensor Eigenvalue Decomposition Using any Discrete Transform
#' @description The Eigenvalue decomposition of a tensor T (\eqn{n} x \eqn{n} x \eqn{k}) decomposes the tensor into
#' a tensor of eigenvectors (P) and a diagonal tensor of eigenvalues (D) so that
#' T = P D inv(P).
#' @param tnsr, a 3-mode S3 tensor class object (\eqn{n} x \eqn{n} x \eqn{k})
#' @param tform, Any discrete transform.
#'
#'fft: Fast Fourier Transorm
#'
#' dwt: Discrete Wavelet Transform (Haar Wavelet)
#'
#' dct: Discrete Cosine transform
#'
#' dst: Discrete Sine transform
#'
#' dht: Discrete Hadley transform
#'
#' dwht: Discrete Walsh-Hadamard transform
#' @return
#' P, a tensor of Eigenvectors (\eqn{n} x \eqn{n} x \eqn{k})
#'
#' D, a diagonal tensor of Eigenvalues (\eqn{n} x \eqn{n} x \eqn{k})
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' tEIG(T,"dst")
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tEIG <- function(tnsr,tform) {
  # Performs a Eigenvalue decomposition of 3-mode tensor
  # using any discrete transform.

  # Input: tnsr, a 3D tensor
  # Output: A tensor P of eigenvectors and a tensor D
  # eigenvalues so that tnsr = P D P^-1

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("An eigen value decomposition can only be performed when the lateral faces are square")

  if (tform=="fft") {
    TE = tEIGfft(tnsr)
  } else if (tform=="dwt") {
    TE = tEIGdwt(tnsr)
  } else if (tform=="dct") {
    TE = tEIGdct(tnsr)
  } else if(tform=="dst") {
    TE = tEIGdst(tnsr)
  } else if(tform=="dwht") {
    TE = tEIGdwht(tnsr)
  } else if(tform=="dht") {
    TE = tEIGdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  tEIG <- TE
  return(tEIG)
}
