#' Singular value decomposition (SVD)
#' @description Performs a Singular Value Decomposition of 3 mode tensor T using any discrete transform.
#' The result is a left singular value tensor object U, a right singular value tensor object V, and a
#' diagonal tensor S so that T = USV^t
#' @param tnsr, a 3-mode tensor S3 class object
#' @param tform, Any discrete transform.
#'
#' fft: Fast Fourier Transorm
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
#'
#' @return If the SVD is performed on a \eqn{m} x \eqn{n} x \eqn{k} tensor, the components in the returned value are:
#'
#' U, the left singular value tensor object (\eqn{m} x \eqn{m} x \eqn{k})
#'
#' V, The right singular value tensor object (\eqn{n} x \eqn{n} x \eqn{k})
#'
#' S: A diagonal tensor (\eqn{m} x \eqn{n} x \eqn{k})#' @examples
#'
#' @examples
#' T <- t_rand(modes=c(2,3,4))
#' print(tSVD(T,"dst"))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization of the matrix svd as a product of third-order tensors,” Tufts University, Department of Computer Science, Tech. Rep. TR-2008-4, 2008
#'
#' K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tSVD <- function(tnsr,tform)
{
  # Performs a tensor singular value decomposition on any 3-mode
  # tensor using any discrete transform.

  # Input: A, 3-mode tensor
  # Output: Tensors U (left singular value object),
  # V (right singular value object) and
  # S, a diagonal tensor so that A=USV^T.

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (tform=="fft") {
    SVD = tSVDfft(tnsr)
  } else if (tform=="dwt") {
    SVD = tSVDdwt(tnsr)
  } else if (tform=="dct") {
    SVD = tSVDdct(tnsr)
  } else if(tform=="dst") {
    SVD = tSVDdst(tnsr)
  } else if(tform=="dwht") {
    SVD = tSVDdwht(tnsr)
  } else if(tform=="dht") {
    SVD = tSVDdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  return(SVD)
}
