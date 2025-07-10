#' LU decomposition of a 3D tensor
#' @description Decomposes a 3 model tensor into a lower triangular tensor and an upper triangular tensor.
#' @param tnsr, a 3-mode tensor S3 class object
#' @param tform, Any discrete transform.
#'
#' fft: Fast Fourier Transform
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
#' @return L, The lower triangular tensor object
#' @return U, The upper triangular tensor object
#' a \href{/library/rTensor2/help/Tensor3-class}{Tensor3-class} object
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' tLU(T,"dst")
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization of the matrix svd as a product of third-order tensors,” Tufts University, Department of Computer Science, Tech. Rep. TR-2008-4, 2008
#'
#' K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tLU <- function(tnsr,tform)
{
  # Performs a tensor LU decomposition on any 3-mode tensor
  # using any discrete transform.

  # Input: A square matrix A.
  # Output: Matrices L and U so that A=LU.

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("LU decomposition only works on tensors with square lateral faces")
  if (tform=="fft") {
    LU = tLUfft(tnsr)
  } else if (tform=="dwt") {
    LU = tLUdwt(tnsr)
  } else if (tform=="dct") {
    LU = tLUdct(tnsr)
  } else if(tform=="dst") {
    LU = tLUdst(tnsr)
  } else if(tform=="dwht") {
    LU = tLUdwht(tnsr)
  } else if(tform=="dht") {
    LU = tLUdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  return(list(L = LU$L, U = LU$U))
}
