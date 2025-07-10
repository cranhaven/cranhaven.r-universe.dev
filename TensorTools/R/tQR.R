#' QR decomposition of a 3D tensor
#' @description Decomposes a 3 mode tensor T into the product of The left singular value tensor object
#' and a right singular value tensor object so that T = QR.
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
#' @return Q, The left singular value tensor object (\eqn{n \times n \times k})
#' @return R, The right singular value tensor object (\eqn{n \times n \times k})
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' tQR(T,"dst")
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization of the matrix svd as a product of third-order tensors,” Tufts University, Department of Computer Science, Tech. Rep. TR-2008-4, 2008
#'
#' K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.


tQR <- function(tnsr,tform) {
  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("QR decomposition is defined only for square lateral faces")
  if (tform=="fft") {
    QR = tQRfft(tnsr)
  } else if (tform=="dwt") {
    QR = tQRdwt(tnsr)
  } else if (tform=="dct") {
    QR = tQRdct(tnsr)
  } else if(tform=="dst") {
    QR = tQRdst(tnsr)
  } else if(tform=="dwht") {
    QR = tQRdwht(tnsr)
  } else if(tform=="dht") {
    QR = tQRdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  return(list(Q = QR$Q, R = QR$R))
}
