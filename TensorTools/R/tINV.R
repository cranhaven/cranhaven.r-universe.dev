#' Performs inverse of 3-mode tensor using any discrete transform.
#' @param tnsr, a 3-mode tensor S3 class object
#' @param tform, Any discrete transform.
#' fft: Fast Fourier Transorm
#' dwt: Discrete Wavelet Transform (Haar Wavelet)
#' dct: Discrete Cosine transform
#' dst: Discrete Sine transform
#' dht: Discrete Hadley transform
#' dwht: Discrete Walsh-Hadamard transform
#' @return S3 class tensor
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' print(tINV(T,"dst"))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo

tINV <- function(tnsr,tform)
{
  # Performs inverse of 3-mode tensor using any
  # discrete transform.

  # Input: tnsr, a 3D tensor
  # Output: The inverse of tnsr, a 3D tensor

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (n1 !=n2)
    stop("The inverse is only defined for square lateral faces")

  if (tform=="fft") {
    TI = tINVfft(tnsr)
  } else if (tform=="dwt") {
    TI = tINVdwt(tnsr)
  } else if (tform=="dct") {
    TI = tINVdct(tnsr)
  } else if(tform=="dst") {
    TI = tINVdst(tnsr)
  } else if(tform=="dwht") {
    TI = tINVdwht(tnsr)
  } else if(tform=="dht") {
    TI = tINVdht(tnsr)
  } else {
    stop("Transform not supported")
  }
  Tinv <- TI
  return(Tinv)
}
