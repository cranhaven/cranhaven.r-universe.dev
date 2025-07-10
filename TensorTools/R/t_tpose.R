#' Tensor transpose
#' @description Performs the transpose of a symmetric 3-mode tensor using any discrete transform.
#' @param tnsr, a 3-mode tensor
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
#' @return S3 class tensor
#' @examples
#' T <- t_rand(modes=c(2,3,4))
#' print(t_tpose(T,"dct"))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references Brachat, J., Comon, P., Mourrain, B., & Tsigaridas, E. (2010). Symmetric tensor decomposition. Linear Algebra and its Applications, 433(11-12), 1851-1872.

t_tpose <- function (tnsr,tform)
{
  # Performs a transpose of a symmetric 3-mode tensor using any discrete
  # transform by transposing each of the lateral slices.

  # Input: a 3-mode tensor
  # Output: the transpose of the tensor

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  if (tform=="fft") {
    dTsym <- aperm(apply(tnsr$data, MARGIN = 1:2, fft), c(2,3,1))
  } else if (tform=="dwt") {
    dTsym <- tDWT(tnsr)
    dTsym <- dTsym$data
  } else if (tform=="dct") {
    dTsym <- aperm(apply(tnsr$data, MARGIN = 1:2, dct), c(2,3,1))
  } else if (tform=="dst") {
    dTsym <- aperm(apply(tnsr$data, MARGIN = 1:2, dst), c(2,3,1))
  } else if(tform=="dwht") {
    dTsym <- aperm(apply(tnsr$data, MARGIN = 1:2, fwht), c(2,3,1))
  } else if(tform=="dht") {
    dTsym <- aperm(apply(tnsr$data, MARGIN = 1:2, fht), c(2,3,1))
  } else {
    stop("Transform not supported")
  }
  tTsym <- array(0,dim = c(n2,n1,n3))
  for (i in 1:n3){
    tTsym[,,i] <- base::t(dTsym[,,i])
  }
  if (tform=="fft") {
    tTsym <- aperm(apply(tTsym, MARGIN = 1:2, ifft), c(2,3,1))
  } else if (tform=="dwt") {
    tTsym <- tIDWT(as.Tensor(tTsym))
  } else if (tform=="dct") {
    tTsym <- aperm(apply(tTsym, MARGIN = 1:2, idct), c(2,3,1))
  } else if (tform=="dst") {
    tTsym <- aperm(apply(tTsym, MARGIN = 1:2, idst), c(2,3,1))
  } else if(tform=="dwht") {
    tTsym <- aperm(apply(tTsym, MARGIN = 1:2, ifwht), c(2,3,1))
  } else if (tform=="dht") {
    tTsym <- aperm(apply(tTsym, MARGIN = 1:2, ifht), c(2,3,1))
  } else {
    stop("Transform not supported")
  }
  if (tform=="dwt"){
    return(tTsym)
  } else {
    return(as.Tensor(tTsym))
  }
}
