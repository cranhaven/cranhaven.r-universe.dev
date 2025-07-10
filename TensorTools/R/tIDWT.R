#' Inverse Wavelet Transform
#' @description Performs inverse of 3-mode tensor using any discrete wavelet transform.
#' @param tnsr, a 3-mode tensor S3 class object
#' @return S3 class tensor
#' @examples
#' T <- t_rand(modes=c(2,3,4))
#' print(tIDWT(T))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo

tIDWT <- function (tnsr)
{
  # Performs the Discrete Inverse Wavelet Transform of a 3-D Tensor.

  # Input: tnsr, a 3D tensor
  # Output: The a 3D tensor

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]

  if (sum(as.numeric(intToBits(n3))) != 1)
    stop("Mode 3 must be a power of 2 otherwise using 0 padding")

  idwtz = array(0,dim=c(n1,n2,n3))
  for (i in 1:n1) {
    for (j in 1:n2) {
      x <- tnsr$data[i,j,]
      # Inverse Transform mode 3 with Haar IDWT
      l <- length(x)/2
      c.in <- x[1:l]
      d.in <- x[(l+1):length(x)]
      idwtz[i,j,] <- conbar(c.in,d.in,
                            filter.select(filter.number=1,
                                          family="DaubExPhase"))
    }
  }
  idwtz <- as.Tensor(idwtz)
  return(idwtz)
}
