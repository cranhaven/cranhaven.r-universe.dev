#' Performs inverse of 3-mode tensor using the discrete wavelet transform.
#' @param tnsr, a 3-mode S3 tensor class object
#' @return S3 class tensor
#' @examples
#' tnsr <- t_rand(modes=c(2,2,4))
#' print(tINVdwt(tnsr))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo

tINVdwt <- function(tnsr)
{
  # Performs inverse of 3-mode tensor using the
  # discrete wavelet transform.

  # Input: tnsr, a 3D tensor
  # Output: The inverse of tnsr, a 3D tensor

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dwtz <- tDWT(tnsr)
  T_inv <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    T_inv[, , j] <- solve(dwtz$data[, , j])
  }
  Tinv <- tIDWT(as.Tensor(T_inv))
}
