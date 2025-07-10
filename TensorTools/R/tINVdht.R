#' Performs inverse of 3-mode tensor using the discrete Hadley transform.
#' @param tnsr, a 3-mode S3 tensor class object
#' @return S3 class tensor
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' print(tINVdht(T))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo

tINVdht <- function(tnsr)
{
  # Performs inverse of 3-mode tensor using the
  # discrete hartley transform.

  # Input: tnsr, a 3D tensor
  # Output: The inverse of tnsr, a 3D tensor

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dhtz <- aperm(apply(tnsr$data, MARGIN = 1:2, fht), c(2,3,1))
  T_inv <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    T_inv[, , j] <- solve(dhtz[, , j])
  }
  T_inv <- as.Tensor(aperm(apply(T_inv, MARGIN = 1:2, ifht), c(2,3,1)))
  return(T_inv)
}
