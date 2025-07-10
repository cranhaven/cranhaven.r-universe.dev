#' Eigenvalue decomposition of 3-mode tensor using the discrete Hadley transform.
#' @param tnsr, a 3-mode S3 tensor class object (\eqn{n} x \eqn{n} x \eqn{k})
#' @return P, tensor of Eigenvectors (\eqn{n} x \eqn{n} x \eqn{k})
#' @return D, diagonal tensor of Eigenvalues (\eqn{n} x \eqn{n} x \eqn{k})
#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' print(tEIGdht(T))
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tEIGdht <- function (tnsr)
{
  # Performs a Eigenvalue decomposition of 3-mode tensor
  # using the discrete hadley transform.

  # Input: tnsr, a 3D tensor
  # Output: A tensor P of eigenvectors and a tensor D
  # eigenvalues so that tnsr = P D P^-1

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dhtz <- aperm(apply(tnsr$data, MARGIN = 1:2, fht), c(2,3,1))
  P_arr <- array(0, dim = c(n1, n2, n3))
  D_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- eigen(dhtz[, , j])
    decompp <- polar(decomp$vectors,decomp$values)
    P_arr[, , j] <- decompp$P
    D_arr[, , j] <- decompp$D
  }
  P <- as.Tensor(aperm(apply(P_arr, MARGIN = 1:2,ifht), c(2,3,1)))
  D <- as.Tensor(aperm(apply(D_arr, MARGIN = 1:2,ifht), c(2,3,1)))
  invisible(list(P = P, D = D))
}
