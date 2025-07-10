#' Singular value decomposition (SVD) of a 3D tensor using the discrete Hadley transform
#' @param tnsr, a 3-mode S3 tensor class object
#' @return U, the left singular value tensor object (\eqn{m} x \eqn{m} x \eqn{k})
#'
#' V, The right singular value tensor object (\eqn{n} x \eqn{n} x \eqn{k})
#'
#' S: A diagonal tensor (\eqn{m} x \eqn{n} x \eqn{k})#' @examples

#'  V: The right singular value tensor object (\eqn{n} x \eqn{n} x \eqn{k})

#' S: A diagonal tensor (\eqn{m} x \eqn{n} x \eqn{k})

#' @examples
#' T <- t_rand(modes=c(2,2,4))
#' tSVDdht(T)
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references M. E. Kilmer, C. D. Martin, and L. Perrone, “A third-order generalization of the matrix svd as a product of third-order tensors,” Tufts University, Department of Computer Science, Tech. Rep. TR-2008-4, 2008
#'
#' K. Braman, "Third-order tensors as linear operators on a space of matrices", Linear Algebra and its Applications, vol. 433, no. 7, pp. 1241-1253, 2010.

tSVDdht <- function (tnsr)
{
  # Performs a tensor singular value decomposition on any 3-mode
  # tensor using the discrete harley transform.

  # Input: A, 3-mode tensor
  # Output: Tensors U (left singular value object),
  # V (right singular value object) and
  # S, a diagonal tensor so that A=USV^T.

  modes <- tnsr$modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]
  dhtz <- aperm(apply(tnsr$data, MARGIN = 1:2, fht), c(2,3,1))
  U_arr <- array(0, dim = c(n1, n1, n3))
  V_arr <- array(0, dim = c(n2, n2, n3))
  m <- min(n1, n2)
  S_arr <- array(0, dim = c(n1, n2, n3))
  for (j in 1:n3) {
    decomp <- svd(dhtz[, , j], nu = n1, nv = n2)
    U_arr[, , j] <- decomp$u
    V_arr[, , j] <- decomp$v
    S_arr[, , j] <- diag(decomp$d, nrow = n1, ncol = n2)
  }
  U <- as.Tensor(aperm(apply(U_arr, MARGIN = 1:2,ifht), c(2,3,1)))
  V <- as.Tensor(aperm(apply(V_arr, MARGIN = 1:2,ifht), c(2,3,1)))
  S <- as.Tensor(aperm(apply(S_arr, MARGIN = 1:2,ifht), c(2,3,1)))
  invisible(list(U = U, V = V, S = S))
}
