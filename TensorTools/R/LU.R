#' LU Decomposition of a Complex Matrix
#' @description
#' Decomposes a a matrix into the product of a lower triangular matrix and an upper triangular matrix.
#' @param A Complex, square matrix of complex numbers
#' @return A lower triangular matrix L and an upper triangular matrix U so that A=LU
#' @examples
#' indices <- c(2,3,4)
#' z <- complex(real = rnorm(16), imag = rnorm(16))
#' A <- matrix(z,nrow=4)
#' LU(A)
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references Stewart, G. W. (1998). Matrix algorithms: volume 1: basic decompositions. Society for Industrial and Applied Mathematics.

LU <- function (A)
{
  # LU decomposition of a complex (possibly) matrix

  # Input: A, a square matrix.
  # Output: the matrices L and U so that A=LU.

  n <- nrow(A)
  m <- ncol(A)
  decomp <- function(A) {
    n <- nrow(A)
    m <- ncol(A)
    if (n == 1) {
      L <- A
      U <- diag(n)
      result <- list(L = L, U = U)
      return(result)
    }
    else {
      L <- matrix(0, nrow = n, ncol = n)
      U <- L
      a11 <- A[1, 1]
      a12 <- A[1, 2:n]
      a21 <- A[2:n, 1]
      a22 <- A[2:n, 2:n]
      l11 <- 1
      u11 <- a11
      l12 <- matrix(0, nrow = (n - 1), ncol = (n - 1))
      u12 <- a12
      l21 <- a21/u11
      u21 <- matrix(0, nrow = (n - 1), ncol = (n - 1))
      s22 <- a22 - l21 %o% u12
      L[1, 1] <- 1
      L[2:n, 1] <- l21
      U[1, 1] <- u11
      U[1, 2:n] <- u12
      result <- list(L = L, U = U, S = s22)
      return(result)
    }
  }
  if (rankMatrix(A)[[1]] != n)
    stop("matrix is singular")
  if (m != n)
    stop("argument x is not a square matrix")
  L <- matrix(0, nrow = n, ncol = n)
  U <- L
  for (i in 1:n) {
    d <- decomp(A)
    Li <- d$L
    Ui <- d$U
    Si <- d$S
    L[i:n, i] <- Li[, 1]
    U[i, i:n] <- Ui[1, ]
    A <- Si
  }
  return(list(L = L, U = U))
}


