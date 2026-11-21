#' Kernel Matrix Computation
#' 
#' Computes kernel matrices for Gaussian and Hermitian kernels.
#' 
#' @param x Numeric vector.
#' @param y Numeric vector, default is \code{x}.
#' @param kernel Either \code{"gauss"} or \code{"hermite"}.
#' @param sigma Numeric value of the kernel variance. Default is 1.
#' @param hermite_rank Rank of the Hermite kernel. Default is 3. Ignored, when the Gaussian kernel is chosen.
#' 
#' @details The function computes a matrix in the form of \eqn{(K_{ij})_{(i,j)} = K(x_i, x_j)} or \eqn{(K_{ij})_{(i,j)} = K(x_i, y_j)} 
#' for a kernel function \eqn{K} depending if a second vector was given. The following two kernels are offered:
#' \itemize{
#' \item Gauss Kernel: \eqn{K(x, y) = e^{-(x-y)^2 / 2 \sigma^2}}
#' \item Hermite Kernel: \eqn{K(x, y) = \sum_{k=0}^d e^{-x^2 / 2 \sigma^2} e^{-y^2 / 2 \sigma^2} \frac{h_k(x / \sigma) h_k(y / \sigma)}{2^k k!}}
#' where \eqn{h_k} is the Hermite polynomial of grade \eqn{k}
#' }
#' 
#' @return A numeric kernel matrix.
#' 
#' @example examples/ex_kernel_matrix.R
#'
#' @export
kernel_matrix <-
  function(x, y = x,
           kernel = c("gauss", "hermite"),
           sigma = 1,
           hermite_rank = 3) {
    kernel <- match.arg(kernel)
    # the c++ wrapper function does not accept integer values from R
    x <- as.numeric(x)
    y <- as.numeric(y)
    if(kernel == "gauss"){
      res <- kernelMatrixGauss(x, y, sigma)
    }else if(kernel == "hermite"){
      res <- kernelMatrixHermite(x, y, sigma, hermite_rank)
    }
    return(res)
  }