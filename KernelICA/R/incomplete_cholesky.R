#' Incomplete Cholesky Decomposition
#' 
#' The incomplete Cholesky decomposition, which computes approximative low rank decompositions for either Gauss or Hermite kernel matrices.
#' Its implementation is inspired by Matlab and C code of F. Bach (see references) and written with the C++ library
#' Eigen3 for speed purposes.
#' 
#' @param x Numeric vector.
#' @param kernel One of \code{"gauss"} or \code{"hermite"}.
#' @param eps Numeric precision parameter for the matrix approximation.
#' @param sigma Numeric value, setting the kernel variance. Default is 1 for vectors smaller than n=1000, otherwise 0.5.
#' @param hermite_rank Integer value for the rank of the Hermite kernel. This parameter is ignored, when the Gaussian kernel is chosen. Default is 3.
#' 
#' @return A list containing the following entries: \describe{
#' \item{L}{A numeric matrix which values \eqn{L_{ij}} are \eqn{0} for \eqn{j > i}.}
#' \item{perm}{An integer vector of indices representing the permutation matrix.}
#' }
#' 
#'
#' @details 
#' The function approximates kernel matrices of the form \eqn{\boldsymbol{K} = (K_{ij})_{(i,j)} = K(x_i, x_j)}
#' for a vector \eqn{\boldsymbol{x}} and a kernel function \eqn{K(\cdot, \cdot)}.
#' It returns a permutation matrix \eqn{\boldsymbol{P}} given as index vector and
#' a numeric \eqn{n \times k} matrix \eqn{\boldsymbol{L}} which is a "cut off" lower triangle matrix,
#' as it contains only the first \eqn{k} columns that were necessary to attain a sufficient approximation. 
#' These matrices follow the inequality
#' \eqn{\| \boldsymbol{P} \boldsymbol{K} \boldsymbol{P}^T - \boldsymbol{L} \boldsymbol{L}^T\|_1 \leq \epsilon }
#' where \eqn{\epsilon} is the given precision parameter. The function offers approximation for kernel matrices of the following two kernels:
#' \itemize{
#' \item Gauss Kernel (fix v1.0.1: corrected sign in exponent): \eqn{K(x, y) = e^{-(x-y)^2 / 2 \sigma^2}}
#' \item Hermite Kernel: \eqn{K(x, y) = \sum_{k=0}^d e^{-x^2 / 2 \sigma^2} e^{-y^2 / 2 \sigma^2} \frac{h_k(x / \sigma) h_k(y / \sigma)}{2^k k!}}, 
#' where \eqn{h_k} is the Hermite polynomial of grade \eqn{k}
#' }
#'  
#' @references 
#' 
#' Kernel ICA implementation in Matlab and C by F. Bach containing the Incomplete Cholesky Decomposition:\cr
#' \href{https://www.di.ens.fr/~fbach/kernel-ica/index.htm}{https://www.di.ens.fr/~fbach/kernel-ica/index.htm}
#' 
#' Francis R. Bach, Michael I. Jordan\cr
#' \emph{Predictive low-rank decomposition for kernel methods.}\cr
#' Proceedings of the Twenty-second International Conference on Machine Learning (ICML) 2005\cr
#' \doi{10.1145/1102351.1102356}.
#' 
#' Francis R. Bach, Michael I. Jordan\cr
#' \emph{Kernel independent component analysis}\cr
#' Journal of Machine Learning Research 2002\cr
#' \doi{10.1162/153244303768966085} \cr
#'
#' @example examples/ex_incomplete_cholesky.R
#'  
#' @export
incomplete_cholesky <-
  function(x,
           kernel = c("gauss", "hermite"),
           eps = 0.0001,
           sigma = ifelse(length(x) < 1000, 1, 0.5),
           hermite_rank = 3) {
    kernel <- match.arg(kernel)
    # the c++ wrapper function does not accept integer values from R
    x <- as.numeric(x)
    if(kernel == "gauss"){
      res <- incompleteCholeskyGauss(x, eps, sigma)
    }else if(kernel == "hermite"){
      res <- incompleteCholeskyHermite(x, eps, sigma, hermite_rank)
    }
    return(res)
}