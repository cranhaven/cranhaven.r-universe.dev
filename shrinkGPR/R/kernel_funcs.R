#' @title Kernel Functions for Gaussian Processes
#' @description
#' A set of kernel functions for Gaussian processes, including the squared exponential (SE) kernel and Matérn kernels
#' with smoothness parameters 1/2, 3/2, and 5/2. These kernels compute the covariance structure for Gaussian process regression
#' models and are designed for compatibility with the \code{shrinkGPR} function.
#' @name kernel_functions
#' @param thetas A \code{torch_tensor} of dimensions \code{n_latent x d}, representing the latent length-scale parameters.
#' @param tau A \code{torch_tensor} of length \code{n_latent}, representing the latent scaling factors.
#' @param x A \code{torch_tensor} of dimensions \code{N x d}, containing the input data points.
#' @param x_star Either \code{NULL} or a \code{torch_tensor} of dimensions \code{N_new x d}. If \code{NULL}, the kernel is computed
#' for \code{x} against itself. Otherwise, it computes the kernel between \code{x} and \code{x_star}.
#' @return A \code{torch_tensor} containing the batched covariance matrices (one for each latent sample):
#' \itemize{
#'   \item If \code{x_star = NULL}, the output is of dimensions \code{n_latent x N x N}, representing pairwise covariances between all points in \code{x}.
#'   \item If \code{x_star} is provided, the output is of dimensions \code{n_latent x N_new x N}, representing pairwise covariances between \code{x_star} and \code{x}.
#' }
#' @details
#' These kernel functions are used to define the covariance structure in Gaussian process regression models. Each kernel implements a specific covariance function:
#' \itemize{
#'   \item \code{kernel_se}: Squared exponential (SE) kernel, also known as the radial basis function (RBF) kernel.
#'   It assumes smooth underlying functions.
#'   \item \code{kernel_matern_12}: Matérn kernel with smoothness parameter \eqn{\nu = 1/2}, equivalent to the absolute exponential kernel.
#'   \item \code{kernel_matern_32}: Matérn kernel with smoothness parameter \eqn{\nu = 3/2}.
#'   \item \code{kernel_matern_52}: Matérn kernel with smoothness parameter \eqn{\nu = 5/2}.
#' }
#'
#' The \code{sqdist} helper function is used internally by these kernels to compute squared distances between data points.
#'
#' Note that these functions perform no input checks, as to ensure higher performance.
#' Users should ensure that the input tensors are of the correct dimensions.
#' @examples
#' if (torch::torch_is_installed()) {
#'   # Example inputs
#'   torch::torch_manual_seed(123)
#'   n_latent <- 3
#'   d <- 2
#'   N <- 5
#'   thetas <- torch::torch_randn(n_latent, d)$abs()
#'   tau <- torch::torch_randn(n_latent)$abs()
#'   x <- torch::torch_randn(N, d)
#'
#'   # Compute the SE kernel
#'   K_se <- kernel_se(thetas, tau, x)
#'   print(K_se)
#'
#'   # Compute the Matérn 3/2 kernel
#'   K_matern32 <- kernel_matern_32(thetas, tau, x)
#'   print(K_matern32)
#'
#'   # Compute the Matérn 5/2 kernel with x_star
#'   x_star <- torch::torch_randn(3, d)
#'   K_matern52 <- kernel_matern_52(thetas, tau, x, x_star)
#'   print(K_matern52)
#' }
NULL

sqdist <- function(x, thetas, x_star = NULL) {
  sqdist <- .shrinkGPR_internal$jit_funcs$sqdist(x, thetas, x_star)
  return(sqdist)
}

#' @rdname kernel_functions
#' @export
kernel_se <- function(thetas, tau, x, x_star = NULL) {
  K <-  .shrinkGPR_internal$jit_funcs$kernel_se(thetas, tau, x, x_star)

  return(K)
}

#' @rdname kernel_functions
#' @export
kernel_matern_12 <- function(thetas, tau, x, x_star = NULL) {
  K <- .shrinkGPR_internal$jit_funcs$kernel_matern_12(thetas, tau, x, x_star)
  return(K)
}

#' @rdname kernel_functions
#' @export
kernel_matern_32 <- function(thetas, tau, x, x_star = NULL) {
  K <- .shrinkGPR_internal$jit_funcs$kernel_matern_32(thetas, tau, x, x_star)
  return(K)
}

#' @rdname kernel_functions
#' @export
kernel_matern_52 <- function(thetas, tau, x, x_star = NULL) {
  K <- .shrinkGPR_internal$jit_funcs$kernel_matern_52(thetas, tau, x, x_star)
  return(K)
}
