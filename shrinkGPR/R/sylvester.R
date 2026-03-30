#' @title Sylvester Normalizing Flow
#' @description
#' The \code{sylvester} function implements Sylvester normalizing flows as described by
#' van den Berg et al. (2018) in "Sylvester Normalizing Flows for Variational Inference."
#' This flow applies a sequence of invertible transformations to map a simple base distribution
#' to a more complex target distribution, allowing for flexible posterior approximations in Gaussian process regression models.
#' @name sylvester
#' @param d An integer specifying the latent dimensionality of the input space.
#' @param n_householder An optional integer specifying the number of Householder reflections used to orthogonalize the transformation.
#' Defaults to \code{d - 1}.
#' @return An \code{nn_module} object representing the Sylvester normalizing flow. The module has the following key components:
#' \itemize{
#'   \item \code{forward(zk)}: The forward pass computes the transformed variable \code{z} and the log determinant of the Jacobian.
#'   \item Internal parameters include matrices \code{R1} and \code{R2}, diagonal elements, and Householder reflections used for orthogonalization.
#' }
#' @details
#' The Sylvester flow uses two triangular matrices (\code{R1} and \code{R2}) and Householder reflections to construct invertible transformations.
#' The transformation is parameterized as follows:
#' \deqn{z = Q R_1 h(Q^T R_2 zk + b) + zk,}
#' where:
#' \itemize{
#'   \item \code{Q} is an orthogonal matrix obtained via Householder reflections.
#'   \item \code{R1} and \code{R2} are upper triangular matrices with learned diagonal elements.
#'   \item \code{h} is a non-linear activation function (default: \code{torch_tanh}).
#'   \item \code{b} is a learned bias vector.
#' }
#'
#' The log determinant of the Jacobian is computed to ensure the invertibility of the transformation and is given by:
#' \deqn{\log |det J| = \sum_{i=1}^d \log |diag_1[i] \cdot diag_2[i] \cdot h'(RQ^T zk + b) + 1|,}
#' where \code{diag_1} and \code{diag_2} are the learned diagonal elements of \code{R1} and \code{R2}, respectively, and \code{h\'} is the derivative of the activation function.
#'
#' @examples
#' if (torch::torch_is_installed()) {
#'   # Example: Initialize a Sylvester flow
#'   d <- 5
#'   n_householder <- 4
#'   flow <- sylvester(d, n_householder)
#'
#'   # Forward pass through the flow
#'   zk <- torch::torch_randn(10, d)  # Batch of 10 samples
#'   result <- flow(zk)
#'
#'   print(result$zk)
#'   print(result$log_diag_j)
#' }
#' @references
#' van den Berg, R., Hasenclever, L., Tomczak, J. M., & Welling, M. (2018).
#' "Sylvester Normalizing Flows for Variational Inference."
#' \emph{Proceedings of the Thirty-Fourth Conference on Uncertainty in Artificial Intelligence (UAI 2018)}.
#' @export
sylvester <- nn_module(
  classname = "Sylvester",
  initialize = function(d, n_householder) {


    if (missing(n_householder)) {
      n_householder <- min(5, d - 1)
    }

    self$d <- d
    self$n_householder <- n_householder

    self$h <- torch_tanh

    self$diag_activation <- torch_tanh

    reg <- 1/sqrt(self$d)
    self$both_R <- nn_parameter(torch_zeros(self$d, self$d)$uniform_(-reg, reg))

    self$diag1 <- nn_parameter(torch_zeros(self$d)$uniform_(-reg, reg))
    self$diag2 <- nn_parameter(torch_zeros(self$d)$uniform_(-reg, reg))

    self$Q <- nn_parameter(torch_zeros(self$n_householder, self$d)$uniform_(-reg, reg))

    self$b <- nn_parameter(torch_zeros(self$d)$uniform_(-reg, reg))

    triu_mask <- torch_triu(torch_ones(self$d, self$d), diagonal = 1)$requires_grad_(FALSE)
    identity <- torch_eye(self$d, self$d)$requires_grad_(FALSE)

    self$triu_mask <- nn_buffer(triu_mask)
    self$identity <- nn_buffer(identity)

    self$register_buffer("triu_mask", triu_mask)
    self$register_buffer("eye", identity)

  },

  der_tanh = function(x) {
    return(1 - self$h(x)^2)
  },

  der_h = function(x) {
    return(1 - self$h(x)^2)
  },

  forward = function(z) {

    # Bring all flow parameters into right shape
    diag1 <- self$diag_activation(self$diag1)
    diag2 <- self$diag_activation(self$diag2)

    r1 <- self$both_R * self$triu_mask
    r1 <- r1 + torch_diag_embed(diag1)

    r2 <- self$both_R$t() * self$triu_mask
    r2 <- r2 + torch_diag_embed(diag2)

    # Orthogonalize q via Householder reflections
    out <- .shrinkGPR_internal$jit_funcs$sylvester_full(
      z = z,
      Q_param = self$Q,
      r1 = r1,
      r2 = r2,
      b = self$b,
      diag1 = diag1,
      diag2 = diag2,
      n_householder = as.integer(self$n_householder)
    )
    zk <- out[[1]]
    log_det_J <- out[[2]]

    return(list(zk = zk, log_diag_j = log_det_J))

  }
)
