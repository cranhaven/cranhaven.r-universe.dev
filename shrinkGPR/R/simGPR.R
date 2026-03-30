#' Simulate Data for Gaussian Process Regression
#'
#' \code{simGPR} generates simulated data for Gaussian Process Regression (GPR) models, including the true hyperparameters used for simulation.
#'
#' @param N Positive integer specifying the number of observations to simulate. Default is 200.
#' @param d Positive integer specifying the number of covariates for the covariance structure. Default is 3.
#' @param d_mean Positive integer specifying the number of covariates for the mean structure. Default is 0.
#' @param sigma2 Positive numeric value specifying the noise variance. Default is 0.1.
#' @param tau Positive numeric value specifying the global shrinkage parameter. Default is 2.
#' @param kernel_func Function specifying the covariance kernel. Default is \code{kernel_se}.
#' @param perc_spars Numeric value in \[0, 1\] indicating the proportion of elements in \code{theta}
#' and \code{beta} to sparsify. Default is 0.5.
#' @param rho Numeric value in \[0, 1\] indicating the correlation between the covariates. Default is 0.
#' @param theta \emph{Optional} numeric vector specifying the true inverse length-scale parameters.
#' If not provided, they are randomly generated.
#' @param beta \emph{Optional} numeric vector specifying the true regression coefficients for the mean
#' structure. If not provided, they are randomly generated.
#' @param device \emph{Optional} \code{torch_device} object specifying whether to run the simulation
#' on CPU or GPU. Defaults to GPU if available.
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: A data frame with \code{y} (response variable), \code{x} (covariates for the covariance structure),
#'   and optionally \code{x_mean} (covariates for the mean structure).
#'   \item \code{true_vals}: A list containing the true values used for the simulation:
#'     \itemize{
#'       \item \code{theta}: The true inverse length-scale parameters.
#'       \item \code{sigma2}: The true noise variance.
#'       \item \code{tau}: The true global shrinkage parameter.
#'       \item \code{beta} (optional): The true regression coefficients for the mean structure.
#'     }
#' }
#' @details
#' This function simulates data from a Gaussian Process Regression model.
#' The response variable \code{y} is sampled from a multivariate normal distribution with
#' a covariance matrix determined by the specified kernel function, \code{theta}, \code{tau},
#' and \code{sigma2}. If \code{d_mean > 0}, a mean structure is included in the simulation, with
#' covariates \code{x_mean} and regression coefficients \code{beta}.
#'
#' @examples
#' if (torch::torch_is_installed()) {
#'   torch::torch_manual_seed(123)
#'
#'   # Simulate data with default settings
#'   sim_data <- simGPR()
#'
#'   # Simulate data with custom settings
#'   sim_data <- simGPR(N = 100, d = 5, d_mean = 2, perc_spars = 0.3, sigma2 = 0.5)
#'
#'   # Access the simulated data
#'   head(sim_data$data)
#'
#'   # Access the true values used for simulation
#'   sim_data$true_vals
#'   }
#' @export
simGPR <-  function(N = 200,
                    d = 3,
                    d_mean = 0,
                    sigma2 = 0.1,
                    tau = 2,
                    kernel_func = kernel_se,
                    perc_spars = 0.5,
                    rho = 0,
                    theta,
                    beta,
                    device){

  # Input checking for simGPR ----------------------------------------------

  # Check that N is a positive integer
  if (!is.numeric(N) || N <= 0 || N %% 1 != 0) {
    stop("The argument 'N' must be a positive integer.")
  }

  # Check that d is a positive integer
  if (!is.numeric(d) || d <= 0 || d %% 1 != 0) {
    stop("The argument 'd' must be a positive integer.")
  }

  # Check that d_mean is a non-negative integer
  if (!is.numeric(d_mean) || d_mean < 0 || d_mean %% 1 != 0) {
    stop("The argument 'd_mean' must be a non-negative integer.")
  }

  # Check that sigma2 is a positive numeric value
  if (!is.numeric(sigma2) || sigma2 <= 0) {
    stop("The argument 'sigma2' must be a positive numeric value.")
  }

  # Check that tau is a positive numeric value
  if (!is.numeric(tau) || tau <= 0) {
    stop("The argument 'tau' must be a positive numeric value.")
  }

  # Check that kernel_func is a function
  if (!is.function(kernel_func)) {
    stop("The argument 'kernel_func' must be a valid function.")
  }

  # Check that perc_spars is a numeric value in [0, 1]
  if (!is.numeric(perc_spars) || perc_spars < 0 || perc_spars > 1) {
    stop("The argument 'perc_spars' must be a numeric value between 0 and 1.")
  }

  # Check that theta, if provided, is a numeric vector of length d
  if (!missing(theta)) {
    if (!is.numeric(theta) || !is.vector(theta) || length(theta) != d) {
      stop("The argument 'theta', if provided, must be a numeric vector of length 'd'.")
    }
  }

  # Check that beta, if provided, is a numeric vector of length d_mean
  if (!missing(beta)) {
    if (!is.numeric(beta) || !is.vector(beta) || length(beta) != d_mean) {
      stop("The argument 'beta', if provided, must be a numeric vector of length 'd_mean'.")
    }
  }

  # Check that device is a torch_device
  if (!missing(device) && !inherits(device, "torch_device")) {
    stop("The argument 'device', if provided, must be a valid 'torch_device' object.")
  }

  # Check that rho is a numeric value between 0 and 1
  if (!is.numeric(rho) || rho < 0 || rho > 1) {
    stop("The argument 'rho' must be a numeric value between 0 and 1.")
  }

  # Add device attribute, set to GPU if available
  if (missing(device)) {
    if (cuda_is_available()) {
      device <- torch_device("cuda")
    } else {
      device <- torch_device("cpu")
    }
  }

  # Take user supplied beta/theta or generate them
  if (missing(beta)){
    beta <- rnorm(d_mean, 0, 1)

    # Sparsify
    beta[sample(1:d_mean, round(d_mean * perc_spars))] <- 0
  } else {
    if (is.vector(beta) == FALSE){
      stop("beta has to be a vector")
    }

    if (length(beta) != d_mean){
      stop("beta has to be of length d_mean")
    }
  }

  if (missing(theta)){
    theta <- rgamma(d, 6, 24)

    # Sparsify
    theta[sample(1:d, round(d * perc_spars))] <- 0
  } else {

    if (is.vector(theta) == FALSE){
      stop("theta has to be a vector")
    }

    if (length(theta) != d){
      stop("theta has to be of length d")
    }
  }

  # Transform into torch tensors
  beta_tens <- torch_tensor(beta, device = device)$unsqueeze(2)
  theta_tens <- torch_tensor(theta, device = device)$unsqueeze(1)
  tau_tensor <- torch_tensor(tau, device = device)$unsqueeze(2)

  # Generate x
  if (rho > 0) {
    Sigma <- matrix(rho, nrow = d, ncol = d)
    diag(Sigma) <- 1
    Sigma <- torch_tensor(Sigma, dtype = torch_float(), device = device)

    # Cholesky decomposition
    L <- linalg_cholesky(Sigma)

    # Generate standard normal noise
    Z <- torch_randn(c(N, d), device = device)

    # Multiply by Cholesky factor to introduce correlation
    x_tens <- Z$matmul(L$t())
  } else {
    x_tens <- torch_randn(N, d, device = device)
  }

  K <- kernel_func(theta_tens, tau_tensor, x_tens)$squeeze() + sigma2 * torch_eye(N, device = device)

  if (d_mean > 0) {
    x_mean <- torch_randn(N, d_mean, device = device)
    mu <- torch_mm(x_mean, beta_tens)
  } else {
    mu <- torch_zeros(N, 1, device = device)
  }


  multivar_dist <- distr_multivariate_normal(mu$squeeze(), K)

  y <- multivar_dist$sample()

  if (d_mean > 0) {
    data_frame <- data.frame(y = as.numeric(y),
                             x = as.matrix(x_tens),
                             x_mean = as.matrix(x_mean))
  } else {
    data_frame <- data.frame(y = as.numeric(y),
                             x = as.matrix(x_tens))
  }

  res <- list(data = data_frame,
              true_vals = list(theta = theta,
                               sigma2 = sigma2,
                               tau = tau))

  if (d_mean > 0) {
    res$true_vals$beta <- beta
  }

  return(res)
}
