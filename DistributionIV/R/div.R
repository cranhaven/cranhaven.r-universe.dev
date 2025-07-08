#' Distributional IV Model Function
#'
#' This function fits a distributional IV model to the data. It allows for
#' the tuning of several parameters related to model complexity and model training.
#' Variables are per default internally standardized (predictions are on the original scale).
#'
#' @param Z A data frame, matrix, vector, or factor variable representing the instrumental variable.
#' @param X A data frame, matrix, vector, or factor variable representing the predictor.
#' @param Y A data frame, matrix, vector, or factor variable representing the target variable.
#' @param epsx_dim The dimension of the noise corresponding to the predictor introduced in the model (default: 50).
#' @param epsy_dim The dimension of the noise corresponding to the outcome introduced in the model (default: 50).
#' @param epsh_dim The dimension of the noise corresponding to the hidden confounder introduced in the model (default: 50).
#' @param hidden_dim The size of the hidden layer in the model (default: 100).
#' @param num_layer The number of layers in the model (default: 3).
#' @param num_epochs The number of epochs to be used in training (default: 1000).
#' @param lr The learning rate to be used in training (default: 10^-3).
#' @param beta The beta scaling factor for energy loss, numeric value from (0,2) (default: 1).
#' @param silent A boolean indicating whether to suppress output during model training (default: FALSE).
#' @param W (Optional) A data frame, matrix, vector, or factor variable representing the exogenous variable(s).
#' @param standardize A boolean indicating whether to standardize the input data (default: TRUE).
#'
#' @return A distributional IV model object with class 'DIV'.
#'
#' @import checkmate
#' @importFrom stats sd
#'
#' @examples
#' \dontrun{
#' # NOTE: This example requires torch runtime.
#' # Please install it with: torch::install_torch()
#' 
#' # Simulate data -------------------------------------------------------------
#' p_Z <- 4
#' p_X <- 2
#'
#' set.seed(2209)
#' n_train <- 1000
#' Z <- matrix(rnorm(n_train * p_Z, mean = 2), ncol = p_Z)
#' H <- rnorm(n_train, mean = 0, sd = 1.5)
#' X1 <- 0.1 * (Z[, 1] + rnorm(n_train, sd = 0.1)) ^ 2 +
#'  (Z[, 2] + rnorm(n_train, sd = 1)) ^ 2 + H + rnorm(n_train, sd = 0.1)
#' X2 <- 0.5 * (Z[, 3] + Z[, 4]) ^ 2 + 0.1 * H ^ 2 + rnorm(n_train, sd = 0.1)
#' X <- matrix(cbind(X1, X2), ncol = p_X)
#' Y <- 0.5 * X[, 1] + 0.2 * (X[, 2] + rnorm(n_train, sd = 0.2) + H) ^ 2 +
#'  rnorm(n_train, sd = 0.1)
#'
#' n_test <- n_train
#' Ztest <- matrix(rnorm(n_test * p_Z, mean = 2), ncol = p_Z)
#' Htest <- rnorm(n_test, mean = 0, sd = 1.5)
#' X1test <- 0.1 * (Ztest[, 1] + rnorm(n_test, sd = 0.1)) ^ 2 +
#'  (Ztest[, 2] + rnorm(n_test, sd = 1)) ^ 2 + Htest + rnorm(n_test, sd = 0.1)
#' X2test <- 0.5 * (Ztest[, 3] + Ztest[, 4]) ^ 2 + 0.1 * Htest ^ 2 + rnorm(n_test, sd = 0.1)
#' Xtest <- matrix(cbind(X1test, X2test), ncol = p_X)
#' Ytest <- 0.5 * Xtest[, 1] + 0.2 * (Xtest[, 2] + rnorm(n_test, sd = 0.2) + Htest) ^ 2 +
#'  rnorm(n_test, sd = 0.1)
#'
#' # Fit generativeIV model ----------------------------------------------------
#' # Consider increasing number of epochs. Here: num_epochs = 100 for fast computation only.
#' DIV_model <- div(Z = Z, X = X, Y = Y, num_epochs = 100)
#' print(DIV_model)
#'
#' # Prediction on test data ---------------------------------------------------
#' Yhat <- predict(object = DIV_model, Xtest = Xtest, type = "mean")
#' cat("\n Correlation between predicted and realized values: ", signif(cor(Yhat, Ytest), 3))
#' plot(Yhat, Ytest, xlab = "model prediction", ylab = "observation")

#' # Quantile prediction -------------------------------------------------------
#' Yhat_quant <- predict(object = DIV_model, Xtest = Xtest, type = "quantile")
#' ord <- order(Yhat)
#' matplot(Yhat[ord], Yhat_quant[ord,], type = "l", col = 2, lty = 1,
#' xlab = "model prediction", ylab = "observation")
#' points(Yhat[ord], Ytest[ord], pch = 20, cex = 0.5)
#'
#' #' # Sampling from estimated model ---------------------------------------------
#' Ysample <- predict(object = DIV_model, Xtest = Xtest, type = "sample", nsample = 1)
#'
#' #' # Plots of realized & sampled values against first variable -----------------
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow = c(1, 2))
#' plot(Xtest[, 1], Ytest, xlab = "Predictor Variable 1", ylab = "Observation")
#' plot(Xtest[, 1], Ysample, xlab = "Predictor Variable 1", ylab = "Model sample")
#' par(oldpar)
#' }
#'
#' @export
div <- function(
    Z, X, Y, W = NULL,
    epsx_dim = 50, epsy_dim = 50, epsh_dim = 50,
    hidden_dim = 100, num_layer = 3,
    num_epochs = 1000, lr = 10^(-3), beta = 1,
    silent = FALSE, standardize = TRUE) {
  # Check input parameters (most of input parameters checked in divfit())
  assert_logical(standardize)

  Z <- check_input(Z)
  X <- check_input(X)
  Y <- check_input(Y)
  if (!is.null(W)) {
    W <- check_input(W)
  }

  if (dim(Z)[1] != dim(X)[1] || dim(Z)[1] != dim(Y)[1]) {
    stop("Sample size should be same for instrument(s) Z, predictor(s) X and outcome(s) Y.")
  }
  if (!is.null(W) && dim(W)[1] != dim(Z)[1]) {
    stop("Sample size of W must match those of Z, X and Y.")
  }
  if (dim(Z)[1] <= 1) {
    stop("Sample size should be greater than 1!")
  }

  muZ <- apply(Z, 2, mean)
  sddZ <- apply(Z, 2, stats::sd)
  if (any(sddZ <= 0)) {
    warning("instrument variable(s) ", colnames(Z)[which(sddZ <= 0)],
            " are constant on training data -- results might be unreliable")
    sddZ <- pmax(sddZ, 10^(-3))
  }

  muX <- apply(X, 2, mean)
  sddX <- apply(X, 2, stats::sd)
  if (any(sddX <= 0)) {
    warning("predictor variable(s) ", colnames(X)[which(sddX <= 0)],
            " are constant on training data -- results might be unreliable")
    sddX <- pmax(sddX, 10^(-3))
  }

  muY <- apply(Y, 2, mean)
  sddY <- apply(Y, 2, stats::sd)
  if (any(sddY <= 0)) {
    warning("outcome variable(s) ", colnames(Y)[which(sddY <= 0)],
            " are constant on training data -- results might be unreliable")
    sddY <- pmax(sddY, 10^(-3))
  }

  if (standardize) {
    Z <- sweep(sweep(Z, 2, muZ, FUN = "-"), 2, sddZ, FUN = "/")
    X <- sweep(sweep(X, 2, muX, FUN = "-"), 2, sddX, FUN = "/")
    Y <- sweep(sweep(Y, 2, muY, FUN = "-"), 2, sddY, FUN = "/")
  }

  muW <- sddW <- NULL

  if (!is.null(W)) {
    W <- check_input(W)
    muW <- apply(W, 2, mean)
    sddW <- apply(W, 2, stats::sd)

    if (any(sddW <= 0)) {
      warning("Exogenous predictor variable(s) ", colnames(W)[which(sddW <= 0)],
              " are constant on training data -- results might be unreliable")
      sddW <- pmax(sddW, 10^(-3))
    }

    if (standardize) {
      W <- sweep(sweep(W, 2, muW, FUN = "-"), 2, sddW, FUN = "/")
    }
  }

  DIV_fit <- divfit(Z = Z, X = X, Y = Y, W = W,
                    epsx_dim = epsx_dim, epsy_dim = epsy_dim, epsh_dim = epsh_dim,
                    hidden_dim = hidden_dim, num_layer = num_layer,
                    num_epochs = num_epochs, lr = lr, beta = beta, silent = silent)
  DIV <- list(
    DIV_f = DIV_fit$DIV_f,
    DIV_g = DIV_fit$DIV_g,
    loss_vec = DIV_fit$loss_vec,
    Z = Z, X = X, W = W, Y = Y,
    muZ = muZ, sddZ = sddZ, muX = muX, sddX = sddX, muW = muW, sddW = sddW, muY = muY, sddY = sddY,
    standardize = standardize,
    epsh_dim = epsh_dim, epsx_dim = epsx_dim, epsy_dim = epsy_dim,
    hidden_dim = hidden_dim,
    num_layer = num_layer, num_epochs = num_epochs, lr = lr
    )

  class(DIV) <- "DIV"
  return(DIV)
}
