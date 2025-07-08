#' Print Function for a DIV Model Object
#'
#' This function is a utility that displays a summary of a fitted DIV model object.
#'
#' @param x A trained DIV model returned from the divfit() function.
#' @param ... additional arguments (currently ignored).
#'
#' @return This function does not return anything. It prints a summary of the model,
#' including information about its architecture and training process, and the loss
#' values achieved at several epochs during training.
#'
#' @examples
#' \dontrun{
#' # NOTE: This example requires torch runtime.
#' # Please install it with: torch::install_torch()
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
#' # Fit DIV model ----------------------------------------------------
#' # Consider increasing number of epochs. Here: num_epochs = 100 for fast computation only.
#' DIV_model <- div(Z = Z, X = X, Y = Y, num_epochs = 100)
#' print(DIV_model)
#' }
#'
#' @export
print.DIV <- function(x, ...) {
  cat("\nDIV object with ")

  cat("\n \t  noise dimensions (for shared noise eps_H, and for indep. noise eps_X and eps_Y): ", "(",x$epsh_dim, ",", x$epsx_dim, ",", x$epsy_dim,")", sep = "")
  cat("\n \t  hidden dimensions: ", x$hidden_dim)
  cat("\n \t  number of layers: ", x$num_layer)
  cat("\n \t  number of epochs: ", x$num_epochs)
  cat("\n \t  learning rate: ", x$lr)
  cat("\n \t  standardization: ", x$standardize)

  m <- nrow(x$loss_vec)
  print_at <- pmax(1, floor(seq(1, m, length = 11)))
  pr <- cbind(print_at, x$loss_vec[print_at, ])
  colnames(pr) <- c("epoch", colnames(x$loss_vec))
  cat("\nTraining loss: \n")
  print(as.data.frame(pr), row.names = FALSE)
  cat("\nPrediction-loss E(||U-Uhat||) and variance-loss E(||Uhat-Uhat'||) should ideally be equally large --\nconsider training for more epochs if there is a mismatch.\n\n")
}
