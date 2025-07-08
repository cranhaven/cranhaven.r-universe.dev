#' Prediction Function for a DIV Model Object
#'
#' This function computes predictions from a trained DIV model. It allows for estimation
#' of the interventional mean and quantiles, as well as sampling from the fitted interventional distribution.
#' If the model includes exogenous predictors, it allows for estimation of the conditional interventional mean
#' and quantiles, as well as sampling from the fitted conditional interventional distribution.
#'
#' @param object A trained DIV model returned from div or divfit functions.
#' @param Xtest A matrix or data frame representing predictors in the test set.
#' @param Wtest A matrix or data frame representing exogenous predictors in the test set.
#' If the model includes exogenous predictors, `Wtest` has to be specified for
#' computation of conditional treatment estimates or to draw samples from the conditional
#' interventional distribution.
#' @param type The type of prediction to make:
#'   * `mean`: for point estimates (the default).
#'   * `sample`: for samples from the estimated distribution.
#'   * `quantile`/`quantiles`: for quantiles of the estimated distribution.
#' @param trim The proportion of extreme values to trim when calculating the mean (default: 0.05).
#' @param quantiles The quantiles to estimate if type is `quantile` (default: 0.1*(1:9)).
#' @param nsample The number of samples to draw if type is `sample` (default: 200).
#' @param drop A boolean indicating whether to drop dimensions of length 1 from the output (default: TRUE).
#' @param ... additional arguments (currently ignored).
#' @return A vector or matrix/array of predictions.
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
#' # Fit DIV model -------------------------------------------------------------
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
#' @importFrom stats quantile sd
#' @export
predict.DIV <- function(object, Xtest, Wtest = NULL,
                        type = c("mean", "sample", "quantile")[1],
                        trim = 0.05, quantiles = 0.1 * (1:9),
                        nsample = 200, drop = TRUE, ...) {
  if (!(type %in% c("mean", "sample", "quantile"))) {
    stop("Type must be one of 'mean', 'sample', or 'quantile'.")
  }

  device <- use_device()

  # Validate input parameters
  assert_numeric(trim, lower = 0, upper = 0.5)
  assert_count(nsample)
  assert_numeric(quantiles, lower = 0, upper = 1)
  assert_logical(drop)
  Xtest <- check_input(Xtest)
  if(!is.null(Wtest)){
    Wtest <- check_input(Wtest)
  }

  if (ncol(object$X) != ncol(Xtest)) {
    stop("'Xtest' should contain the same number of variables as used for training.")
  }

  if (!is.null(object$W) && is.null(Wtest)) {
    stop("Exogenous predictors W have been used during training.
         Specify Wtest for prediction of *conditional* causal effect.")
  }

  if (!is.null(Wtest) && ncol(object$W) != ncol(Wtest)) {
    stop("'Wtest' should contain the same number of variables as used for training.")
  }

  if (!is.null(Wtest) && nrow(Wtest) != nrow(Xtest)) {
    stop("'Xtest' and 'Wtest' should be of the same sample size.")
  }

  Xtest <- torch_tensor(Xtest, device = device)
  if(!is.null(Wtest)){
    Wtest <- torch_tensor(Wtest, device = device)
  }

  # Standardize input if required
  if (object$standardize) {
    Xtest <- (Xtest - torch_tensor(object$muX, device = device)) / torch_tensor(object$sddX, device = device)
    if(!is.null(Wtest)){
      Wtest <- (Wtest - torch_tensor(object$muW, device = device)) / torch_tensor(object$sddW, device = device)
    }
  }

  # Generate predictions
  if (!is.null(Wtest)) {
    Yhat1 <- object$DIV_f(x = Xtest, w = Wtest)
    Yhat <- array(dim = c(dim(Yhat1)[1], dim(Yhat1)[2], nsample))

    for (sam in 1:nsample) {
      pred <- object$DIV_f(x = Xtest, w = Wtest)
      if (object$standardize) {
        pred <- sweep(sweep(pred, 2, object$sddY, FUN = "*"), 2, object$muY, FUN = "+")
      }
      Yhat[, , sam] <- pred
    }
  } else {
    # If W is not provided
    Yhat1 <- object$DIV_f(x = Xtest)
    Yhat <- array(dim = c(dim(Yhat1)[1], dim(Yhat1)[2], nsample))

    for (sam in 1:nsample) {
      pred <- object$DIV_f(x = Xtest)
      if (object$standardize) {
        pred <- sweep(sweep(pred, 2, object$sddY, FUN = "*"), 2, object$muY, FUN = "+")
      }
      Yhat[, , sam] <- pred
    }
  }

  return(process_output(Yhat, type, quantiles, trim, drop))
}

# Helper function to process output (mean, sample, quantile)
process_output <- function(Yhat, type, quantiles, trim, drop) {
  if (type == "sample") {
    dimnames(Yhat)[[3]] <- paste("sample_", 1:dim(Yhat)[3], sep = "")
    return(if (drop) drop(Yhat) else Yhat)
  }
  if (type == "mean") {
    return(if (drop) drop(apply(Yhat, 1:(length(dim(Yhat)) - 1), mean, trim = trim)) else
      apply(Yhat, 1:(length(dim(Yhat)) - 1), mean, trim = trim))
  }
  if (type == "quantile") {
    if (length(quantiles) == 1) {
      return(if (drop) drop(apply(Yhat, 1:(length(dim(Yhat)) - 1), stats::quantile, quantiles)) else
        apply(Yhat, 1:(length(dim(Yhat)) - 1), stats::quantile, quantiles))
    } else {
      return(if (drop) drop(aperm(apply(Yhat, 1:(length(dim(Yhat)) - 1), quantile, quantiles), c(2, 3, 1))) else
        aperm(apply(Yhat, 1:(length(dim(Yhat)) - 1), quantile, quantiles), c(2, 3, 1)))
    }
  }
}
