#' @title General Interface for EMNearestMeanClassifier model
#' @description model from RSSL package
#' Semi-Supervised Nearest Mean Classifier using Expectation Maximization
#'
#' Expectation Maximization applied to the nearest mean classifier assuming Gaussian classes with a spherical covariance matrix.
#'
#' Starting from the supervised solution, uses the Expectation Maximization algorithm (see Dempster et al. (1977)) to iteratively update the means and shared covariance of the classes (Maximization step) and updates the responsibilities for the unlabeled objects (Expectation step).
#' @param method character; Currently only "EM"
#' @param scale Should the features be normalized? (default: FALSE)
#' @param eps Stopping criterion for the maximinimization
#' @references Dempster, A., Laird, N. & Rubin, D., 1977. Maximum likelihood from incomplete data via the EM algorithm. Journal of the Royal Statistical Society. Series B, 39(1), pp.1-38.
#' @example demo/EMNearestMeanClassifier.R
#' @importFrom RSSL EMNearestMeanClassifier
#' @export
EMNearestMeanClassifierSSLR <- function(method = "EM", scale = FALSE,
                                         eps = 1e-04) {

  train_function <- function(x, y) {

    load_RSSL()

    number_classes <- length(levels(y))

    #Check binary problem
    if (number_classes > 2) {
      stop("EMNearestMeanClassifierSSLR is for binary problems")
    }

    list_values <- get_x_y_And_unlabeled(x, y)

    model <- RSSL::EMNearestMeanClassifier(X = list_values$x, y = list_values$y, X_u = list_values$X_u,
                                            method = method, scale = scale,
                                            eps = eps)

    result <- list(
      model = model
    )

    result$classes = levels(y)
    result$pred.params = c("class","raw")
    result$mode = "classification"
    class(result) <- "EMNearestMeanClassifierSSLR"

    return(result)
  }

  args <- list(
    method = method, scale = scale,
    eps = eps
  )

  new_model_sslr(train_function, "EMNearestMeanClassifierSSLR", args)

}


#' @title Predict EMNearestMeanClassifierSSLR
#' @param object is the object
#' @param x is the dataset
#' @param ... This parameter is included for compatibility reasons.
#' @method predict EMNearestMeanClassifierSSLR
#' @importFrom stats predict
#' @importFrom magrittr %>%
predict.EMNearestMeanClassifierSSLR <- function(object, x, ...) {

  result <- object$model %>% predict(x)

  result
}



