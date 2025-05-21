#' @title General Interface for USMLeastSquaresClassifier (Updated Second Moment Least Squares Classifier) model
#' @description model from RSSL package
#' This methods uses the closed form solution of the supervised least squares problem,
#' except that the second moment matrix (X'X) is exchanged with a second moment matrix that
#' is estimated based on all data. See for instance \cite{Shaffer1991}, where in this
#' implementation we use all data to estimate E(X'X), instead of just the labeled data.
#' This method seems to work best when the data is first centered \code{x_center=TRUE}
#' and the outputs are scaled using \code{y_scale=TRUE}.
#' @param lambda numeric; L2 regularization parameter
#' @inheritParams RSSL::BaseClassifier
#' @references Shaffer, J.P., 1991. The Gauss-Markov Theorem and Random Regressors. The American Statistician, 45(4), pp.269-273.
#' @example demo/USMLeastSquaresClassifier.R
#' @importFrom RSSL USMLeastSquaresClassifier
#' @export
USMLeastSquaresClassifierSSLR <- function(lambda = 0, intercept = TRUE,
                                           x_center = FALSE, scale = FALSE, y_scale = FALSE, ...,
                                           use_Xu_for_scaling = TRUE) {

  train_function <- function(x, y) {

    load_RSSL()

    number_classes <- length(levels(y))

    #Check binary problem
    if (number_classes > 2) {
      stop("USMLeastSquaresClassifierSSLR is for binary problems")
    }

    list_values <- get_x_y_And_unlabeled(x, y)

    model <- RSSL::USMLeastSquaresClassifier(X = list_values$x, y = list_values$y, X_u = list_values$X_u,
                                              lambda = lambda, intercept = intercept,
                                              x_center = x_center, scale = scale,
                                              y_scale = y_scale,
                                              use_Xu_for_scaling = use_Xu_for_scaling)

    result <- list(
      model = model
    )

    result$classes = levels(y)
    result$pred.params = c("class","raw")
    result$mode = "classification"
    class(result) <- "USMLeastSquaresClassifierSSLR"

    return(result)
  }

  args <- list(
    lambda = lambda, intercept = intercept,
    x_center = x_center, scale = scale,
    y_scale = y_scale,
    use_Xu_for_scaling = use_Xu_for_scaling
  )

  new_model_sslr(train_function, "USMLeastSquaresClassifierSSLR", args)

}

#' @title Predict USMLeastSquaresClassifierSSLR
#' @param object is the object
#' @param x is the dataset
#' @param ... This parameter is included for compatibility reasons.
#' @importFrom stats predict
#' @importFrom magrittr %>%
#' @method predict USMLeastSquaresClassifierSSLR
predict.USMLeastSquaresClassifierSSLR <- function(object, x, ...) {

  result <- object$model %>% predict(x)

  result
}



