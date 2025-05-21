#' @title General Interface for EMLeastSquaresClassifier model
#' @description model from RSSL package
#'
#' An Expectation Maximization like approach to Semi-Supervised Least Squares Classification
#'
#' As studied in Krijthe & Loog (2016), minimizes the total loss of the labeled and unlabeled objects by finding the weight vector and labels that minimize the total loss. The algorithm proceeds similar to EM, by subsequently applying a weight update and a soft labeling of the unlabeled objects. This is repeated until convergence.
#'
#' By default (method="block") the weights of the classifier are updated, after which the unknown labels are updated. method="simple" uses LBFGS to do this update simultaneously. Objective="responsibility" corresponds to the responsibility based, instead of the label based, objective function in Krijthe & Loog (2016), which is equivalent to hard-label self-learning.
#'
#' @param scale Should the features be normalized? (default: FALSE)
#' @param eps Stopping criterion for the minimization
#' @param verbose logical; Controls the verbosity of the output
#' @param alpha numeric; the mixture of the new responsibilities and the old in each iteration of the algorithm (default: 1)
#' @param method character; one of "block", for block gradient descent or "simple" for LBFGS optimization (default="block")
#' @param objective character; "responsibility" for hard label self-learning or "label" for soft-label self-learning
#' @param init objective character; "random" for random initialization of labels, "supervised" to use supervised solution as initialization or a numeric vector with a coefficient vector to use to calculate the initialization
#' @param max_iter integer; maximum number of iterations
#' @param beta numeric; value between 0 and 1 that determines how much to move to the new solution from the old solution at each step of the block gradient descent
#' @param save_all logical; saves all classifiers trained during block gradient descent
#' @inheritParams RSSL::BaseClassifier
#' @references Krijthe, J.H. & Loog, M., 2016. Optimistic Semi-supervised Least Squares Classification. In International Conference on Pattern Recognition (To Appear).
#' @example demo/EMLeastSquaresClassifier.R
#' @importFrom RSSL EMLeastSquaresClassifier
#' @export
EMLeastSquaresClassifierSSLR <- function(x_center = FALSE, scale = FALSE,
                                          verbose = FALSE, intercept = TRUE, lambda = 0, eps = 1e-09,
                                          y_scale = FALSE, alpha = 1, beta = 1, init = "supervised",
                                          method = "block", objective = "label", save_all = FALSE,
                                          max_iter = 1000) {

  train_function <- function(x, y) {

    load_RSSL()

    number_classes <- length(levels(y))

    #Check binary problem
    if (number_classes > 2) {
      stop("EMLeastSquaresClassifierSSLR is for binary problems")
    }

    list_values <- get_x_y_And_unlabeled(x, y)

    model <- RSSL::EMLeastSquaresClassifier(X = list_values$x, y = list_values$y, X_u = list_values$X_u,
                                             x_center = x_center, scale = scale,
                                             verbose = verbose, intercept = intercept, lambda = lambda, eps = eps,
                                             y_scale = y_scale, alpha = alpha, beta = beta, init = init,
                                             method = method, objective = objective, save_all = save_all,
                                             max_iter = max_iter)

    result <- list(
      model = model
    )

    result$classes = levels(y)
    result$pred.params = c("class","raw")
    result$mode = "classification"
    class(result) <- "EMLeastSquaresClassifierSSLR"

    return(result)
  }

  args <- list(
    x_center = x_center, scale = scale,
    verbose = verbose, intercept = intercept, lambda = lambda, eps = eps,
    y_scale = y_scale, alpha = alpha, beta = beta, init = init,
    method = method, objective = objective, save_all = save_all,
    max_iter = max_iter
  )

  new_model_sslr(train_function, "EMLeastSquaresClassifierSSLR", args)

}


#' @title Predict EMLeastSquaresClassifierSSLR
#' @param object is the object
#' @param x is the dataset
#' @param ... This parameter is included for compatibility reasons.
#' @method predict EMLeastSquaresClassifierSSLR
#' @importFrom magrittr %>%
predict.EMLeastSquaresClassifierSSLR <- function(object, x, ...) {

  result <- object$model %>% predict(x)

  result
}



