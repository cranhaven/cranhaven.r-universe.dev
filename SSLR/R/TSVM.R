#' @title General Interface for TSVM (Transductive SVM classifier using the convex concave procedure) model
#' @description model from RSSL package
#' Transductive SVM using the CCCP algorithm as proposed by Collobert et al. (2006)
#' implemented in R using the quadprog package. The implementation does not handle large
#' datasets very well, but can be useful for smaller datasets and visualization purposes.
#' C is the cost associated with labeled objects, while Cstar is the cost for the
#' unlabeled objects. s control the loss function used for the unlabeled objects: it
#' controls the size of the plateau for the symmetric ramp loss function. The balancing
#' constraint makes sure the label assignments of the unlabeled objects are similar to the
#' prior on the classes that was observed on the labeled data.
#' @param C numeric; Cost parameter of the SVM
#' @param Cstar numeric; Cost parameter of the unlabeled objects
#' @param s numeric; parameter controlling the loss function of the unlabeled objects (generally values between -1 and 0)
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param verbose logical; print debugging messages, only works for vanilladot() kernel (default: FALSE)
#' @param balancing_constraint logical; Whether a balancing constraint should be enfored that causes the fraction of objects assigned to each label in the unlabeled data to be similar to the label fraction in the labeled data.
#' @param max_iter integer; Maximum number of iterations
#' @inheritParams RSSL::BaseClassifier
#' @references Collobert, R. et al., 2006. Large scale transductive SVMs.
#' Journal of Machine Learning Research, 7, pp.1687-1712.
#' @example demo/TSVM.R
#' @importFrom RSSL TSVM
#' @export
TSVMSSLR <- function(C = 1, Cstar = 0.1, kernel = kernlab::vanilladot(),
                       balancing_constraint = TRUE, s = 0, x_center = TRUE,
                       scale = FALSE, eps = 1e-09, max_iter = 20, verbose = FALSE) {

  train_function <- function(x, y) {

    load_RSSL()

    number_classes <- length(levels(y))

    #Check binary problem
    if (number_classes > 2) {
      stop("TSVMSSLR is for binary problems")
    }

    list_values <- get_x_y_And_unlabeled(x, y)

    model <- RSSL::TSVM(X = list_values$x, y = list_values$y, X_u = list_values$X_u, C = C, Cstar = Cstar, kernel = kernel,
                         balancing_constraint = balancing_constraint, s = s, x_center = x_center,
                         scale = scale, eps = eps, max_iter = max_iter, verbose = verbose)

    result <- list(
      model = model
    )

    result$classes = levels(y)
    result$pred.params = c("class","raw")
    result$mode = "classification"
    class(result) <- "TSVMSSLR"

    return(result)
  }

  args <- list(
    C = C, Cstar = Cstar, kernel = kernel,
    balancing_constraint = balancing_constraint, s = s, x_center = x_center,
    scale = scale, eps = eps, max_iter = max_iter, verbose = verbose
  )

  new_model_sslr(train_function, "TSVMSSLR", args)

}

#' @title Predict TSVMSSLR
#' @param object is the object
#' @param x is the dataset
#' @param ... This parameter is included for compatibility reasons.
#' @importFrom stats predict
#' @importFrom magrittr %>%
#' @method predict TSVMSSLR
predict.TSVMSSLR <- function(object, x, ...) {

  result <- object$model %>% predict(x)

  result
}



