#' @title General Interface for WellSVM model
#' @description model from RSSL package
#' WellSVM is a minimax relaxation of the mixed integer programming problem of finding the
#' optimal labels for the unlabeled data in the SVM objective function.
#' This implementation is a translation of the Matlab implementation of Li (2013) into R.
#' @param C1 double; A regularization parameter for labeled data, default 1;
#' @param C2 double; A regularization parameter for unlabeled data, default 0.1;
#' @param gamma double; Gaussian kernel parameter, i.e., k(x,y) = exp(-gamma^2||x-y||^2/avg) where avg is the average distance among instances; when gamma = 0, linear kernel is used. default gamma = 1;
#' @param max_iter integer; Maximum number of iterations
#' @inheritParams RSSL::BaseClassifier
#' @references Y.-F. Li, I. W. Tsang, J. T. Kwok, and Z.-H. Zhou. Scalable and Convex Weakly Labeled SVMs. Journal of Machine Learning Research, 2013.
#' @references R.-E. Fan, P.-H. Chen, and C.-J. Lin. Working set selection using second order information for training SVM. Journal of Machine Learning Research 6, 1889-1918, 2005.
#' @example demo/WellSVM.R
#' @importFrom RSSL WellSVM
#' @export
WellSVMSSLR <- function(C1 = 1, C2 = 0.1, gamma = 1, x_center = TRUE,
                     scale = FALSE, use_Xu_for_scaling = FALSE, max_iter = 20) {

  train_function <- function(x, y) {

    load_RSSL()

    number_classes <- length(levels(y))

    #Check binary problem
    if (number_classes > 2) {
      stop("WellSVMSSLR is for binary problems")
    }

    list_values <- get_x_y_And_unlabeled(x, y)

    model <- RSSL::WellSVM(X = list_values$x, y = list_values$y, X_u = list_values$X_u, C1 = C1, C2 = C2, gamma = gamma, x_center = x_center,
            scale = scale, use_Xu_for_scaling = use_Xu_for_scaling, max_iter = max_iter)

    result <- list(
      model = model
    )

    result$classes = levels(y)
    result$pred.params = c("class","raw")
    result$mode = "classification"
    class(result) <- "WellSVMSSLR"

    return(result)
  }

  args <- list(
    C1 = C1, C2 = C2, gamma = gamma, x_center = x_center,
    scale = scale, use_Xu_for_scaling = use_Xu_for_scaling, max_iter = max_iter
  )

  new_model_sslr(train_function, "WellSVMSSLR", args)

}

#' @title Predict WellSVMSSLR
#' @param object is the object
#' @param x is the dataset
#' @param ... This parameter is included for compatibility reasons.
#' @importFrom stats predict
#' @importFrom magrittr %>%
#' @method predict WellSVMSSLR
predict.WellSVMSSLR <- function(object, x, ...) {

  result <- object$model %>% predict(x)

  result
}



