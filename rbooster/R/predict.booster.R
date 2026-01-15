#' Prediction function for Adaboost framework
#'
#' Makes predictions based on booster function
#'
#' @param object booster object
#' @param newdata a factor class variable. Boosting algorithm allows for
#' @param type pre-ready or a custom classifier function.
#' @param print_detail prints the prediction process. Default is \code{FALSE}.
#' @param ... additional arguments.
#'
#' @details
#' Type "pred" will give class predictions. "prob" will give probabilities for
#' each class.
#'
#' @return A vector of class predictions or a matrix of class probabilities
#' depending of \code{type}
#'
#' @seealso [predict()]
#'
#' @rdname predict.booster
#' @export

predict.booster <- function(object, newdata, type = "pred", print_detail = FALSE, ...){

  pp <- get(paste0("predict.", object$method, "_adaboost"))
  results <- pp(object = object, newdata = newdata, type = type, print_detail = print_detail, ...)
  return(results)

}


#' @rdname predict.booster
#' @export
predict.discrete_adaboost <- function(object, newdata, type = "pred", print_detail = FALSE, ...){

  n_train <- object$n_train
  w <- object$w
  alpha <- object$alpha
  models <- object$models
  selected_vars <- object$selected_vars
  x_classes <- object$x_classes
  n_classes <- object$n_classes
  k_classes <- object$k_classes
  class_names <- object$class_names
  predictor <- object$predictor

  x_test <- newdata
  n_test <- nrow(x_test)

  posteriors_all <- matrix(NA, nrow = n_test, ncol = k_classes)

  fit_test <- matrix(0, nrow = n_test, ncol = k_classes)
  y_codes <- c(-1/(k_classes - 1), 1)

  for (i in 1:length(models)) {
    preds <- predictor(models[[i]], x_test[,unlist(selected_vars[i,]), drop = FALSE])
    preds_num <- sapply(class_names, function(m) y_codes[as.numeric(preds == m) + 1])

    fit_test <- fit_test + alpha[i]*preds_num
    if (print_detail) {
      cat("\r",
          "%",
          formatC(i/length(models)*100, digits = 2, format = "f"),
          " |",
          rep("=", floor(i/length(models)*20)),
          rep("-", 20 - ceiling(i/length(models)*20)),
          "|   ", sep = "")
    }
  }
  if (print_detail) {
    cat("\n")
  }

  posteriors <- 1/(1 + exp(-fit_test))
  posteriors[is.infinite(as.matrix(posteriors))] <- .Machine$double.xmax
  posteriors <- posteriors/apply(posteriors, 1, sum)

  colnames(posteriors) <- class_names

  if (type == "prob") {
    return(posteriors)
  }
  if (type == "pred") {
    predictions <- factor(class_names[apply(fit_test, 1, which.max)], levels = class_names, labels = class_names)
    return(predictions)
  }
}

#' @rdname predict.booster
#' @export
predict.real_adaboost <- function(object, newdata, type = "pred", print_detail = FALSE, ...){

  n_train <- object$n_train
  w <- object$w
  alpha <- object$alpha
  models <- object$models
  selected_vars <- object$selected_vars
  x_classes <- object$x_classes
  n_classes <- object$n_classes
  k_classes <- object$k_classes
  class_names <- object$class_names
  predictor <- object$predictor

  x_test <- newdata
  n_test <- nrow(x_test)

  posteriors_all <- matrix(NA, nrow = n_test, ncol = k_classes)

  fit_test <- matrix(0, nrow = n_test, ncol = k_classes)
  y_codes <- c(-1/(k_classes - 1), 1)

  for (i in 1:length(models)) {
    probs <- predictor(models[[i]], x_test[,unlist(selected_vars[i,]), drop = FALSE], type = "prob")
    probs_fit <- (probs*(1 - 2*1e-5) + 1e-5)
    log_probs_fit <- log(probs_fit)

    fit <- (k_classes - 1)*(log_probs_fit - (1/k_classes)*rowSums(log_probs_fit))
    fit_test <- fit_test + fit

    if (print_detail) {
      cat("\r",
          "%",
          formatC(i/length(models)*100, digits = 2, format = "f"),
          " |",
          rep("=", floor(i/length(models)*20)),
          rep("-", 20 - ceiling(i/length(models)*20)),
          "|   ", sep = "")
    }
  }
  if (print_detail) {
    cat("\n")
  }

  posteriors <- 1/(1 + exp(-fit_test))
  posteriors[is.infinite(as.matrix(posteriors))] <- .Machine$double.xmax
  posteriors <- posteriors/apply(posteriors, 1, sum)

  colnames(posteriors) <- class_names

  if (type == "prob") {
    return(posteriors)
  }
  if (type == "pred") {
    predictions <- factor(class_names[apply(fit_test, 1, which.max)], levels = class_names, labels = class_names)
    return(predictions)
  }
}
