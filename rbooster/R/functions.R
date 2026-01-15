#' Functions to be used internally
#'
#' @description for internal use
#'
#' @param x_train input features.
#' @param y_train factor class variable.
#' @param weights instance weights.
#' @param model model obtained from respective classifier.
#' @param x_new new features for prediction.
#' @param ... other control parameters.
#'
#' @return Classifiers produce an object which is appropriate
#' for respective predictor. Predictors returns class
#' predictions for \code{x_new}.
#'
#' @rdname fun
#' @keywords internal
#' @export

classifier_rpart <- function(x_train, y_train, weights, ...) {
  model <- rpart::rpart(formula = y_train~.,
                        data = data.frame(x_train, y_train),
                        weights = weights,
                        control = rpart::rpart.control(minsplit = -1,
                                                       maxcompete = 0,
                                                       maxsurrogate = 0,
                                                       usesurrogate = 0),
                        ...)
  return(model)
}


#'
#' @rdname fun
#' @keywords internal
#' @export

predictor_rpart <- function(model, x_new, type = "pred", ...) {
  x_new <- as.data.frame(x_new)
  if (type == "pred") {
    preds <- predict(object = model, newdata = x_new, type = "class", ...)
  }
  if (type == "prob") {
    preds <- predict(object = model, newdata = x_new, type = "prob", ...)
  }
  return(preds)
}


#' @rdname fun
#' @keywords internal
#' @export

classifier_glm <- function(x_train, y_train, weights, ...){
  model <- suppressWarnings(stats::glm(y_train~., family = "binomial",
                                       data = data.frame(x_train, y_train),
                                       weights = weights, ...))
  return(model)
}

#' @rdname fun
#' @keywords internal
#' @export

predictor_glm <- function(model, x_new, type = "pred", ...) {
  dat <- model$data
  y <- dat[,ncol(dat)]
  class_names <- levels(y)
  class_pos <- names(which.min(table(y)))
  class_neg <- as.character(class_names[class_names != class_pos])

  x_new <- as.data.frame(x_new)
  probs_pos <- stats::predict.glm(object = model, newdata = x_new, type = "response", ...)

  if (type == "prob") {
    probs_neg <- 1 - probs_pos

    probs <- data.frame(probs_pos, probs_neg)
    colnames(probs) <- c(class_pos, class_neg)
    probs <- probs[,class_names]
    return(probs)
  }

  if (type == "pred") {
    preds <- factor(ifelse(probs_pos > 0.5, class_pos, class_neg),
                    levels = class_names, labels = class_names)
    return(preds)
  }
}

#' @rdname fun
#' @keywords internal
#' @export

classifier_gnb <- function(x_train, y_train, weights, ...){
  model <- w_naive_bayes(x_train = x_train, y_train = y_train, discretize = FALSE, w = weights)
  return(model)
}

#' @rdname fun
#' @keywords internal
#' @export

predictor_gnb <- function(model, x_new, type = "pred", ...) {
  if (type == "pred") {
    preds <- predict.w_naive_bayes(object = model, newdata = x_new,
                                   type = "pred", ...)  }
  if (type == "prob") {
    preds <- predict.w_naive_bayes(object = model, newdata = x_new,
                                   type = "prob", ...)  }
  return(preds)
}

#' @rdname fun
#' @keywords internal
#' @export

classifier_dnb <- function(x_train, y_train, weights, ...){
  model <- w_naive_bayes(x_train = x_train, y_train = y_train, discretize = TRUE, w = weights, ...)
  return(model)
}

#' @rdname fun
#' @keywords internal
#' @export

predictor_dnb <- function(model, x_new, type = "pred", ...) {
  if (type == "pred") {
    preds <- predict.w_naive_bayes(object = model, newdata = x_new,
                                   type = "pred", ...)  }
  if (type == "prob") {
    preds <- predict.w_naive_bayes(object = model, newdata = x_new,
                                   type = "prob", ...)  }
  return(preds)
}

#' @rdname fun
#' @keywords internal
#' @export

classifier_earth <- function(x_train, y_train, weights, ...){
  model <- earth::earth(x = x_train, y = y_train, weights = weights, nk = 4,
                        ...)
  return(model)
}

#' @rdname fun
#' @keywords internal
#' @export

predictor_earth <- function(model, x_new, type = "pred", ...) {
  dat <- model$data
  y <- dat[,ncol(dat)]
  class_names <- levels(y)
  class_pos <- names(which.min(table(y)))
  class_neg <- as.character(class_names[class_names != class_pos])

  probs_pos <- predict(object = model, newdata = x_new, type = "response", ...)

  if (type == "prob") {
    probs_neg <- 1 - probs_pos

    probs <- data.frame(probs_pos, probs_neg)
    colnames(probs) <- c(class_pos, class_neg)
    probs <- probs[,class_names]
    return(probs)
  }

  if (type == "pred") {
    preds <- predict(object = model, newdata = x_new,
                     type = "class", ...)
  }
  return(preds)
}
