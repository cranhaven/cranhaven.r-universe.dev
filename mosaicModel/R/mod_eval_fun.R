#' Internal functions for evaluating models
#' 
#' These functions are the interface to the various model types for `mod_eval()`, and through 
#' that to all the other `mod_` functions that need to evaluate models, e.g. `mod_effect()`, `mod_cv()`, and so on.
#' 
#' @param model A model object of the classes permitted
#' @param data Usually, a data table specifying the inputs to the model. But if
#' not specified, the training data will be used.
#' @param interval One of "none", "confidence", or "prediction". Not all model
#' types support "prediction" or even "confidence".
#' @param ... additional arguments
#' 
#' 
#' @details All of the `eval_` functions are ex
#' These functions return a numerical vector (for regression types) or
#' a matrix of probabilities (for classifiers)
#' @export
mod_eval_fun <- function(model, data=NULL, interval="none", ...) {
  UseMethod("mod_eval_fun")
}
#' @export
mod_eval_fun.default <- function(model, data=NULL, interval="none", ...) {
  stop("The modelMosaic package doesn't have access to an evaluation function for this kind of model object.")
}
#' @export 
mod_eval_fun.lm <- function(model, data=NULL, interval="none", ...) {
  interval <- match.arg(interval, c("none", "confidence", "prediction"))
  
  if (is.null(data)) data <- data_from_mod(model)
  
  res <- as.data.frame(
    predict(model, newdata = data, type = "response", interval = interval )
  )
  
  if (interval == "none" || ncol(res) == 1)
    names(res) <- "model_output"
  else
    names(res) <- c("model_output", "lower", "upper")
  
  tibble::remove_rownames(res)
}

#' @export
mod_eval_fun.randomForest <- function(model, data = NULL, interval="none", ...) {
  interval <- match.arg(interval,
                        choices = c("none", "confidence", "prediction"))
  
  if (is.null(data)) data <- data_from_mod(model)
  
  if (model$type == "classification") {
    res <- tibble::remove_rownames(
      as.data.frame(
        predict(model, newdata = data, type = "prob")))
  } else if (model$type == "regression") {
    res <- data.frame(model_output = 
      predict(model, newdata = data, type = "response"))
  }
  
  res
}

#' @export
mod_eval_fun.glm <- function(model, data=NULL, interval="none", ...) {
  interval <- match.arg(interval, choices = c("none", "confidence"))
  
  if (is.null(data)) data <- data_from_mod(model) 
  
  vals <- predict(model, newdata = data, 
                  type = "link", se.fit = interval == "confidence")
  
  
  
  if (interval == "confidence") {
    res <- data.frame(model_output = model$family$linkinv(vals$fit),
                      lower = vals$fit - 2 * vals$se.fit,
                      upper = vals$fit + 2 * vals$se.fit)
    res$lower <- model$family$linkinv(res$lower)
    res$upper <- model$family$linkinv(res$upper)
  } else {
    names(tmp) <- NULL # strip off case labels
    res <- data.frame(model_output = model$family$linkinv(vals)) 
  }
  
  
  tibble::remove_rownames(res)
}

#' @export
mod_eval_fun.rpart <- function(model, data = NULL, interval = "none", ...) {
  interval <- match.arg(interval, choices = c("none"))
  
  if (is.null(data)) data <- data_from_mod(model) 
  
  if (model$method == "class") { # classifier
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob" )
    )
  } else {
    res <- as.data.frame(
      predict(model, newdata = data)
    )
    names(res) <- "model_output"
  }
  
  tibble::remove_rownames(res)
}

#' @export
mod_eval_fun.randomForest <- function(model, data = NULL, interval = "none", ...) {
  interval <- match.arg(interval, choices = c("none"))
  
  if (is.null(data)) data <- data_from_mod(model) 
  
  if (model$type == "classification") { # classifier
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob" )
    )
  } else {
    res <- as.data.frame(
      predict(model, newdata = data)
    )
    names(res) <- "model_output"
  }
  
  tibble::remove_rownames(res)
}

#' @export
mod_eval_fun.knn3 <- function(model, data = NULL, interval = "none", ...) {
  interval <- match.arg(interval, choices = c("none"))
  
  if (is.null(data)) data <- data_from_mod(model) 
  
  res <- as.data.frame(
      predict(model, newdata = data, type = "prob" )
  )
  
  tibble::remove_rownames(res)
}

#' @export
mod_eval_fun.train <- function(model, data = NULL, interval = "none", ...) { # caret-package
  interval <- match.arg(interval, choices = c("none"))
  
  if (is.null(data)) data <- data_from_mod(model) 
  
  if (model$modelInfo$type[1] == "Regression") { 
    res <- as.data.frame(
      predict(model, newdata = data, type = "raw")
    )
    names(res) <- "model_output"
  } else if (model$modelInfo$type[1] == "Classification") {
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob" ))
  } else {
    stop("Caret model is neither classifier nor regression. mosaicModel doesn't know what to do.")
  }
  
  tibble::remove_rownames(res)
}

#' @export
mod_eval_fun.lda <- function(model, data = NULL, interval = "none", ...) {
  if (is.null(data)) data <- data_from_mod(model)
  
  res <- as.data.frame(predict(model, newdata = data)$posterior)

  tibble::remove_rownames(res)
}
#' @export
mod_eval_fun.qda <- mod_eval_fun.lda
