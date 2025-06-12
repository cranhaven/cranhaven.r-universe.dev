#' Construct a call for refitting a model from the model itself
#' 
#' This will typically *not* be used by the end-user.
#' 
#' @param model the model in question
#' @param data_name character string specifying the name of the data 
#' frame used for the refitting. This object *must* be defined in the environment in which the 
#' call is being made.
#
#' @param ... (not used)
#' 
#' 
#' @details This provides a way to refit a model on either resampled or sub-sampled data. 
#' Not all model architectures support this. If not, then you can't use `mod_ensemble` or `mod_cv`, 
#' or use the `bootstrap=` argument in any of the other functions.
#' 
#' @export
construct_fitting_call <- function(model, data_name = "training", ...) {
  UseMethod("construct_fitting_call")
}

#' @export
construct_fitting_call.default <- function(model, data_name, ...) {
  # set up the call for fitting the model to the training data
  if (! "call" %in% names(model))
    stop("No 'call' component to model, so the model can't be retrained.")
  architecture <- model$call[[1]]
  fit_call <- model$call
  if (data_name != "") fit_call[["data"]] <- as.name(data_name)
  
  fit_call
}

#' @export
construct_fitting_call.knn3 <- function(model, data_name = "training", ...) {
  res <- call("knn3")
  formula <- model$terms
  attributes(formula) <- NULL
  res[[2]] <- formula
  res[["data"]] <- as.name(data_name)
  res[["k"]] <- model$k
  
  res
}
