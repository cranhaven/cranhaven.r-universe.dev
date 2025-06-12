#' Extract training data from model
#' 
#' This typically will *not* be used by an end-user.
#' 
#' @param model the model from which to extract the training data
#' @param ... additional arguments (not used)
#' 
#' @details not all model architectures keep track of the training data
#' If a model architecture isn't recognized, you'll have to add a method for that class. See vignette.
#' @export
data_from_mod <- function(model, ...) {
  UseMethod("data_from_mod")
}


#' @export
data_from_mod.bootstrap_ensemble <- function(model, ...) data_from_mod(model$original_model, ...)
#' @export
data_from_mod.default <- function(model, ...) {
  error_string <- paste0("Model architecture '",
                         paste(class(model), collapse = "', "),
                         "'not recognized by mosaicModel.")
  if ( ! "call" %in% names(model) || !"data" %in% names(model$call))
    stop(error_string)
  data_in_call <- which("data" == names(model$call))
  if (length(data_in_call) == 1) {
    the_data <- eval(model$call[[data_in_call]], envir = parent.frame(3))
    if (is.data.frame(the_data)) return(the_data)
  } 
  stop(error_string)
}

#' @export
data_from_mod.knn3 <- function(model, ...) {
  res <- data.frame(model$learn$y, model$learn$X)
  names(res)[1] <- as.character(response_var(model))
  
  res
}