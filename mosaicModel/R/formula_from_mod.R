#' Extract the model formula used in specifying the model
#' 
#' This typically will *not* be used by an end-user.
#' 
#' @param model the model
#' @param ... (not used)
#' 
#' @details Not all model architectures support this. If a model architecture isn't recognized,
#' you'll have to add a method for that class. See vignette.
#' @export
formula_from_mod <- function(model, ...) {
  UseMethod("formula_from_mod")
}
#' @export
formula_from_mod.default <- function(model, ...) {
  if ("terms" %in% names(model)) return(formula(model$terms))
  if ("Terms" %in% names(model)) return(formula(model$Terms))
  stop("Model architecture '", class(model), "' not recognized by mosaicModel package.")
}
#' @export
formula_from_mod.bootstrap_ensemble <- function(model, ...) {
  formula_from_mod(model$original_model)
}
