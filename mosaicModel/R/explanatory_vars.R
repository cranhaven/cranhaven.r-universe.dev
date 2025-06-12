#' Get the names of the explanatory or response variables in a model
#' 
#' This will typically *not* be used by the end_user. These functions let you
#' interrogate any model architecture that's covered by mosaicModel about the response and
#' explanatory variables. These are used internally in functions such as `mod_plot`.
#' 
#' 
#' @param model the model in question
#' @param ... (not used)
#' 
explanatory_vars <- function(model, ...) {
  all.vars(formula_from_mod(model)[[3]]) 
}

response_var <- function(model, ...) {
  formula_from_mod(model)[[2]]
}

response_values <- function(model, ...) { 
  eval(parse(text = response_var(model)), envir = data_from_mod(model))
}

