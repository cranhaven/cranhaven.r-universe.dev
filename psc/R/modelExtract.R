#' A generic function for extracting model information
#' @param CFM a model of class either 'glm' or 'flexsurvreg'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components
#' @export
modelExtract <- function(CFM){
  UseMethod("modelExtract")
}

