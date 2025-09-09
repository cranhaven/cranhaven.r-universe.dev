#' Creating a CFM model which can be shared
#'
#' Standard R model objects contain within them the datasets used to create the
#' model and as such care is needed when sharing these objects for research.
#' The psc.cfm function creates an object with all identifiable information
#' retracted and includes only the information required to use the models within
#' the psc package
#'
#'
#' @param cfm a 'glm' or 'flexsurvreg' model object
#' @param dataSumm a logical indicator specifying whether a summary of the data
#' should be provided, defaults to TRUE.
#' @param dataVis a logical indicator specifying whether a visualisations of the
#' data should be provided, defaults to TRUE.
#' @return a list containing objects which specifiy the required exported components
#'   of the model.
#' @export
pscCFM <- function(cfm,dataSumm=T,dataVis=T){
  me <- modelExtract(cfm)

  if(dataSumm){
    me$datasumm <- cfmDataSumm(cfm)
  }

  if(dataVis){
    me$datavis <- cfmDataVis(cfm)
  }

  class(me) <- c("pscCFM")
  return(me)
}
