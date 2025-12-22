#' Creating a CFM model which can be shared
#'
#' Standard R model objects contain within them the datasets used to create the
#' model and as such care is needed when sharing these objects for research.
#' The psc.cfm function creates an object with all identifiable information
#' retracted and includes only the information required to use the models within
#' the psc package
#'
#'
#' @param CFM a 'glm' or 'flexsurvreg' model object
#' @param dataSumm a logical indicator specifying whether a summary of the data
#' should be provided, defaults to TRUE.
#' @param dataVis a logical indicator specifying whether a visualisations of the
#' data should be provided, defaults to TRUE.
#' @return a list containing objects which specify the required exported components
#'   of the model.
#' @importFrom survival Surv
#' @export
pscCFM <- function(CFM,dataSumm=T,dataVis=T){

  if(!("pscCFM"%in%class(CFM))){

    me <- modelExtract(CFM)

    if(dataSumm){
      me$datasumm <- cfmDataSumm(CFM)
    }

    if(dataVis){
      me$datavis <- cfmDataVis(CFM)
    }

    class(me) <- c("pscCFM")
    CFM <- me
  }

  return(CFM)

}
