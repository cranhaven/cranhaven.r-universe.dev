#' Function for cleaning the data of a model with class
#'
#' The purpose of this function is to prepare the dataset and the counter-factual
#' model for estimation and is the first step pf the pscfit.R process. The output
#' is a complete-case dataset where the data names match the variables used in the CFM.
#'
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param id to specify which observations in the data cohort should be evaluated.
#' Defualts to 'NULL' i.e all observations
#' @param trt used to specify multiple treatment effects. Defaults to NULL
#' @param cfmOb used to specify if a CFM object is supplies as the counter factual model
#' @return a list containing objects which specifiy the required exported components
#'   of the model and a cleaned data cohort.  Attirbutes of the 'cleaned' object include:
#'  Attributes include \itemize{
#'  \item{'model.type' specifying the class of model to be used as the CFM }
#'  \item{'model_extract' sepcifying the model componets required for estimation}
#'  \item{'cov' a cleaned dataset of covariates}
#'  \item{'outcome' a cleaned dataset containing the outcomes}
#'  }
#' @export
dataComb.pscCFM <- function(CFM,DC,id=NULL,trt=NULL,cfmOb=TRUE){
  if("flexsurvreg"%in%CFM$mod_class) ret <- dataComb.flexsurvreg(CFM=CFM,DC=DC,id=id,trt=trt,cfmOb=TRUE)
  if("glm"%in%CFM$mod_class) ret <- dataComb.glm(CFM=CFM,DC=DC,id=id,trt=trt,cfmOb=TRUE)
  return(ret)
}
