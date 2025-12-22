#' A function which structures the Data Cohort in a format for model estimation
#'
#' This function ensures the data are supplied in a structure which allows for
#' estimation.  This is performed by re-fitting the original CFM with the DC and
#' extracting the appropriate structures.  Data are returned in terms of "Y" for
#' model outcomes, "X" for data and "Z" for random effects where mixed models
#' are supplied.
#'
#' @param CFM a Counter Factual Model
#' @param DC a Data Cohort object
#' @param id to be specified for subgroup analysis. Defaults to NULL
#' @param trt to be specified for multiple treatment comparisons. Defaults to
#' NULL
#' @return A set of structures for use with estimation procedures
#' @importFrom survival Surv
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' pscOb <- pscData(gemCFM,e4_data)
#' @export
pscData <- function(CFM,DC,id=NULL,trt=NULL){

  ### Getting term names
  term.nm <- CFM$terms;term.nm
  out.nm <- CFM$out.nm;out.nm
  term.nm <- setdiff(term.nm,out.nm)

  ### ERROR CHECK: Selecting data from DC
  pscData_error(term.nm,DC)
  pscData_error(out.nm,DC)

  ### Creating versions of the dataset with covariates and outcomes
  if("flexsurvreg"%in%CFM$mod_class){
    DCcov <- DC[,which(names(DC)%in%term.nm)]
    DCout <- DC[,which(names(DC)%in%out.nm)];DCout
    ## Matching data between DC and CFM
    DC <- pscData_match(CFM$cov_class,CFM$cov_lev,DCcov)
    DC <- cbind(DCout,DC)
  }

  if("glm"%in%CFM$mod_class){
    DC <- pscData_match(CFM$cov_class,CFM$cov_lev,DC)
  }


  ## Add additional trt variable (if supplied)
  if(!is.null(trt)){
    DCtrt <- pscData_addtrt(DC,trt)
    DC <- DCtrt[[1]]
    trt.nm <- DCtrt[[2]]
  }

  #########
  #### Selecting subgroup (if 'id' is specified)
  if(!is.null(id)){
    DC <- DC[id,]
  }

  ### Removing missing data
  DC <- pscData_miss(DC)

  ### Getting required data structures ... Adding in 'trt' (if required)
  struc <- pscData_structure(CFM,DC)

  if(!is.null(trt)) struc$trt <- DC$trt
  CFM$DC <- struc

  ### Changing data summary to comparison
  CFM$datavis <- visComp(CFM,DC)

  ### Adding Likelihood
  lik <- pscData_addLik(CFM)
  CFM$lik <- lik

  ## adding in Treatment and id info
  CFM$trt <- trt
  CFM$id <- id

  ### setting class
  class(CFM) <- "pscOb"

  ### returning object
  return(CFM)
}
