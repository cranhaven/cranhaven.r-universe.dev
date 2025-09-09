#' Function for cleaning the data of a model with class 'flexsurvreg'
#'
#' The purpose of this function is to prepare the dataset and the counter-factual
#' model for estimation and is the first step pf the pscfit.R process. The output
#' is a complete-case dataset where the data names match the variables used in the CFM.
#'
#' @param CFM a model object supplied to pscfit
#' @param DC a dataset including covariates to match the CFM
#' @param id a vector specifiying whether a subset of the dataset should be selected.
#'   Defaults to 'NULL' e.g. all data points included
#' @param trt An optional additional vector denoting treatment allocations for multiple treatment comparisons.  Defaults to 'NULL'
#' @param cfmOb used to specify if a CFM object is supplies as the counter factual model
#' @return a list containing objects which specifiy the required exported components
#'   of the model and a cleaned data cohort.
#'  Attributes include \itemize{
#'  \item{'model.type' specifying the class of model to be used as the CFM }
#'  \item{'model_extract' sepcifying the model componets required for estimation}
#'  \item{'cov' a cleaned dataset of covariates}
#'  \item{'outcome' a cleaned dataset containing the outcomes}
#'  }
#' @export
dataComb.flexsurvreg <- function(CFM,DC,id=NULL,trt=NULL,cfmOb=FALSE){

  model_extract <- CFM
  ### removing response and weights
  if(!cfmOb) model_extract <- modelExtract(CFM);model_extract

  ### Getting term names (and removing outcome and 'weights')
  term.nm <- model_extract$terms
  out.nm <- term.nm[1]
  term.nm <- term.nm[-c(1,length(term.nm))];term.nm

  ### ERROR CHECK: Selecting data from DC
  data_unavail_id  <- which(!term.nm%in%names(DC))
  data_unavail <- term.nm[data_unavail_id]
  if(length(data_unavail_id)!=0) stop(paste("Covariate '",data_unavail,"' is included in the model but not the dataset",sep=""))

  ### Making sure 'time' and 'cen' are present
  out.nm.trap <- which(names(DC)%in%c("time","cen"))
  if(length(out.nm.trap)!=2) stop("outcome covariates in data cohort should be named 'time' and 'cen'")

  ## Creating model matrix (adding response to covariates)
  DC <- data.frame(cbind(0,DC))
  names(DC)[1] <- out.nm
  DC[,1] <- Surv(DC$time,DC$cen)

  ### Adding treatment variable (if not null)
  if(!is.null(trt)) {
    if("trt"%in%names(DC)){
      DC <- DC[,-which(names(DC)=="trt")]
    }
    term.nm <- c(term.nm,"trt")
  }

  #### Selecting subgroup (if 'id' is specified)
  if(!is.null(id)){
    DC <- DC[id,]
    trt <- trt[id]
  }

  ### Removing missing data
  miss.cov <- which(is.na(DC),arr.ind=T)[,1]
  miss.trt <- which(is.na(trt))
  miss.id <- union(miss.cov,miss.trt)

  if(length(miss.id)>0) {
    DC <- DC[-miss.id,]
    trt <- trt[-miss.id]
    warning(paste(length(miss.id),"rows removed due to missing data in dataset"))
  }

  ## Matching data between DC and CFM
  cls <- model_extract$cov_class;cls
  lev <- model_extract$cov_lev;lev
  DCcov <- data_match(cls,lev,DC);DCcov[1:4,];trt[1:4]


  ### Creating model matrix based on new dataset
  out.ob <- DCcov[,1];out.ob
  out <- data.frame("time"=out.ob[,1],"cen"=out.ob[,2]);out[1:3,]
  DCcov <- cbind(DCcov,out)
  dc_mm <- model.matrix(model_extract$formula,data=DCcov)[,-1]

  ### Adding in 'trt' (if required)
  if(!is.null(trt)) dc_mm <- cbind(dc_mm,"trt"=trt)
  ret <- list("model_extract"=model_extract,"cov"=dc_mm,"outcome"=out)
  ret

}
