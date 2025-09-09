#' Estimates the linear predictor of a psc object
#'
#' A function which created a linear predictor from a model and an external
#' dataset.  If required, linear predictors will be provided on the response
#' scale.  For a survival outcome, this will take the form of median survival
#' estimates.
#'
#' @param DC_clean a cleaned data obhject created using dataComb()
#' @param resp detailing whether the linear predictor shoudl be returned on the natural or response level.  Defaults to the natural scale (resp=F)
#' @details A function which combines the data from the data cohort against the model parameters of the PSC
#' @return Extracts the linear predictor from a object containing both a counter factual model and a data cohort which is created using the dataComb() fucntion.
#' @export
linPred <- function(DC_clean,resp=FALSE){

  mt <- DC_clean$model_extract$mod_class
  cov <- DC_clean$cov;cov

  ## Removing 'trt' from dataset
  if("trt"%in%colnames(cov)){
    cov <- cov[,-which(colnames(cov)=="trt")]
  }

  cov_co <- DC_clean$model_extract$cov_co;cov_co
  lp <- cov %*% cov_co
  ret <- lp

  if(resp){
    if("glm"%in%mt){
      fam <- enrich(DC_clean$model_extract$family)
      ret <- fam$linkinv(lp)
    }

    if("flexsurvreg"%in%mt){
      mnlp <- mean(lp);mnlp
      ret <- surv_fpm(DC_clean,s=0.5)^exp(mnlp-lp);ret
    }
  }
  return(ret)
}
