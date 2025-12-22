#' A generic function for extracting model information
#' @param CFM a model of class either 'flexsurvreg'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components
#' @importFrom survival Surv
modelExtract.flexsurvreg <- function(CFM){

  ### Model class
  mod_cls <- class(CFM)

  ## Model Ceofficients
  co <- CFM$coefficients
  k <- CFM$k
  kn <- CFM$knots
  sig <- vcov(CFM)
  lam <- (max(kn)-kn)/(max(kn)-min(kn))

  ### Data names
  form <- formula(CFM);form
  mf <- model.frame(CFM)
  lev <- lapply(mf,levels)
  cls <-lapply(mf,class)
  nm <- names(mf);nm
  out.nm <- c("time","cen")

  ## removing 'Surv' from terms
  surv.id <- which(cls=="Surv")
  if(length(surv.id)>0){
    lev <- lev[-surv.id]
    cls <- cls[-surv.id]
    nm <- nm[-surv.id]
  }

  ### Cleaning model parameters
  n_haz_co <- k+2
  haz_co <- co[1:n_haz_co]
  cov_co <- co[(n_haz_co+1):length(co)]

  ret <- list("mod_class"=mod_cls,"terms"=nm,"out.nm"=out.nm,"cov_class"=cls,
              "cov_lev"=lev,"co"=co,"cov_co"=cov_co,"sig"=sig,"haz_co"=haz_co,"k"=k,
              "kn"=kn,"lam"=lam,"formula"=form)
  return(ret)
}
