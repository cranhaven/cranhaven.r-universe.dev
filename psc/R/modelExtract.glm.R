#' A generic function for extracting model information
#' @param CFM a model of class either 'glm'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components
modelExtract.glm <- function(CFM){

  ### Model class
  mod_cls <- class(CFM)

  ## Model Ceofficients
  co <- CFM$coefficients;co
  fam <- CFM$family
  sig <- vcov(CFM)

  ### Data names
  form <- formula(CFM)
  mf <- model.frame(CFM)
  cls <-lapply(mf,class);cls

  char.id <- which(cls=="character")
  lev <- lapply(mf,levels);lev
  nm <- names(mf);nm

  ret <- list("mod_class"=mod_cls,"terms"=nm,"cov_class"=cls,"cov_lev"=lev,"co"=co,
              "cov_co"=co,"sig"=sig,"formula"=form,"family"=fam,"out.nm"=nm[1])
  ret

}

