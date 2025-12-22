#' A generic function for extracting model information
#' @param CFM a model of class either 'lmer'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components
#' @importFrom lme4 VarCorr
modelExtract.lmerMod <- function(CFM){

  ### Model class
  mod_cls <- class(CFM)[1]

  ## Model Ceofficients
  cod <- summary(CFM)$coefficients
  co <- data.frame(cod)$Estimate

  fam <- gaussian()
  sig <- vcov(CFM)

  ## random effects
  b <- lme4::VarCorr(CFM)
  ran.var <- diag(b)
  e <- summary(CFM)$sigma;e

  ### Getting fixed and random effect model forms
  fix.form <- formula(CFM,fixed.only=TRUE)
  ran.form <- formula(CFM,random.only=TRUE)

  ### Data format
  form <- formula(CFM);form
  mf <- model.frame(CFM);mf
  cls <-lapply(mf,class);cls
  lev <- lapply(mf,levels);lev
  nm <- names(mf);nm

  ret <- list("mod_class"=mod_cls,"terms"=nm,"cov_class"=cls,"cov_lev"=lev,
              "cov_co"=co,"sig"=sig,"family"=fam,"formula"=form,"fix_form"=fix.form,
              "ran.form"=ran.form,"out.nm"=nm[1])
  ret


}
