#' A generic function for extracting model information
#' @param CFM a model of class 'glm'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components.  Included are
#' \itemize{
#' \item{mod_class: The class of the model object}
#' \item{terms: The terms included in the model}
#' \item{cov_cls: The classes of the model covariates}
#' \item{cov_lev: The levels of categorical variables}
#' \item{cov_co: covariate coefficients}
#' \item{sig: variance-covariance matrix}
#' \item{form: model formula}
#' \item{family: model family}
#' \item{out.nm: outcome covariates names}
#' }
#' @export
#'
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

  ret <- list("mod_class"=mod_cls,"terms"=nm,"cov_class"=cls,"cov_lev"=lev,
              "cov_co"=co,"sig"=sig,"formula"=form,"family"=fam,"out.nm"=nm[1])
  ret

}
