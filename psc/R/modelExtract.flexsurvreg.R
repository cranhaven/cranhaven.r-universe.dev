#' A generic function for extracting model information
#'
#' This function extracts model information for use with the pscfit.R function.
#'
#' @param CFM a model of class 'flexsurvreg'
#' @details A function for extracting the model information required for using pscfit
#' @return a list of extracted model components.  Included are
#' \itemize{
#' \item{mod_class: The class of the model object}
#' \item{terms: The terms included in the model}
#' \item{cov_cls: The classes of the model covariates}
#' \item{cov_lev: The levels of categorical variables}
#' \item{cov_co: covariate coefficients}
#' \item{sig: variance-covariance matrix}
#' \item{haz_co: hazard parameter coefficients}
#' \item{k: number of knots}
#' \item{knots: knot position}
#' \item{lam: lambda parameter}
#' \item{form: model formula}
#' }
#' @export
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
  form <- formula(CFM)
  mf <- CFM$data$m
  lev <- lapply(mf,levels)
  cls <-lapply(mf,class)
  nm <- names(mf)


  ### Cleaning model parameters
  n_haz_co <- k+2
  haz_co <- co[1:n_haz_co]
  cov_co <- co[(n_haz_co+1):length(co)]

  ret <- list("mod_class"=mod_cls,"terms"=nm,"cov_class"=cls,"cov_lev"=lev,
              "cov_co"=cov_co,"sig"=sig,"haz_co"=haz_co,"k"=k,"kn"=kn,
              "lam"=lam,"formula"=form)
  return(ret)
}

