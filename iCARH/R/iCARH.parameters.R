#' @title Return model parameters
#'
#'@description Group of functions to return model parameters of interest
#'
#' @describeIn iCARH.getBeta Get beta parameter draws from all chains combined
#'
#' @param fit Object returned by iCARH.model
#' @param path.names pathway names
#'
#' @return the \code{iCARH.get[*]} functions return a an array with corresponding model parameters MCMC draws.
#'  
#'
#' @examples data.sim = iCARH.simulate(4, 10, 14, 8, 2, path.probs=0.3, Zgroupeff=c(0,4),
#' beta.val=c(1,-1,0.5, -0.5))
#' XX = data.sim$XX
#' Y = data.sim$Y
#' Z = data.sim$Z
#' pathways = data.sim$pathways
#' \donttest{
#' rstan_options(auto_write = TRUE)
#' options(mc.cores = 2)
#' fit = iCARH.model(XX, Y, Z,groups=rep(c(0,1), each=5), pathways, 
#' control = list(adapt_delta = 0.99, max_treedepth=10), iter = 2, chains = 2)
#' if(!is.null(fit$icarh))
#' iCARH.getBeta(fit)}
#'
#' @export iCARH.getBeta
#'
#' @import ggplot2
#' @importFrom rstan extract

iCARH.getBeta = function(fit){
  gam1= extract(fit$icarh, inc_warmup = FALSE, pars="beta")$beta
  xnames = attr(fit$X, "dimnames")[[3]]
  ynames = attr(fit$Y, "dimnames")[[3]]
  attr(gam1,"dimnames")[[2]] = if(is.null(xnames)) paste("X",1:dim(gam1)[2]) else xnames
  attr(gam1,"dimnames")[[3]] = if(is.null(ynames)) paste("Y",1:dim(gam1)[3]) else ynames
  names(dimnames(gam1))=c("iterations", "X", "Y")
  return(gam1)
}


#' @describeIn iCARH.getBeta  return theta coefficients
#' @export iCARH.getARCoeff

iCARH.getARCoeff = function(fit){
  gam1= extract(fit$icarh, inc_warmup = FALSE, pars="theta")$theta
  xnames = attr(fit$X, "dimnames")[[3]]
  attr(gam1,"dimnames")[[2]] = if(is.null(xnames)) paste("X",1:dim(gam1)[2]) else xnames
  names(dimnames(gam1))=c("iterations", "X")
  return(gam1)
}

#' @describeIn iCARH.getBeta  return alpha coefficients
#' @export iCARH.getTreatmentEffect

iCARH.getTreatmentEffect = function(fit){
  gam1= extract(fit$icarh, inc_warmup = FALSE, pars="alpha")$alpha
  xnames = attr(fit$X, "dimnames")[[3]]
  attr(gam1,"dimnames")[[2]] = if(is.null(xnames)) paste("X",1:dim(gam1)[2]) else xnames
  names(dimnames(gam1))=c("iterations", "X")
  return(gam1)
}

#' @describeIn iCARH.getBeta  return phi coefficients
#' @export iCARH.getPathwaysCoeff

iCARH.getPathwaysCoeff = function(fit, path.names=NULL){
  phi= extract(fit$icarh, inc_warmup = FALSE, pars="phi")$phi
  attr(phi,"dimnames")[[2]] = if(is.null(path.names)) paste("path",1:dim(phi)[2]) else path.names
  attr(phi,"dimnames")[[3]] = c("controls","cases")
  names(dimnames(phi))=c("iterations", "pathways", "groups")
  return(phi)
}

#' @describeIn iCARH.getBeta  return complete data (including imputed data)
#' @export iCARH.getDataImputation

iCARH.getDataImputation = function(fit){
  stopifnot(any(is.na(fit$X))|any(is.na(fit$Y)))
  YY = NULL
  XX = NULL
  if(any(is.na(fit$X))){
    XX = extract(fit$icarh, inc_warmup=F, pars="XX")$XX
    attr(XX, "dimnames")[2:4] = dimnames(fit$X)
    names(dimnames(XX))=c("iterations", "timepoints", "observations", "variables") 
  }
  if(!is.null(fit$Y) & any(is.na(fit$Y))){
    YY = extract(fit$icarh, inc_warmup=F, pars="YY")$YY
    attr(YY, "dimnames")[2:4] = dimnames(fit$Y)
    names(dimnames(YY))=c("iterations", "timepoints", "observations", "variables")
  }
  return(list(X=XX, Y=YY))
}

