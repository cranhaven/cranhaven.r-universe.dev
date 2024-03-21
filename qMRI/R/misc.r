

hg1f1 <- function(a, b, z){
  ##
  ##  Confluent Hypergeometric 1F1 (a,b scalar, z vector)
  ##  rel accuracy 1e-13 for z in -1400:700 for a=-.5, .5
  ##  rel accuracy 2e-4 for z < -1400 for a=-.5, .5
  ##
  n <- length(z)
  z[is.na(z)] <- -1e20
  z[is.infinite(z)] <- 1e-20
  .Fortran(C_hg1f1,
           as.double(a),
           as.double(b),
           as.double(z),
           as.integer(n),
           fz = double(n))$fz
}

getnlspars <- function (object) {
  r <- as.vector(object$m$resid())
  w <- object$weights
  n <- if (!is.null(w))
    sum(w > 0)
  else length(r)
  param <- coef(object)
  pnames <- names(param)
  p <- length(param)
  rdf <- n - p
  resvar <- if (rdf <= 0)
    NaN
  else deviance(object)/rdf
  Rmat <- object$m$Rmat()
  XtX <- t(Rmat)%*%Rmat/n
  dimnames(XtX) <- list(pnames, pnames)
  ans <- list(formula = formula(object), residuals = r, sigma = sqrt(resvar),
              df = c(p, rdf), XtX = XtX, invCov = XtX/resvar, call = object$call,
              convInfo = object$convInfo, control = object$control,
              na.action = object$na.action, coefficients = param)
  ans
}

setMPMmask <- function(mpmData,mask){
   if(any(dim(mask)!=mpmData$sdim)||!is.logical(mask)){
      warning("can't set new mask returning old mpmData object \n")
      return(mpmData)
   }
   ddata <- extract(mpmData,"ddata")
   dim(ddata) <- c(mpmData$nFiles,prod(mpmData$sdim))
   mpmData$ddata <- ddata[,mask]
   mpmData$mask <- mask
   mpmData$maskFile <- "none"
   mpmData
}

getnlspars2 <- function (object, sigma, ind) {
#
#   using variance estimates from data instead of RSS
#
  r <- as.vector(object$m$resid())
  w <- object$weights
  n <- if (!is.null(w))
    sum(w > 0)
  else length(r)
  param <- coef(object)
  pnames <- names(param)
  p <- length(param)
  rdf <- n - p
  resvar <- if (rdf <= 0)
    NaN
  else deviance(object)/rdf
  grad <- object$m$gradient()
  XtX <- t(grad)%*%grad
  sgrad <- sigma[ind] * grad
  z <- svd(sgrad)
  if(any(z$d<1e-6*max(z$d))){
     cat("singular covariance\ngradient:\n")
     print(grad)
     cat("sigma\n")
     print(sigma[ind])
  }
  z$d <- pmax(z$d,1e-6*max(z$d))
  sXtXinv <- z$v%*%diag(1/z$d^2)%*%t(z$v)
  XtXsinv <- XtX%*%sXtXinv%*%XtX
  dimnames(XtX) <- list(pnames, pnames)
  ans <- list(formula = formula(object), residuals = r, sigma = sqrt(resvar),
              df = c(p, rdf), XtX = XtX, invCov=XtXsinv,call = object$call,
              convInfo = object$convInfo, control = object$control,
              na.action = object$na.action, coefficients = param)
  ans
}
