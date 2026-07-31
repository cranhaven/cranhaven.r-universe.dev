#' @method AIC fit
#' @export
AIC.fit<-function(object,...)
{
  names(object$AIC)<-NULL
  return(object$AIC)
}

#' @method residuals fit
#' @export
residuals.fit<-function(object,...)
{
  names(object$obs.freq)<-names(object$exp.freq)<-NULL
  return(object$obs.freq-object$exp.freq)
}

#' @method fitted fit
#' @export
fitted.fit<-function(object,...)
{
  names(object$exp.freq)<-NULL
  return(object$exp.freq)
}
