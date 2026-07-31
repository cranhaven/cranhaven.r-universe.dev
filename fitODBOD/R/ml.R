#' @method AIC ml
#' @export
AIC.ml<-function(object,...)
{
  names(object$AIC)<-NULL
  return(object$AIC)
}
