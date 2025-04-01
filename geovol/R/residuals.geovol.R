residuals.geovol <- function (object, as.zoo = TRUE, ...)
{
  
  if (as.zoo == TRUE) {
    object$residuals <- zoo(object$residuals)
  }
    return(object$residuals)
  
}
