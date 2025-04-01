fitted.geovol <- function (object, as.zoo = TRUE, ...)
{
  
  if (as.zoo == TRUE) {
    object$geovol2 <- zoo(object$geovol2)
  }
  return(object$geovol2)
  
}