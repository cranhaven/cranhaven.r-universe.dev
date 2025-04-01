coef.geovol <- function (object, as.zoo = TRUE, ...)
{
  
  results <- NULL
  if (as.zoo == TRUE) {
    object$x <- zoo(object$x)
    object$s <- zoo(object$s)
  }
  results$x <- object$x
  results$s <- object$s
  return(results)
  
}
