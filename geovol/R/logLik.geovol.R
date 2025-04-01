logLik.geovol <- function (object, ...)
{
  
  logLik.results <- list()
  for(i in 1:object$m) {
    result <- geovolObj(par = object$s[i], par.fixed = object$x[!is.na(object$e[,i])], y = na.omit(object$e[,i]), x = FALSE, flag = 1)
    attr(result, "df") <- 1
    attr(result, "nobs") <- nobs.geovol(object = object)
    logLik.results[[object$names[i]]] <- result
  }
  return(logLik.results)
  
}
