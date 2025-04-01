geovolSim <- function(n, m, innovations = NULL, x.sd = 0.5, as.zoo = TRUE, verbose = FALSE)
{
  
  if (is.null(innovations)) innovations <- matrix(rnorm(n*m), n, m)
  if (is.null(colnames(innovations))) colnames(innovations) <- paste("innovations", 1:m, sep = "")
  if (as.zoo == TRUE) innovations <- as.zoo(innovations)
  x <- exp(rnorm(n, 0, x.sd))
  x <- x/mean(x)
  if (as.zoo == TRUE) x <- as.zoo(x)
  e <- sqrt(x)*innovations
  colnames(e) <- paste("e", 1:m, sep = "")
  if (as.zoo == TRUE) e <- as.zoo(e)
  if (verbose)  result <- list(e = e, x = x, innovations = innovations)
  else result <- e
  return(result)
  
}
