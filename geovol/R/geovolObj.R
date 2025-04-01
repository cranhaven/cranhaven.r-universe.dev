geovolObj <- function (par, par.fixed, y, x, flag)
{
  if (x == TRUE) geovol2 <- par.fixed * par + 1 - par.fixed
  else geovol2 <- par * par.fixed + 1 - par
  
  if (flag != 2) ll <- dnorm(x = y, mean = 0, sd = sqrt(geovol2), log = TRUE)    
  if (flag == 0) return(-ll)
  if (flag == 1) return(-sum(ll))
  if (flag == 2) return(geovol2)
}