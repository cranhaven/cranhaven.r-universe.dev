#' Title running integral
#' @description calculates the running integral for customer input
#' @param x variable x use for integration process
#' @param y variable y use for integration process
#' @param integrate.step = the step used for calculating the integrale the default value is 1
#' @importFrom sfsmisc integrate.xy
#'
#' @export
#' @examples npoints=1000
#' x=seq(1,npoints)
#' y=(dnorm(x, mean=npoints/2, sd=npoints/10))
#' runningIntegral(x,y)


runningIntegral <- function(x, y, integrate.step=1) {
  xp <- x
  yp <- y
  peak.area <- integrate.xy(xp, yp)
  steps <- length(xp)
  ri <- 0
  for (intK in seq(2, steps, integrate.step))
  {
    ri[intK] <- (integrate.xy(xp[1:intK], yp[1:intK])) / peak.area
  }
  mylist <- list("area" = peak.area, "r.i" = ri, "x" = x, "y" = y)
  return(mylist)
}

