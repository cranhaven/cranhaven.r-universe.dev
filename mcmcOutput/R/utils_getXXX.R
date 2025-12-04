
# Calculation of MC error, effective number of draws, and Rhat
#  these are exported

getMCE <- function(x, pc=TRUE, bad=5, sort=TRUE) {
  x <- mcmcOutput(x)
  out <- getMCEpc3d(matTo3d(x), pc=pc)
  if(!is.na(bad)) {
    out <- out[!is.na(out)]
    out <- out[out > bad]
  }
  if(sort)
    out <- sort(out, decreasing=TRUE, na.last=TRUE)
  if(length(out) == 0)
    message("No values exceed", bad)
  return(out)
}

getNeff <- function(x, bad=10000, sort=TRUE) {
  x <- mcmcOutput(x)
  out <- round(safeNeff(x))
  if(!is.na(bad)) {
    out <- out[!is.na(out) & out > 2]
    out <- out[out < bad]
  }
  if(sort)
    out <- sort(out, na.last=TRUE)
  if(length(out) == 0)
    message("No values are less than", bad)
  return(out)
}

getRhat <- function(x, bad=1.1, sort=TRUE) {
  x <- mcmcOutput(x)
  out <- simpleRhat(x)
  if(!is.na(bad)) {
    out <- out[!is.na(out)]
    out <- out[out > bad]
  }
  if(sort)
    out <- sort(out, decreasing=TRUE, na.last=TRUE)
  if(length(out) == 0)
    message("No values are less than", bad)
  return(out)
}


