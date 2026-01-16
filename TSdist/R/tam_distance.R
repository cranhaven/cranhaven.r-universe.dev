TAMDistance <- function(x, y){
  
  if (is(try(TAMInitialCheck(x, y)))[1] == "try-error") {
    return(NA)
  } else {
    
  # Calculates the DTW between two time series and stores the optimal warping path.
  res = dtw(x, y)
  px <- unlist(res["index1"], use.names = FALSE) - 1
  py <- unlist(res["index2"], use.names = FALSE) - 1
  
  dpx <- diff(px)
  dpy <- diff(py)
  
  # Counts the number of samples in delay, advance and phase 
  delay <- length(which(dpx %in% c(0)))
  advance <- length(which(dpy %in% c(0)))
  phase <- length(which(((dpx == 1) * (dpy == 1)) %in% c(1)))
  
  # Get the length of both time series
  len_y <- py[length(py)]
  len_x <- px[length(px)]
  
  # Calculates the ratios of delay, advance and phase
  p_delay <- delay * 1. / len_y
  p_advance <- advance * 1. / len_x
  p_phase <- phase * 1. / min(len_x, len_y)
  
  tam <- p_advance + p_delay + (1 - p_phase)
  
  return(tam)
  }
}

TAMInitialCheck <- function(x, y){
  if (! is.numeric(x) | ! is.numeric(y)) {
    stop('The series must be numeric', call.=FALSE)
  }
  if (! is.vector(x) | ! is.vector(y)) {
    stop('The series must be univariate vectors', call.=FALSE)
  }
  if (length(x) <= 1 | length(y) <= 1) {    
    stop('The series must have a more than one point', call.=FALSE)
  }
  if (any(is.na(x)) | any(is.na(y))) {
    stop('There are missing values in the series', call.=FALSE)
  }
}