#' Reshape array of data matrices into long format
#'
#' @import foreach
#' 
#' @param X 3 dimensional array of matrices to extract to long format
#' @param coords Spatial coordinates associated with the data (longitude in first column)
#' @param yrs Vector with labels for the years
#' 
#' 

arrayToLong = function(X, coords, yrs) {
  
  X.dim = dim(X)
  
  foreach(t = 1:X.dim[length(X.dim)], .combine = 'rbind') %do% {
    if(length(X.dim)==3)
      data.frame(lon = coords[,1], lat = coords[,2], time = yrs[t], X = X[,,t])
    else if(length(X.dim)==2)
      data.frame(lon = coords[,1], lat = coords[,2], time = yrs[t], X = X[,t])
  }
  
}