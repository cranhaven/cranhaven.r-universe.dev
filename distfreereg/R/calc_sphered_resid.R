calc_sphered_resid <-
function(raw_residuals, Q){
  # Calculate sphered residuals. First verify that raw_residuals is a numeric
  # vector.
  stopifnot(is.vector(raw_residuals), is.numeric(raw_residuals), is.numeric(Q))
  if(is.matrix(Q)){
    sphered_residuals <- as.vector(Q %*% raw_residuals)
  } else {
    sphered_residuals <- Q * raw_residuals
  }
  return(sphered_residuals)
}
