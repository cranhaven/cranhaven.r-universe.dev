cost_calc <- function(X, Y, ground_p){
  
  if (!is.double(ground_p) ) ground_p <- as.double(ground_p)
  if (nrow(X) != nrow(Y)) {
    stop("Rows of X and Y should be equal to have same dimension. Observations should be unique by column")
  }
  
  return( cost_calculation_(X, Y, ground_p) )
}
