"mse" <-
function(x,y){
  if (length(x)!=length(y)) stop("MSE: the lenghts of input vectors must be the same.")
  mse <- 1/length(x) * sum((x-y)^2)
  return(round(mse,5))
}

