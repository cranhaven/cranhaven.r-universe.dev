acc = function(x, lag = 1){
  n = length(x)
  x1 = x[1:(n-lag)]
  x2 = x[(lag+1):n]
  out = sum((x1 - mean(x)) * (x2 - mean(x))) / sum( (x - mean(x))^2 )
  return(out)
}
