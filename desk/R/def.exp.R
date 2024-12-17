def.exp = function(x, lambda = 0, normalize = FALSE){
  if(normalize){
      corr = (exp(mean(log(x))))^(lambda - 1)
  } else {
    corr = 1
  }
  if(lambda == 0){
    xt = exp(x)/corr
  } else {
    xt = (corr*x*lambda + 1)^(1/lambda)
  }
  return(xt)
}
