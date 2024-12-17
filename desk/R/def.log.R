def.log = function(x, lambda = 0, normalize = FALSE){
  if(normalize){
      corr = (exp(mean(log(x))))^(lambda - 1)
  } else {
    corr = 1
  }
  if(lambda == 0)
    xt = log(x)/corr
  else
    xt = (x^lambda - 1)/(corr*lambda)
  return(xt)
}
