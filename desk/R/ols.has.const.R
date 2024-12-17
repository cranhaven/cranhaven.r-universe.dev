ols.has.const = function(mod){
  if (attr(mod$terms,"intercept") == 0){
    return(FALSE)}
  if (attr(mod$terms,"intercept") == 1){
    return(TRUE)}
  }
