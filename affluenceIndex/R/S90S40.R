S90S40 <-
function(x, weight){
  n <- length(x)
  ifelse(is.null(weight), weight <- rep(1, n), weight <- weight)
  
  ind90 <- ifelse(x > weighted.quantile(x, weight, 0.9), 1, 0)
  ind40 <- ifelse(x <= weighted.quantile(x, weight, 0.4), 1, 0)
  return(sum(x*weight*ind90)/sum(x*weight*ind40))
}
