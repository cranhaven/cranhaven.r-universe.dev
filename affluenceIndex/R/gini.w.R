gini.w <-
function(x, weight){
  n <- length(x)
  ifelse(is.null(weight), weight <- rep(1, n), weight <- weight)
    gg <- sum(weight*sapply(1:n, function(i){
      sum(weight*abs(x[i] - x))}))
    GG <- gg/(2*(sum(weight)*sum(x*weight)))
  return(GG)
}
