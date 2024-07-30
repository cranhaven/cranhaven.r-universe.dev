r.is <-
function(x, weight, p){
  n <- length(x)
  if (is.null(weight)) weight <- rep(1, n)
    q.p <- rep(weighted.quantile(x, weight, c(p)), length(x))
    ind <- ifelse(x > q.p, 1, 0)
    r.2 <- sum(x*weight*ind)/sum(x*weight)
  return(r.2)
}
