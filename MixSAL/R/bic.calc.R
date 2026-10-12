bic.calc <-
function(loglik,g,p,n){
  v <- g*(2*p+((p^2)+p)/2)+(g-1)
  val <- 2*loglik - v*log(n)
  return(val)
}
