GSAfisher.multiple <-
function(...){
  p.multiple<-apply(cbind(...), 2, function (x) pchisq(-2 * sum(log(x)),df=2*length(x),lower.tail=FALSE))
  return(p.multiple)
}
