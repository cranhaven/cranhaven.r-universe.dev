GSAfisher.plot <-
function (...){
  p.multiple<-apply(cbind(...), 2, function (x) pchisq(-2 * sum(log(x)),df=2*length(x),lower.tail=FALSE))
  plot.p.multiple<-plot(p.multiple, main="Fisher method combined pvalue plot", xlab="Data vector number", ylab="Combined p.value")
}
