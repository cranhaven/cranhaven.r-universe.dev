GSAfisher.print <-
function(x,...){
  p<-pchisq(-2 * sum(log(x)),df=2*length(x),lower.tail=FALSE)
  print.p<-cat("Fisher method combined pvalue", p, "\n")
}
