GSAfisher.default <-
function(x,...){
  p<-pchisq(-2 * sum(log(x)),df=2*length(x),lower.tail=FALSE)
  return(p)
}
