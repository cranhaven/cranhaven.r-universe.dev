chis<-function(x,n,l){
  # inverse chi function function chi with:
  #  n <- degrees of freedom
  #  l <- scale parameter


  t1 <- n/2
  t2 <- (n/2)+1
  t3 <- gamma(t1)
  p1 <- ((l/2)^t1) / t3
  p2 <- 1 / (x^t2)
  p3 <- exp((-l) / (2 * x))

#  w <- 1/l
#  p1<-1/((w^t1)*(2^t2)*t3)
#  p2<-x^(n-1)
#  p3<-exp(((-1)*x*x)/(2*w))

  chi<-p1*p2*p3

}
