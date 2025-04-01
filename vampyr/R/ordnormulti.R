ordnormulti<-function(z,PHI){

  n     <- nrow(cbind(z))
  dospi <-6.2832
  tmp1  <-(dospi)^(n/2)
  if (n == 1){ # JNT Right ??
    tmp2 <- sqrt(PHI)
  } else {
    tmp2<- sqrt(det(PHI))
  }
  tmp3<-1/(tmp1*tmp2)
  quad<-t(z)%*%solve(PHI)%*%z # JNT: Alternative for solve() is chol2inv(chol())). It saves time when PHI is large (not your case here).
  orde<-tmp3*exp(-.5*(quad))

}
