


smooth.bound <- function(Fmat,Mmat.q1=Mmat.q1){

  n <- nrow(Fmat)

  ###constraint for lsei function
  Ec<-rep(1,3) #equation constraint
  Fc<-1
  Gc<-diag(3) #inequation constraint
  Hc<-rep(0,3)

  smooth.func<-function(x){
    coef<-limSolve::lsei(A =Mmat.q1[,-1],B=x,G=Gc,H=Hc,E=Ec,F=Fc,type=2)$X
    return(coef)
  }

  Alp.i<-matrix(0,4,n)
  Alp.i[-1,] <- apply(Fmat,1,smooth.func)

  t(Alp.i)


}
