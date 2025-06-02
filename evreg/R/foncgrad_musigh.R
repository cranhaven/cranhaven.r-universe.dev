# Used by foncgrad_RFS
foncgrad_musigh<-function(theta,y,eps,lambda,nu){
  n<-length(y)
  mux<-theta[1:n]
  sig2x<-theta[(n+1):(2*n)]
  hx<-theta[(2*n+1):(3*n)]
  # loss calculation
  sigx<-sqrt(sig2x)
  Z2<-hx*sig2x+1
  Z<-sqrt(Z2)
  sig1<-sigx*Z
  A<-pnorm(y+eps,mux,sigx)-pnorm(y-eps,mux,sigx)
  pl1<- 1/Z*exp(-0.5*hx*(y-eps-mux)^2/Z2)
  pl2<- 1/Z*exp(-0.5*hx*(y+eps-mux)^2/Z2)
  Phi1<-pnorm(y-eps,mux,sig1)
  Phi2<-pnorm(y+eps,mux,sig1)
  B<-pl1*Phi1+pl2*(1-Phi2)
  PL<-A+B
  Phi3<-pnorm(y,mux,sig1)
  BEL<-pmax(0,PL-pl1*Phi3-pl2*(1-Phi3))
  loss <- mean(-lambda*log(BEL+nu)-(1-lambda)*log(PL)) #+ xi*mean(h) +
  #  rho*mean(gam^2) + tau*mean(Beta^2)

  ############ Gradient

  # mux, sig2x and hx
  dpldh<--0.5*Z2^(-3/2)*exp(-0.5*hx*(y-eps-mux)^2/Z2)*(sig2x+(y-eps-mux)^2/Z2)
  dpldmu<-pl1*hx*(y-eps-mux)/Z2
  dpldsig2<-pl1*(hx/2)*((hx*(y-eps-mux)^2-Z2)/Z2^2)
  gradpl1<-cbind(dpldmu,dpldsig2,dpldh)

  dpldh<--0.5*Z2^(-3/2)*exp(-0.5*hx*(y+eps-mux)^2/Z2)*(sig2x+(y+eps-mux)^2/Z2)
  dpldmu<-pl2*hx*(y+eps-mux)/Z2
  dpldsig2<-pl2*(hx/2)*((hx*(y+eps-mux)^2-Z2)/Z2^2)
  gradpl2<-cbind(dpldmu,dpldsig2,dpldh)

  phi<-dnorm((y-eps-mux)/sig1)
  dPhidh<--0.5*sigx*phi*(y-eps-mux)*Z2^(-3/2)
  dPhidmu <- -phi/sig1
  dPhidsig2<- -0.5*phi*(y-eps-mux)*(sig2x*Z2)^(-3/2)*(2*hx*sig2x+1)
  gradPhi1<-cbind(dPhidmu,dPhidsig2,dPhidh)
  phi <- dnorm((y+eps-mux)/sig1)
  dPhidh <- -0.5*sigx*phi*(y+eps-mux)*Z2^(-3/2)
  dPhidmu <- -phi/sig1
  dPhidsig2 <- -0.5*phi*(y+eps-mux)*(sig2x*Z2)^(-3/2)*(2*hx*sig2x+1)
  gradPhi2 <- cbind(dPhidmu,dPhidsig2,dPhidh)

  A<- gradpl1*Phi1+pl1*gradPhi1
  B<- gradpl2*(1-Phi2)-pl2*gradPhi2
  phi2<-dnorm((y+eps-mux)/sigx)
  phi1<-dnorm((y-eps-mux)/sigx)
  C<--1/sigx*(phi2-phi1)
  D<--0.5*sig2x^(-3/2)*(phi2*(y+eps-mux)-phi1*(y-eps-mux))
  gradPL<-rbind(C,D,0)+t(A+B)

  phi<-dnorm((y-mux)/(sigx*Z))
  dPhidh<--0.5*sigx*phi*(y-mux)*Z2^(-3/2)
  dPhidmu<--phi/sig1
  dPhidsig2<- -0.5*phi*(y-mux)*(sig2x*Z2)^(-3/2)*(2*hx*sig2x+1)
  gradPhi3<-cbind(dPhidmu,dPhidsig2,dPhidh)

  A<- gradpl1*Phi3+pl1*gradPhi3
  B<- gradpl2*(1-Phi3)-pl2*gradPhi3
  gradBEL <- gradPL - t(A+B)
  dlossdtheta <- -(1/n)*(lambda*gradBEL/matrix(BEL+nu,3,n,byrow=TRUE)+
                           (1-lambda)*gradPL/matrix(PL,3,n,byrow=TRUE))
  return(list(fun=loss,grad=as.vector(t(dlossdtheta))))
}
