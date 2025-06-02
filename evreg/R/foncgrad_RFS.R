# Function used by ENNreg and ENNreg_online
foncgrad_RFS<-function(psi,X,y,K,eps,lambda,xi,rho,nu,optimProto){
  p<-ncol(X) # X must be a matrix
  n<-length(y)
  # Parameter extraction
  alpha<-psi[1:K]
  beta<-psi[(K+1):(K*p+K)]
  Beta<-matrix(beta,K,p)
  sig<-psi[(K*p+K+1):(K*p+2*K)]
  sig2<-sig^2
  eta<-psi[(K*p+2*K+1):(K*p+3*K)]
  gam<-psi[(K*p+3*K+1):(K*p+4*K)]
  w<-psi[(K*p+4*K+1):(2*K*p+4*K)]
  W<-matrix(w,K,p)
  h<-eta^2
  ####################### Propagation
  d<-matrix(0,n,K)
  a<-matrix(0,n,K)
  for(k in 1:K){
    d[,k] <- rowSums((X - matrix(W[k,],n,p,byrow=TRUE))^2)
    a[,k] <- exp(- gam[k]^2 * d[,k])
  }
  H<-matrix(h,n,K,byrow=TRUE)
  aH<-a*H
  hx<-rowSums(aH)
  S2<-hx^2
  mu<-X%*%t(Beta)+matrix(alpha,n,K,byrow=TRUE) # size (n,K)
  mux<-rowSums(mu*aH)/hx
  sig2x<-rowSums(matrix(sig2,n,K,byrow=TRUE)*aH^2)/S2

  # loss calculation
  theta<-c(mux,sig2x,hx)
  fg<-foncgrad_musigh(theta,y,eps,lambda,nu)
  loss<-fg$fun + xi*mean(h) + rho*mean(gam^2)
  dlossdtheta <-matrix(fg$grad,3,n,byrow=TRUE)

  # beta_j and alpha_j
  dmuxdmu<-aH/hx # size (n,K)
  dlossdbeta<-matrix(0,K,p)
  for(k in 1:K) dlossdbeta[k,]<-(dlossdtheta[1,]*dmuxdmu[,k])%*%X
  dlossdalpha<-dlossdtheta[1,]%*%dmuxdmu
  # sig_j
  dsig2xdsig2<-aH^2/S2 # size (n,K)
  dlossdsig2<-dlossdtheta[2,]%*%dsig2xdsig2
  dlossdsig<-2*sig*dlossdsig2
  # eta_j
  dhxdh<-a  #(n,K)
  dmuxdh<- a*(mu-matrix(mux,n,K))/hx  #(n,K)
  dsig2xdh<-2*a*(a*matrix(sig2*h,n,K,byrow=TRUE)-matrix(hx*sig2x,n,K))/S2  #(n,K)
  dlossdh<-dlossdtheta[3,]%*%dhxdh+dlossdtheta[1,]%*%dmuxdh+dlossdtheta[2,]%*%dsig2xdh + xi/K
  dlossdeta<-2*eta*dlossdh
  # gamma_j et w
  dhxda<-H
  dmuxda<- H*(mu-matrix(mux,n,K))/hx  #(n,K)
  dsig2xda<-2*H*(a*matrix(sig2*h,n,K,byrow=TRUE)-matrix(hx*sig2x,n,K))/S2  #(n,K)
  dlossda <- matrix(dlossdtheta[1,],n,K)*dmuxda+matrix(dlossdtheta[2,],n,K)*dsig2xda+
    matrix(dlossdtheta[3,],n,K)*dhxda #(n,K)
  dadgamma<--2*matrix(gam,n,K,byrow=TRUE)*d*a # (n,K)
  dlossdgamma<-colSums(dlossda*dadgamma) + 2*rho*gam/K
  dlossdw<-matrix(0,K,p)
  if(optimProto){
    for(k in 1:K){
      dajdw<- 2*gam[k]^2*(X-matrix(W[k,],n,p,byrow=TRUE))*matrix(a[,k],n,p) # (n,p)
      dlossdw[k,]<-dlossda[,k]%*%dajdw
    }
  }
  grad<-c(dlossdalpha,as.vector(dlossdbeta),dlossdsig,dlossdeta,dlossdgamma,as.vector(dlossdw))
  return(list(fun=loss,grad=grad))
}
