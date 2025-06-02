

OBJfunc<-function(para.list){

  Beta.p <- para.list$Beta
  G.p <- para.list$G
  U.p <- para.list$U
  lam <- para.list$lam
  Mmat.q1 <- para.list$Mmat.q1
  Mmat.q <- para.list$Mmat.q
  X <- para.list$X
  Fmat <- para.list$Fmat

  ####matrix ver
  DS <- Fmat-(Beta.p)%*%t(Mmat.q1) #Fmat.ns->Fmat(2/9)
  n <- nrow(Fmat)

  Y.hat <- transformRSdata(X,Beta=Beta.p,Mmat.q=Mmat.q)$Y.hat
  #opt.scales <- t(tcrossprod(Mmat.q, (Beta.p)))
  #Y.hat <- t(apply(cbind(1:n, opt.scales), 1, function(x, y) x[-1][as.numeric(y[x[1], ])], y = X))

  CL <- U.p%*%G.p-Y.hat

  OBres<-lam*(norm(DS,"F")^2) +(1-lam)*(norm(CL,"F")^2)

  OBres
  #browser()
  #list(obval=OBres)

}
