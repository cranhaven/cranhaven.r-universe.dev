mccov <- function(S,X1=NULL,X2=NULL,
                  yv=rep(1,nrow(S)),
                  start=0,tol=10^-8,maxit=1000,
                  out_se=FALSE,output=FALSE,fort=TRUE){

# Preliminaries
  check_der = FALSE  # to check derivatives
  n = sum(yv)
  sS = dim(S)
  ns = sS[1]
  TT = sS[2]
  b = max(S)

# Covariate structure and related matrices: initial probabilities
  if((b+1) == 2){
    GBe = as.matrix(c(0,1))
  }else{
    GBe = diag(b+1); GBe = GBe[,-1]
  }
  if(is.null(X1)){
    nc1=0
    Xlab = rep(1,ns)
    nameBe = NULL
  }else{
    if(is.vector(X1)) X1 = matrix(X1,ns,1)
    nc1 = dim(X1)[2] # number of covariates on the initial probabilities
    if(ns!= dim(X1)[1]) stop("dimension mismatch between S and X1")
    nameBe = colnames(X1)
    out = aggr_data(X1,fort=fort)
    Xdis = out$data_dis
    if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
    Xlab = out$label
  }
  Xndis = max(Xlab)
  XXdis = array(0,c(b+1,b*nc1,Xndis))
  for(i in 1:Xndis){
    if(nc1==0) xdis = 1 else xdis = Xdis[i,]
    XXdis[,,i] = GBe%*%(diag(b)%x%t(xdis))
  }

# for the transition probabilities
  if(is.null(X2)){
    nc2 = 0
    Zlab = rep(1,ns*(TT-1))
    nameGa = NULL
    Zndis = max(Zlab)
  }else{
    if(TT==2) X2 = array(X2,c(ns,1,dim(X2)[2]))
    if(is.matrix(X2)) X2 = array(X2,c(ns,TT-1,1))
    nc2 = dim(X2)[3] # number of covariates on the transition probabilities
    if(ns!= dim(X2)[1]) stop("dimension mismatch between S and X2")
    nameGa = colnames(aperm(X2,c(1,3,2)))
    Z = NULL
    for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
    if(nc2==1) Z = as.vector(X2)
    out = aggr_data(Z,fort=fort); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
    if(nc2==1) Zdis=matrix(Zdis,length(Zdis),1)
  }
  ZZdis = array(0,c(b+1,(b)*nc2,Zndis,b+1))
  for(h in 1:(b+1)){
    if((b+1)==2){
      if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
    }else{
      GGa = diag(b+1); GGa = GGa[,-h]
    }
    for(i in 1:Zndis){
      if(nc2==0) zdis = 1 else zdis = Zdis[i,]
      ZZdis[,,i,h] = GGa%*%(diag(b)%x%t(zdis))
    }
  }

# parameters on initial probabilities
  if(start==0) be = array(0,nc1*b)
  else if(start==1){
    be = c(rnorm(1),rep(0,nc1-1))
    if((b+1)>2) for(h in 2:b) be = c(be,rnorm(1),rep(0,nc1-1))
  }
  out = prob_multilogit(XXdis,be,Xlab,fort)
  Piv = out$P; Pivdis = out$Pdis

# parameters on transition probabilities
  Ga = matrix(0,nc2*b,b+1)
  if(start==0) Ga[1+(0:(b-1))*nc2,] = -log(10)
  else if(start==1) Ga[1+(0:(b-1))*nc2,] = -abs(rnorm(b))
  PIdis = array(0,c(Zndis,b+1,b+1)); PI = array(0,c(b+1,b+1,ns,TT))
  for(h in 1:(b+1)){
    tmp = ZZdis[,,,h]
    if(nc2==1) tmp = array(tmp,c(b+1,b,Zndis))
    out = prob_multilogit(tmp,Ga[,h],Zlab,fort)
    PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,b+1,ns,TT-1))
  }

  #updating be
  V = matrix(0,ns,b+1)
  for(i in 1:ns) V[i,S[i,1]+1]=yv[i]
  out = est_multilogit(V,XXdis,Xlab,be,Pivdis,fort=fort)
  be = out$be; Pivdis = out$Pdi; Piv = out$P
  if(out_se){
    iFi = ginv(out$Fi)
    sebe = sqrt(diag(iFi))
  }
  #Updating Ga
  U = array(0,c(b+1,b+1,ns,TT))
  for(i in 1:ns) for(t in 2:TT){
    U[S[i,t-1]+1,S[i,t]+1,i,t] = yv[i]
  }
  if(out_se) sega = matrix(0,nc2*b,b+1)
  for(h in 1:(b+1)){
    UU = NULL
    for(t in 2:TT) UU = rbind(UU,t(U[h,,,t]))
    tmp = ZZdis[,,,h]
    if(nc2==1) tmp = array(tmp,c(b+1,b,Zndis))
    tmp2 = PIdis[,,h]
    if(Zndis==1) tmp2 = matrix(tmp2,1,b+1)
    out = est_multilogit(UU,tmp,Zlab,Ga[,h],tmp2,fort=fort)
    PIdis[,,h] = out$Pdis; PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,b+1,ns,TT-1))
    Ga[,h] = out$be
    if(out_se){
      iFi = ginv(out$Fi)
      sega[,h] = sqrt(diag(iFi))
    }
  }


  # Compute log-likelihood
  lk = sum(V*log(Piv))+sum(U[,,,2:TT]*log(PI[,,,2:TT]))


  # Compute number of parameters
  np = b*nc1
  np = np+b*nc2*(b+1)
  aic = -2*lk+np*2
  bic = -2*lk+np*log(n)
  Be = matrix(be,nc1,b)
  if(is.null(nameBe)) if(nc1==1) nameBe = "(Intercept)"
  dimnames(Be) = list(nameBe,logit=2:(b+1))
  if(out_se){
    seBe = matrix(sebe,nc1,b)
    dimnames(seBe) = list(nameBe,logit=2:(b+1))
  }
  if(is.null(nameGa)) if(nc2==1) nameGa = c("(Intercept)")

  if((b+1)>2) {
    Ga = array(as.vector(Ga),c(nc2,b,b+1))
    dimnames(Ga) = list(nameGa,logit=2:(b+1),logit=1:(b+1))
  }else if((b+1)==2){
    dimnames(Ga) = 	list(nameGa,logit=1:(b+1))
  }
  if(out_se){
    if((b+1)==2){
      seGa = matrix(sega,nc2,2)
      dimnames(seGa) = list(nameGa,logit=1:(b+1))
    }else if((b+1)>2){
      seGa = array(as.vector(sega),c(nc2,b,b+1))
      dimnames(seGa) = list(nameGa,logit=2:(b+1),logit=1:(b+1))
    }
  }
  # adjust output
  lk = as.vector(lk)
  if(output){
    dimnames(Piv)=list(subject=1:ns,category=0:b)
    dimnames(PI)=list(category=0:b,category=0:b,subject=1:ns,time=1:TT)
  }
  out = list(lk=lk,Be=Be,Ga=Ga,np=np,aic=aic,bic=bic,n=n,TT=TT,ns=ns,yv=yv)
  if(out_se){
    out$seBe = seBe
    out$seGa = seGa
  }

# final output
  if(output){
    out$PI = PI
    out$Piv = Piv
  }
  class(out)="MCcov"
  return(out)

}
