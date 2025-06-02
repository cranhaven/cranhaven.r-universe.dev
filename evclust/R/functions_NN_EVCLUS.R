## Internal functions used by nn_evclus
#### Version 5 July, 2020 (normalization, no a and b)
foncgrad2_ReLU6<-function(theta,X,fhat,y,Is,D,J,Xi,F,lambda,nu,ML,CL,Q,rho){
  # Version with only one loop on i, ReLU activation function, 
  # One-SVM input (optional) and semi-supervised learning (optional)
  n<-nrow(X)
  d<-ncol(X)
  N<-length(theta)
  f<-nrow(Xi)
  p<-ncol(D)
  n_H<-(N-2-f)/(d+f)
#  a<-theta[1]
#  b<-theta[2]
  V<-matrix(theta[1:(n_H*d)],n_H,d)
  W<-matrix(theta[(n_H*d+1):(N-2)],f,n_H+1)
  beta<-theta[c(N-1,N)]
#  C<-1/sum(D^(2-tau))
  C<-1/(n*p)
  c<-ncol(F)
  
  # Propagation
  Zeros<-matrix(0,n,n_H)
  A<-X%*%t(V)
  Z<-cbind(rep(1,n),pmax(Zeros,A)) # size(n,n_H+1)
  alpha<-Z%*%t(W)
  mass<-exp(alpha-apply(alpha,1,max))
  mass<-mass/rowSums(mass)
  if(is.null(fhat)){
    mass1<-mass
    gam<-rep(0,n)
  } else{
    betafhat<-beta[1]+beta[2]*fhat
    #   eta<-pmax(0,betafhat)
    eta<-log(1+exp(betafhat))
    gam<-eta/(1+eta)
    mass1<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
    #    dgdbeta0<-1/(1+eta)^2*(betafhat > 0)
    dgdbeta0<-1/(1+eta)^2 * exp(betafhat)/(1+exp(betafhat))
    dgdbeta1<-dgdbeta0*fhat
  }
  K<-matrix(0,n,p)
  mK<-mass1 %*% Xi
  for(i in 1:n) K[i,]= mK[i,] %*% t(mass1[J[i,],])
#  error<-a*K+b-D
  error<-K-D
  S<-(1-nu)*sum(error^2)*C + 0.5*lambda*(mean(V^2)+mean(W^2))
  semi<-(nu>0) 
  if(semi){
    Id<-diag(c)
    Y<-Id[y,]
    pl1<-mass1[Is,]%*%F
    S<-S+nu*mean((pl1-Y)^2)
  }
  pairwise<-(rho>0)
  if(pairwise){
    nml=nrow(ML)
    ncl=nrow(CL)
    Lplus<-0
    Lminus<-0
    Q1<-2-Q
    for(i in 1:nml) Lplus<-Lplus+mass1[ML[i,1],]%*%Q%*%mass1[ML[i,2],]
    for(i in 1:ncl) Lminus<-Lminus+mass1[CL[i,1],]%*%Q1%*%mass1[CL[i,2],]
    rho1<-rho/(2*(nml+ncl))
    S<-S+rho1*(Lplus+Lminus)
  }
  # Backpropagation
  gradW<-matrix(0,f,n_H+1) # term corresponding to Lij
  gradV<-matrix(0,n_H,d) # term corresponding to Lij
  gradbeta<-c(0,0)
  dmdalpha<-array(0,c(n,f,f))
  mass_empty<-c(1,rep(0,f-1))
  for(i in 1:n) dmdalpha[i,,]<- -mass[i,]%o%mass[i,] + diag(mass[i,])
  # Lij part
#  grada<-(1-nu)*2*C*sum(K*error/w)
#  gradb<-(1-nu)*2*C*sum(error/w)
  ii<-2:(n_H+1)
  for(i in 1:n){
    # Hidden-to-output
    dKijdmi=mK[J[i,],] # size (p,f)
    Error<-matrix(error[i,],p,f)
    Deltaijq<-2*C*Error*(dKijdmi %*% dmdalpha[i,,])*(1-gam[i]) # size (p,f)
    dKijdmj=matrix(mK[i,],p,f,byrow=TRUE)
    B<-matrix(0,p,f)
    for(r in 1:f) B<-B+matrix(dKijdmj[,r],p,f)*dmdalpha[J[i,],r,]
    Deltapijq<-2*C*Error*B*matrix(1-gam[J[i,]],p,f)
    gradW<- gradW+t(Deltaijq)%*%matrix(Z[i,],p,n_H+1,byrow=TRUE)+
      t(Deltapijq) %*% Z[J[i,],]
    # Beta
    if(!is.null(fhat)){
      dJijdgi=2*C*error[i,]*(dKijdmi%*%(-mass[i,]+mass_empty)) # size (p,1)
      dJijdgj=2*C*error[i,]*rowSums(dKijdmj * 
                                            (-mass[J[i,],]+matrix(mass_empty,p,f,byrow=TRUE))) # size (p,1)
      gradbeta[1] <- gradbeta[1]+dgdbeta0[i]*sum(dJijdgi)+sum(dJijdgj*dgdbeta0[J[i,]])
      gradbeta[2] <- gradbeta[2]+dgdbeta1[i]*sum(dJijdgi)+sum(dJijdgj*dgdbeta1[J[i,]])
    } #endif
    # Input-to-Hidden
    Deltaijh<-matrix(Z[i,ii]>0,p,n_H,byrow=TRUE)* (Deltaijq%*%W[,ii])
    Deltapijh<-(Z[J[i,],ii]>0) * (Deltapijq%*%W[,ii])
    gradV<-gradV+t(Deltaijh)%*%matrix(X[i,],p,d,byrow=TRUE) +
      t(Deltapijh) %*% X[J[i,],]
  } # end for i
  # Li part
  if(semi){
    gradbeta1<-c(0,0)
    ns<-length(Is)
    # Hidden-to-output
    Deltaiq<-matrix(0,ns,f)
    for(i in 1:ns){
      dpldalpha<-(t(F) %*% dmdalpha[Is[i],,])*(1-gam[Is[i]]) # size (c,f)
      Deltaiq[i,]<-2*(pl1[i,]-Y[i,]) %*% dpldalpha # size (1,f)
    } # endfor
    gradW1<-t(Deltaiq)%*%Z[Is,] # size (f,n_H+1)
    # Input-to-hidden
    Deltaih<-(Z[Is,2:(n_H+1)]>0)* (Deltaiq %*% W[,2:(n_H+1)])
    gradV1<-t(Deltaih) %*% X[Is,]
    # Beta
    if(!is.null(fhat)){
      dpldg<-matrix(0,ns,c)
      for(i in 1:ns) dpldg[i,]<-t(F)%*%(-mass[Is[i],]+mass_empty)
      dLidg<-rowSums(2*(pl1-Y) * dpldg)
      gradbeta1[1]<-dLidg %*% dgdbeta0[Is]
      gradbeta1[2]<-dLidg %*% dgdbeta1[Is]
    } # if svm
  } # if semi
  # Pairwise part
  if(pairwise){
    gradW2<-matrix(0,f,n_H+1) 
    gradV2<-matrix(0,n_H,d) 
    gradbeta2<-c(0,0)
    # ML constraints
    for(k in 1:nml){
      # Hidden-to-output
      i<-ML[k,1]
      j<-ML[k,2]
      dLijdmi=Q%*%mass1[j,] # length f
      Nablaijq<-dmdalpha[i,,] %*% dLijdmi * (1-gam[i])  # length f
      dLijdmj=Q%*%mass1[i,] # length f
      Nablapijq<- dmdalpha[j,,] %*% dLijdmj *(1-gam[j])  # length f
      gradW2<- gradW2+Nablaijq%*%Z[i,]+Nablapijq%*%Z[j,]
      # Beta
      if(!is.null(fhat)){
        dLijdgi=sum(dLijdmi*(-mass[i,]+mass_empty)) # size (1,1)
        dLijdgj=sum(dLijdmj*(-mass[j,]+mass_empty)) # size (1,1)
        gradbeta2[1] <- gradbeta2[1]+dgdbeta0[i]*dLijdgi+dLijdgj*dgdbeta0[j]
        gradbeta2[2] <- gradbeta2[2]+dgdbeta1[i]*dLijdgi+dLijdgj*dgdbeta1[j]
      } #endif
      # Input-to-Hidden
      Nablaijh<-(Z[i,ii]>0)* (t(Nablaijq)%*%W[,ii])
      Nablapijh<-(Z[j,ii]>0) * (t(Nablapijq)%*%W[,ii])
      gradV2<-gradV2+t(Nablaijh)%*%X[i,] + t(Nablapijh)%*%X[j,]
    }
    # CL constraints
    for(k in 1:ncl){
      # Hidden-to-output
      i<-CL[k,1]
      j<-CL[k,2]
      dLijdmi <- Q1%*%mass1[j,] # length f
      Nablaijq <- dmdalpha[i,,] %*% dLijdmi * (1-gam[i])# length f
      dLijdmj <- Q1%*%mass1[i,] # length f
      Nablapijq <- dmdalpha[j,,] %*% dLijdmj * (1-gam[j]) # length f
      gradW2 <- gradW2 + Nablaijq%*%Z[i,] + Nablapijq%*%Z[j,]
      # Beta
      if(!is.null(fhat)){
        dLijdgi=sum(dLijdmi*(-mass[i,]+mass_empty)) # size (1,1)
        dLijdgj=sum(dLijdmj*(-mass[j,]+mass_empty)) # size (1,1)
        gradbeta2[1] <- gradbeta2[1]+dgdbeta0[i]*dLijdgi+dLijdgj*dgdbeta0[j]
        gradbeta2[2] <- gradbeta2[2]+dgdbeta1[i]*dLijdgi+dLijdgj*dgdbeta1[j]
      } #endif
      # Input-to-Hidden
      Nablaijh <- (Z[i,ii]>0)* (t(Nablaijq)%*%W[,ii])
      Nablapijh <- (Z[j,ii]>0) * (t(Nablapijq)%*%W[,ii])
      gradV2<-gradV2 + t(Nablaijh)%*%X[i,] + t(Nablapijh)%*%X[j,]
    }
  }
  
  # Putting everything together
  gradV<-(1-nu)*gradV +  lambda*V/(n_H*d)
  gradW<-(1-nu)*gradW +  lambda*W/(f*(n_H+1))
  gradbeta<- (1-nu)*gradbeta
  if(semi){
    gradV<-gradV + nu/(ns*c)*gradV1
    gradW<-gradW + nu/(ns*c)*gradW1
    gradbeta<- gradbeta + nu/(ns*c) * gradbeta1
  }  
  if(pairwise){
    gradV<-gradV + rho1*gradV2
    gradW<-gradW + rho1*gradW2
    gradbeta<- gradbeta + rho1 * gradbeta2
  }
#  grad<-c(grada,gradb,as.vector(gradV),as.vector(gradW),gradbeta)
  grad<-c(as.vector(gradV),as.vector(gradW),gradbeta)
  return(list(fun=S,grad=grad))
}



#### Version 18 July, 2020 (normalization, no a and b) 2 hidden layers
foncgrad2_ReLU7<-function(theta,n_H,X,fhat,y,Is,D,J,Xi,F,lambda,nu,ML,CL,Q,rho){
  # Version with only one loop on i, ReLU activation function, 
  # One-SVM input (optional) and semi-supervised learning (optional)
  n<-nrow(X)
  d<-ncol(X)
  N<-length(theta)
  f<-nrow(Xi)
  p<-ncol(D)
  N_U<-n_H[1]*d
  N_V<-n_H[2]*(n_H[1]+1)
  N_W<-f*(n_H[2]+1)
  U<-matrix(theta[1:N_U],n_H[1],d)
  V<-matrix(theta[(N_U+1):(N_U+N_V)],n_H[2],n_H[1]+1)
  W<-matrix(theta[(N_U+N_V+1):(N-2)],f,n_H[2]+1)
  beta<-theta[c(N-1,N)]
  #  C<-1/sum(D^(2-tau))
  C<-1/(n*p)
  c<-ncol(F)
  
  
  # Propagation
  Zeros1<-matrix(0,n,n_H[1])
  A1<-X%*%t(U)
  Z1<-cbind(rep(1,n),pmax(Zeros1,A1)) # size(n,n_H[1]+1)
  Zeros2<-matrix(0,n,n_H[2])
  A2<-Z1%*%t(V)
  Z2<-cbind(rep(1,n),pmax(Zeros2,A2)) # size(n,n_H[2]+1)
  alpha<-Z2%*%t(W)
  mass<-exp(alpha-apply(alpha,1,max))
  mass<-mass/rowSums(mass)
  if(is.null(fhat)){
    mass1<-mass
    gam<-rep(0,n)
  } else{
    betafhat<-beta[1]+beta[2]*fhat
    #   eta<-pmax(0,betafhat)
    eta<-log(1+exp(betafhat))
    gam<-eta/(1+eta)
    mass1<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
    #    dgdbeta0<-1/(1+eta)^2*(betafhat > 0)
    dgdbeta0<-1/(1+eta)^2 * exp(betafhat)/(1+exp(betafhat))
    dgdbeta1<-dgdbeta0*fhat
  }
  K<-matrix(0,n,p)
  mK<-mass1 %*% Xi
  for(i in 1:n) K[i,]= mK[i,] %*% t(mass1[J[i,],])
  #  error<-a*K+b-D
  error<-K-D
  S<-(1-nu)*sum(error^2)*C + 0.5*lambda*(mean(U^2)+mean(V^2)+mean(W^2))
  semi<-(nu>0) 
  if(semi){
    Id<-diag(c)
    Y<-Id[y,]
    pl1<-mass1[Is,]%*%F
    S<-S+nu*mean((pl1-Y)^2)
  }
  pairwise<-(rho>0)
  if(pairwise){
    nml=nrow(ML)
    ncl=nrow(CL)
    Lplus<-0
    Lminus<-0
    Q1<-2-Q
    for(i in 1:nml) Lplus<-Lplus+mass1[ML[i,1],]%*%Q%*%mass1[ML[i,2],]
    for(i in 1:ncl) Lminus<-Lminus+mass1[CL[i,1],]%*%Q1%*%mass1[CL[i,2],]
    rho1<-rho/(2*(nml+ncl))
    S<-S+rho1*(Lplus+Lminus)
  }
  #-------------------------------------
  # Backpropagation
  gradW<-matrix(0,f,n_H[2]+1) # term corresponding to Lij
  gradV<-matrix(0,n_H[2],n_H[1]+1) # term corresponding to Lij
  gradU<-matrix(0,n_H[1],d) # term corresponding to Lij
  gradbeta<-c(0,0)
  dmdalpha<-array(0,c(n,f,f))
  mass_empty<-c(1,rep(0,f-1))
  for(i in 1:n) dmdalpha[i,,]<- -mass[i,]%o%mass[i,] + diag(mass[i,])
  # Lij part
  #  grada<-(1-nu)*2*C*sum(K*error/w)
  #  gradb<-(1-nu)*2*C*sum(error/w)
  ii2<-2:(n_H[2]+1)
  ii1<-2:(n_H[1]+1)
  for(i in 1:n){
    # Hidden-to-output
    dKijdmi=mK[J[i,],] # size (p,f)
    Error<-matrix(error[i,],p,f)
    Deltaijq<-2*C*Error*(dKijdmi %*% dmdalpha[i,,])*(1-gam[i]) # size (p,f)
    dKijdmj=matrix(mK[i,],p,f,byrow=TRUE)
    B<-matrix(0,p,f)
    for(r in 1:f) B<-B+matrix(dKijdmj[,r],p,f)*dmdalpha[J[i,],r,]
    Deltapijq<-2*C*Error*B*matrix(1-gam[J[i,]],p,f)
    gradW<- gradW+t(Deltaijq)%*%matrix(Z2[i,],p,n_H[2]+1,byrow=TRUE)+
      t(Deltapijq) %*% Z2[J[i,],]
    # Beta
    if(!is.null(fhat)){
      dJijdgi=2*C*error[i,]*(dKijdmi%*%(-mass[i,]+mass_empty)) # size (p,1)
      dJijdgj=2*C*error[i,]*rowSums(dKijdmj * 
                                            (-mass[J[i,],]+matrix(mass_empty,p,f,byrow=TRUE))) # size (p,1)
      gradbeta[1] <- gradbeta[1]+dgdbeta0[i]*sum(dJijdgi)+sum(dJijdgj*dgdbeta0[J[i,]])
      gradbeta[2] <- gradbeta[2]+dgdbeta1[i]*sum(dJijdgi)+sum(dJijdgj*dgdbeta1[J[i,]])
    } #endif
    # Hidden-to-Hidden
    Deltaijh2<-matrix(Z2[i,ii2]>0,p,n_H[2],byrow=TRUE)* (Deltaijq%*%W[,ii2])
    Deltapijh2<-(Z2[J[i,],ii2]>0) * (Deltapijq%*%W[,ii2])
    gradV<-gradV+t(Deltaijh2)%*%matrix(Z1[i,],p,n_H[1]+1,byrow=TRUE) +
      t(Deltapijh2) %*% Z1[J[i,],]
    # Input-to-Hidden
    Deltaijh1<-matrix(Z1[i,ii1]>0,p,n_H[1],byrow=TRUE)* (Deltaijh2%*%V[,ii1])
    Deltapijh1<-(Z1[J[i,],ii1]>0) * (Deltapijh2%*%V[,ii1])
    gradU<-gradU+t(Deltaijh1)%*%matrix(X[i,],p,d,byrow=TRUE) +
      t(Deltapijh1) %*% X[J[i,],]
  } # end for i
  # Li part
  if(semi){
    gradbeta1<-c(0,0)
    ns<-length(Is)
    # Hidden-to-output
    Deltaiq<-matrix(0,ns,f)
    for(i in 1:ns){
      dpldalpha<-(t(F) %*% dmdalpha[Is[i],,])*(1-gam[Is[i]]) # size (c,f)
      Deltaiq[i,]<-2*(pl1[i,]-Y[i,]) %*% dpldalpha # size (1,f)
    } # endfor
    gradW1<-t(Deltaiq)%*%Z2[Is,] # size (f,n_H[2]+1)
    # Hidden-to-hidden
    Deltaih2<-(Z2[Is,ii2]>0)* (Deltaiq %*% W[,ii2])
    gradV1<-t(Deltaih2) %*% Z1[Is,]
    # Input-to-hidden
    Deltaih1<-(Z1[Is,ii1]>0)* (Deltaih2 %*% V[,ii1])
    gradU1<-t(Deltaih1) %*% X[Is,]
    # Beta
    if(!is.null(fhat)){
      dpldg<-matrix(0,ns,c)
      for(i in 1:ns) dpldg[i,]<-t(F)%*%(-mass[Is[i],]+mass_empty)
      dLidg<-rowSums(2*(pl1-Y) * dpldg)
      gradbeta1[1]<-dLidg %*% dgdbeta0[Is]
      gradbeta1[2]<-dLidg %*% dgdbeta1[Is]
    } # if svm
  } # if semi
  # Pairwise part
  if(pairwise){
    gradW2<-matrix(0,f,n_H[2]+1) 
    gradV2<-matrix(0,n_H[2],n_H[1]+1) 
    gradU2<-matrix(0,n_H[1],d)
    gradbeta2<-c(0,0)
    # ML constraints
    for(k in 1:nml){
      # Hidden-to-output
      i<-ML[k,1]
      j<-ML[k,2]
      dLijdmi=Q%*%mass1[j,] # length f
      Nablaijq<-dmdalpha[i,,] %*% dLijdmi * (1-gam[i])  # length f
      dLijdmj=Q%*%mass1[i,] # length f
      Nablapijq<- dmdalpha[j,,] %*% dLijdmj *(1-gam[j])  # length f
      gradW2<- gradW2+Nablaijq%*%Z2[i,]+Nablapijq%*%Z2[j,]
      # Beta
      if(!is.null(fhat)){
        dLijdgi=sum(dLijdmi*(-mass[i,]+mass_empty)) # size (1,1)
        dLijdgj=sum(dLijdmj*(-mass[j,]+mass_empty)) # size (1,1)
        gradbeta2[1] <- gradbeta2[1]+dgdbeta0[i]*dLijdgi+dLijdgj*dgdbeta0[j]
        gradbeta2[2] <- gradbeta2[2]+dgdbeta1[i]*dLijdgi+dLijdgj*dgdbeta1[j]
      } #endif
      # Hidden-to-Hidden
      Nablaijh2<-(Z2[i,ii2]>0)* (t(Nablaijq)%*%W[,ii2])
      Nablapijh2<-(Z2[j,ii2]>0) * (t(Nablapijq)%*%W[,ii2])
      gradV2<-gradV2+t(Nablaijh2)%*%Z1[i,] + t(Nablapijh2)%*%Z1[j,]
      # Input-to-Hidden
      Nablaijh1<-(Z1[i,ii1]>0)* (Nablaijh2%*%V[,ii1])
      Nablapijh1<-(Z1[j,ii1]>0) * (Nablapijh2%*%V[,ii1])
      gradU2<-gradU2+t(Nablaijh1)%*%X[i,] + t(Nablapijh1)%*%X[j,]
    }
    # CL constraints
    for(k in 1:ncl){
      # Hidden-to-output
      i<-CL[k,1]
      j<-CL[k,2]
      dLijdmi <- Q1%*%mass1[j,] # length f
      Nablaijq <- dmdalpha[i,,] %*% dLijdmi * (1-gam[i])# length f
      dLijdmj <- Q1%*%mass1[i,] # length f
      Nablapijq <- dmdalpha[j,,] %*% dLijdmj * (1-gam[j]) # length f
      gradW2 <- gradW2 + Nablaijq%*%Z2[i,] + Nablapijq%*%Z2[j,]
      # Beta
      if(!is.null(fhat)){
        dLijdgi=sum(dLijdmi*(-mass[i,]+mass_empty)) # size (1,1)
        dLijdgj=sum(dLijdmj*(-mass[j,]+mass_empty)) # size (1,1)
        gradbeta2[1] <- gradbeta2[1]+dgdbeta0[i]*dLijdgi+dLijdgj*dgdbeta0[j]
        gradbeta2[2] <- gradbeta2[2]+dgdbeta1[i]*dLijdgi+dLijdgj*dgdbeta1[j]
      } #endif
      # Hidden-to-Hidden
      Nablaijh2<-(Z2[i,ii2]>0)* (t(Nablaijq)%*%W[,ii2])
      Nablapijh2<-(Z2[j,ii2]>0) * (t(Nablapijq)%*%W[,ii2])
      gradV2<-gradV2+t(Nablaijh2)%*%Z1[i,] + t(Nablapijh2)%*%Z1[j,]
      # Input-to-Hidden
      Nablaijh1<-(Z1[i,ii1]>0)* (Nablaijh2%*%V[,ii1])
      Nablapijh1<-(Z1[j,ii1]>0) * (Nablapijh2%*%V[,ii1])
      gradU2<-gradU2+t(Nablaijh1)%*%X[i,] + t(Nablapijh1)%*%X[j,]
    }
  }
  
  # Putting everything together
  gradU<-(1-nu)*gradU +  lambda*U/N_U
  gradV<-(1-nu)*gradV +  lambda*V/N_V
  gradW<-(1-nu)*gradW +  lambda*W/N_W
  gradbeta<- (1-nu)*gradbeta
  if(semi){
    gradU<-gradU + nu/(ns*c)*gradU1
    gradV<-gradV + nu/(ns*c)*gradV1
    gradW<-gradW + nu/(ns*c)*gradW1
    gradbeta<- gradbeta + nu/(ns*c) * gradbeta1
  }  
  if(pairwise){
    gradU<-gradU + rho1*gradU2
    gradV<-gradV + rho1*gradV2
    gradW<-gradW + rho1*gradW2
    gradbeta<- gradbeta + rho1 * gradbeta2
  }
  #  grad<-c(grada,gradb,as.vector(gradV),as.vector(gradW),gradbeta)
  grad<-c(as.vector(gradU),as.vector(gradV),as.vector(gradW),gradbeta)
  return(list(fun=S,grad=grad))
}



##################################################################
#### Version 7 July, 2020 - On line
foncgrad_online_part1<-function(theta,X,D,fhat,Xi)#,lambda)
  {
  n<-nrow(X)
  d<-ncol(X)
  N<-length(theta)
  f<-nrow(Xi)
  n_H<-(N-2-f)/(d+f)
  V<-matrix(theta[1:(n_H*d)],n_H,d)
  W<-matrix(theta[(n_H*d+1):(N-2)],f,n_H+1)
  beta<-theta[c(N-1,N)]
  
  # Propagation
  Zeros<-matrix(0,n,n_H)
  A<-X%*%t(V)
  Z<-cbind(rep(1,n),pmax(Zeros,A)) # size(n,n_H+1)
  alpha<-Z%*%t(W)
  mass<-exp(alpha-apply(alpha,1,max))
  mass<-mass/rowSums(mass)
  if(is.null(fhat)){
    mass1<-mass
    gam<-rep(0,n)
  } else{
    betafhat<-beta[1]+beta[2]*fhat
    #   eta<-pmax(0,betafhat)
    eta<-log(1+exp(betafhat))
    gam<-eta/(1+eta)
    mass1<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
    #    dgdbeta0<-1/(1+eta)^2*(betafhat > 0)
    dgdbeta0<-1/(1+eta)^2 * exp(betafhat)/(1+exp(betafhat))
    dgdbeta1<-dgdbeta0*fhat
  }
  mK<-mass1 %*% Xi
  K<- mK %*% t(mass1)
  error<-K-D
  diag(error)<-0
  C<-1/(n*(n-1))
  S<-C*sum(error^2) #+ 0.5*lambda*(mean(V^2)+mean(W^2))

  # Backpropagation
  gradW<-matrix(0,f,n_H+1) # term corresponding to Lij
  gradV<-matrix(0,n_H,d) # term corresponding to Lij
  gradbeta<-c(0,0)
  dmdalpha<-array(0,c(n,f,f))
  mass_empty<-c(1,rep(0,f-1))
  for(i in 1:n) dmdalpha[i,,]<- -mass[i,]%o%mass[i,] + diag(mass[i,])
  # Lij part
  ii<-2:(n_H+1)
  for(i in 1:n){
    # Hidden-to-output
    dKijdmi=mK[-i,] # size (n-1,f)
    Error<-matrix(error[i,-i],n-1,f)
    Deltaijq<-2*C*Error*(dKijdmi %*% dmdalpha[i,,])*(1-gam[i]) # size (n-1,f)
    dKijdmj=matrix(mK[i,],n-1,f,byrow=TRUE)
    B<-matrix(0,n-1,f)
    for(r in 1:f) B<-B+matrix(dKijdmj[,r],n-1,f)*dmdalpha[-i,r,]
    Deltapijq<-2*C*Error*B*matrix(1-gam[-i],n-1,f)
    gradW<- gradW+t(Deltaijq)%*%matrix(Z[i,],n-1,n_H+1,byrow=TRUE)+
      t(Deltapijq) %*% Z[-i,]
    # Beta
    if(!is.null(fhat)){
      
#     dJijdgi=2*C*error[i,]/w[i,]*(dKijdmi%*%(-mass[i,]+mass_empty)) # size (p,1)
#     dJijdgj=2*C*error[i,]/w[i,]*rowSums(dKijdmj * 
#                                           (-mass[J[i,],]+matrix(mass_empty,p,f,byrow=TRUE))) # size (p,1)
#     gradbeta[1] <- gradbeta[1]+dgdbeta0[i]*sum(dJijdgi)+sum(dJijdgj*dgdbeta0[J[i,]])
#     gradbeta[2] <- gradbeta[2]+dgdbeta1[i]*sum(dJijdgi)+sum(dJijdgj*dgdbeta1[J[i,]])
      
      
      dJijdgi=2*C*error[i,-i]*(dKijdmi%*%(-mass[i,]+mass_empty)) # size (n-1,1)
      dJijdgj=2*C*error[i,-i]*rowSums(dKijdmj * 
                  (-mass[-i,]+matrix(mass_empty,n-1,f,byrow=TRUE))) # size (n-1,1)
      gradbeta[1] <- gradbeta[1]+dgdbeta0[i]*sum(dJijdgi)+sum(dJijdgj*dgdbeta0[-i])
      gradbeta[2] <- gradbeta[2]+dgdbeta1[i]*sum(dJijdgi)+sum(dJijdgj*dgdbeta1[-i])
    } #endif
    # Input-to-Hidden
    Deltaijh<-matrix(Z[i,ii]>0,n-1,n_H,byrow=TRUE)* (Deltaijq%*%W[,ii])
    Deltapijh<-(Z[-i,ii]>0) * (Deltapijq%*%W[,ii])
    gradV<-gradV+t(Deltaijh)%*%matrix(X[i,],n-1,d,byrow=TRUE)+
           t(Deltapijh) %*% X[-i,]
  } # end for i

  gradV<- gradV #+  lambda*V/(n_H*d)
  gradW<- gradW #+  lambda*W/(f*(n_H+1))
  gradbeta<- gradbeta
  grad<-c(as.vector(gradV),as.vector(gradW),gradbeta)
  return(list(fun=S,grad=grad))
}

fonc_online_part1<-function(theta,X,D,fhat,Xi){
  n<-nrow(X)
  d<-ncol(X)
  N<-length(theta)
  f<-nrow(Xi)
  n_H<-(N-2-f)/(d+f)
  V<-matrix(theta[1:(n_H*d)],n_H,d)
  W<-matrix(theta[(n_H*d+1):(N-2)],f,n_H+1)
  beta<-theta[c(N-1,N)]
  
  # Propagation
  Zeros<-matrix(0,n,n_H)
  A<-X%*%t(V)
  Z<-cbind(rep(1,n),pmax(Zeros,A)) # size(n,n_H+1)
  alpha<-Z%*%t(W)
  mass<-exp(alpha-apply(alpha,1,max))
  mass<-mass/rowSums(mass)
  if(is.null(fhat)){
    mass1<-mass
    gam<-rep(0,n)
  } else{
    betafhat<-beta[1]+beta[2]*fhat
    #   eta<-pmax(0,betafhat)
    eta<-log(1+exp(betafhat))
    gam<-eta/(1+eta)
    mass1<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
  }
  mK<-mass1 %*% Xi
  K<- mK %*% t(mass1)
  error<-K-D
  diag(error)<-0
  C<-1/(n*(n-1))
  S<-C*sum(error^2) #+ 0.5*lambda*(mean(V^2)+mean(W^2))
}

#------------------------------------------------------------------------
# Part 2: semi-supervised with class labels

foncgrad_online_part2<-function(theta,X,fhat,F,y){
  n<-nrow(X)
  d<-ncol(X)
  N<-length(theta)
  f<-nrow(F)
  c<-ncol(F)
  n_H<-(N-2-f)/(d+f)
  V<-matrix(theta[1:(n_H*d)],n_H,d)
  W<-matrix(theta[(n_H*d+1):(N-2)],f,n_H+1)
  beta<-theta[c(N-1,N)]
  
  # Propagation
  Zeros<-matrix(0,n,n_H)
  A<-X%*%t(V)
  Z<-cbind(rep(1,n),pmax(Zeros,A)) # size(n,n_H+1)
  alpha<-Z%*%t(W)
  mass<-exp(alpha-apply(alpha,1,max))
  mass<-mass/rowSums(mass)
  if(is.null(fhat)){
    mass1<-mass
    gam<-rep(0,n)
  } else{
    betafhat<-beta[1]+beta[2]*fhat
    #   eta<-pmax(0,betafhat)
    eta<-log(1+exp(betafhat))
    gam<-eta/(1+eta)
    mass1<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
    #    dgdbeta0<-1/(1+eta)^2*(betafhat > 0)
    dgdbeta0<-1/(1+eta)^2 * exp(betafhat)/(1+exp(betafhat))
    dgdbeta1<-dgdbeta0*fhat
  }
  Id<-diag(c)
  Y<-Id[y,]
  pl1<-mass1%*%F
  S<-mean((pl1-Y)^2)
  
  # Backpropagation
  dmdalpha<-array(0,c(n,f,f))
  mass_empty<-c(1,rep(0,f-1))
  for(i in 1:n) dmdalpha[i,,]<- -mass[i,]%o%mass[i,] + diag(mass[i,])
 
  # Li part
    gradbeta<-c(0,0)
    # Hidden-to-output
    Deltaiq<-matrix(0,n,f)
    for(i in 1:n){
      dpldalpha<-(t(F) %*% dmdalpha[i,,])*(1-gam[i]) # size (c,f)
      Deltaiq[i,]<-2*(pl1[i,]-Y[i,]) %*% dpldalpha # size (1,f)
    } # endfor
    gradW<-t(Deltaiq)%*%Z # size (f,n_H+1)
    # Input-to-hidden
    Deltaih<-(Z[,2:(n_H+1)]>0)* (Deltaiq %*% W[,2:(n_H+1)])
    gradV<-t(Deltaih) %*% X
    # Beta
    if(!is.null(fhat)){
      dpldg<-matrix(0,n,c)
      for(i in 1:n) dpldg[i,]<-t(F)%*%(-mass[i,]+mass_empty)
      dLidg<-rowSums(2*(pl1-Y) * dpldg)
      gradbeta[1]<-dLidg %*% dgdbeta0
      gradbeta[2]<-dLidg %*% dgdbeta1
    } # if svm

  grad<-c(as.vector(gradV),as.vector(gradW),gradbeta)/(n*c)
  return(list(fun=S,grad=grad))
}

fonc_online_part2<-function(theta,X,fhat,F,y){
  n<-nrow(X)
  d<-ncol(X)
  N<-length(theta)
  f<-nrow(F)
  c<-ncol(F)
  n_H<-(N-2-f)/(d+f)
  V<-matrix(theta[1:(n_H*d)],n_H,d)
  W<-matrix(theta[(n_H*d+1):(N-2)],f,n_H+1)
  beta<-theta[c(N-1,N)]
  
  # Propagation
  Zeros<-matrix(0,n,n_H)
  A<-X%*%t(V)
  Z<-cbind(rep(1,n),pmax(Zeros,A)) # size(n,n_H+1)
  alpha<-Z%*%t(W)
  mass<-exp(alpha-apply(alpha,1,max))
  mass<-mass/rowSums(mass)
  if(is.null(fhat)){
    mass1<-mass
    gam<-rep(0,n)
  } else{
    betafhat<-beta[1]+beta[2]*fhat
    #   eta<-pmax(0,betafhat)
    eta<-log(1+exp(betafhat))
    gam<-eta/(1+eta)
    mass1<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
    #    dgdbeta0<-1/(1+eta)^2*(betafhat > 0)
    #dgdbeta0<-1/(1+eta)^2 * exp(betafhat)/(1+exp(betafhat))
    #dgdbeta1<-dgdbeta0*fhat
  }
  Id<-diag(c)
  Y<-Id[y,]
  pl1<-mass1%*%F
  S<-mean((pl1-Y)^2)
}

