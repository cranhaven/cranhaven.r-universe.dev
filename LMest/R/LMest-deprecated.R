bootstrap_lm_basic <- function(piv,Pi,Psi,n,B=100,start=0,mod=0,tol=10^-6){

    #        [lk,piv,Pi,Psi,np,aic,bic,lkv] = draw_lm_basic(piv,Pi,Psi,n)
    #
    # Bootstrap EM estimates piv, Pi and Psi
    .Deprecated(new = "bootstrap",package = "LMest")
    k = length(piv)
    c = dim(Psi)[1]
    TT = dim(Pi)[3]
    # Reparametrize
    mPsi = mpiv = mPi = 0
    m2Psi = m2piv = m2Pi = 0
    #    mth = 0; m2th = 0;
    for(b in 1:B){
      out = draw_lm_basic(piv,Pi,Psi,n)
      Sb = out$S; yvb = out$yv
      ns = dim(Sb)[1]
      out = est_lm_basic(Sb,yvb,k,start,mod,tol)
      mPsi = mPsi+out$Psi/B; mpiv = mpiv+out$piv/B; mPi = mPi+out$Pi/B
      m2Psi = m2Psi+out$Psi^2/B; m2piv = m2piv+out$piv^2/B; m2Pi = m2Pi+out$Pi^2/B
      #	    	mth = mth+out$th/B; m2th = m2th+out$th/B
    }
    sePsi = sqrt(m2Psi-mPsi^2); sepiv = sqrt(m2piv-mpiv^2); sePi = sqrt(m2Pi-mPi^2)
    #    seth = sqrt(m2th-mth^2)
    out = list(mPsi=mPsi,mpiv=mpiv,mPi=mPi,sePsi=sePsi,sepiv=sepiv,sePi=sePi)

  }
bootstrap_lm_basic_cont <-function(piv,Pi,Mu,Si,n,B=100,start=0,mod=0,tol=10^-6){

    # Bootstrap EM estimates piv, Pi, Mu and Si
    .Deprecated(new = "bootstrap",package = "LMest")

    # Preliminaries
    k = length(piv)
    # Reparametrize
    mMu = mSi = mpiv = mPi = 0
    m2Mu = m2Si=  m2piv = m2Pi = 0
    #    mth = 0; m2th = 0;
    for(b in 1:B){
      cat("boostrap sample n. ",b,"\n")
      out = draw_lm_basic_cont(piv,Pi,Mu,Si,n)
      Yb = out$Y
      out = est_lm_basic_cont(Yb,k,start,mod,tol)
      mMu = mMu + out$Mu/B
      mSi = mSi + out$Si/B
      mpiv = mpiv+out$piv/B
      mPi = mPi+out$Pi/B
      m2Mu = m2Mu + out$Mu^2/B; m2Si = m2Si + out$Si^2/B
      m2piv = m2piv+out$piv^2/B; m2Pi = m2Pi+out$Pi^2/B
    }
    seMu = sqrt(m2Mu - mMu^2)
    seSi = sqrt(m2Si - mSi^2)
    sepiv = sqrt(m2piv-mpiv^2); sePi = sqrt(m2Pi-mPi^2)
    out = list(mMu=mMu,mSi=mSi,mpiv=mpiv,mPi=mPi,seMu=seMu,seSi=seSi,sepiv=sepiv,sePi=sePi)

  }
bootstrap_lm_cov_latent <- function(X1,X2,param="multilogit",Psi,Be,Ga,B=100,fort=TRUE){
  .Deprecated(new = "bootstrap",package = "LMest")

  mPsi = 0
  mBe = 0
  m2Psi = 0
  m2Be = 0
  if(param=="multilogit"){
    mGa = 0
    m2Ga = 0
  }else if(param=="difflogit"){
    mGa = vector("list",2)
    m2Ga = vector("list",2)
    mGa[[1]] = matrix(0,dim(Ga[[1]]))
    mGa[[2]] = matrix(0,dim(Ga[[2]]))
    m2Ga[[1]] = matrix(0,dim(Ga[[1]]))
    m2Ga[[2]] = matrix(0,dim(Ga[[2]]))
  }

  dPsi = dim(Psi)
  if(length(dPsi)==1) k = 1
  else k = dPsi[2]
  for (b in 1:B) {
    cat("boostrap sample n. ",b,"\n")
    out = draw_lm_cov_latent(X1,X2,param,Psi,Be,Ga,fort=fort)
    Yb = out$Y
    out = est_lm_cov_latent(Yb,X1,X2,param=param,k=k,fort=fort)
    mPsi = mPsi + out$Psi/B
    mBe = mBe + out$Be/B
    if(param=="multilogit"){
      mGa = mGa + out$Ga/B
      m2Ga = m2Ga + out$Ga^2/B
    }else if(param=="difflogit"){
      mGa[[1]] = mGa[[1]]+out$Ga[[1]]/B
      mGa[[2]] = mGa[[2]]+out$Ga[[2]]/B
      m2Ga[[1]] = m2Ga[[1]] + out$Ga[[1]]^2/B
      m2Ga[[2]] = m2Ga[[2]] + out$Ga[[2]]^2/B
    }
    m2Psi = m2Psi + out$Psi^2/B
    m2Be = m2Be + out$Be^2/B

  }
  sePsi = sqrt(m2Psi - mPsi^2)
  seBe = sqrt(m2Be - mBe^2)
  if(param=="multilogit"){
    seGa = sqrt(m2Ga - mGa^2)
  }else if(param=="difflogit"){
    seGa = vector("list",2)
    seGa[[1]] = sqrt(m2Ga[[1]] - mGa[[1]]^2)
    seGa[[2]] = sqrt(m2Ga[[2]] - mGa[[2]]^2)
  }
  out = list(mPsi = mPsi, mBe = mBe, mGa = mGa, sePsi = sePsi,
             seBe = seBe, seGa = seGa)

}
bootstrap_lm_cov_latent_cont <- function(X1,X2,param="multilogit",Mu,Si,Be,Ga,B=100){
  .Deprecated(new = "bootstrap",package = "LMest")

  # preliminaries
  mMu = mSi = mBe = 0
  m2Mu = m2Si = m2Be = 0
  if(param=="multilogit"){
    mGa = 0
    m2Ga = 0
  }else if(param=="difflogit"){
    mGa = vector("list",2)
    m2Ga = vector("list",2)
    mGa[[1]] = matrix(0,dim(Ga[[1]]))
    mGa[[2]] = matrix(0,dim(Ga[[2]]))
    m2Ga[[1]] = matrix(0,dim(Ga[[1]]))
    m2Ga[[2]] = matrix(0,dim(Ga[[2]]))
  }

  if(is.vector(Mu)){
    r =1
    k = length(Mu)
  }else{
    r = nrow(Mu)
    k = ncol(Mu)
  }

  for (b in 1:B) {
    cat("boostrap sample n. ",b,"\n")
    out = draw_lm_cov_latent_cont(X1,X2,param,Mu,Si,Be,Ga)
    Yb = out$Y
    out = est_lm_cov_latent_cont(Yb,X1,X2,param=param,k=k)
    mMu = mMu + out$Mu/B
    mSi = mSi + out$Si/B
    mBe = mBe + out$Be/B
    if(param=="multilogit"){
      mGa = mGa + out$Ga/B
      m2Ga = m2Ga + out$Ga^2/B
    }else if(param=="difflogit"){
      mGa[[1]] = mGa[[1]]+out$Ga[[1]]/B
      mGa[[2]] = mGa[[2]]+out$Ga[[2]]/B
      m2Ga[[1]] = m2Ga[[1]] + out$Ga[[1]]^2/B
      m2Ga[[2]] = m2Ga[[2]] + out$Ga[[2]]^2/B
    }
    m2Mu = m2Mu + out$Mu^2/B
    m2Si = m2Si + out$Si^2/B
    m2Be = m2Be + out$Be^2/B

  }
  seMu = sqrt(m2Mu - mMu^2)
  seSi = sqrt(m2Si - mSi^2)
  seBe = sqrt(m2Be - mBe^2)
  if(param=="multilogit"){
    seGa = sqrt(m2Ga - mGa^2)
  }else if(param=="difflogit"){
    seGa = vector("list",2)
    seGa[[1]] = sqrt(m2Ga[[1]] - mGa[[1]]^2)
    seGa[[2]] = sqrt(m2Ga[[2]] - mGa[[2]]^2)
  }
  out = list(mMu = mMu, mSi = mSi, mBe = mBe, mGa = mGa,
             seMu = seMu, seSi = seSi, seBe = seBe, seGa = seGa)

}

# draw_lm_basic <- function(piv,Pi,Psi,n){
#
#   #        [lk,piv,Pi,Psi,np,aic,bic,lkv] = draw_lm_basic(piv,Pi,Psi,n)
#   #
#   # Draw a sample of size n from a Basic Latent Markov model with parameter piv, Pi and Psi
#   .Deprecated(new = "drawLMbasic",package = "LMest")
#
#   k = length(piv)
#   dd = dim(Psi)
#   c = apply(Psi, c(2,3), function(x) sum(!is.na(x)))[1,]
#   TT = dim(Pi)[3]
#   if (length(dd) > 2)
#     r = dd[3]
#   else r = 1
#   Y = matrix(0, n, TT * r)
#   cat("------------|\n")
#   cat(" sample unit|\n")
#   cat("------------|\n")
#   for (i in 1:n) {
#     if (i/1000 == floor(i/1000))
#       cat(sprintf("%11g", i), "\n", sep = " | ")
#     u = k + 1 - sum(runif(1) < cumsum(piv))
#     ind = 0
#     for (j in 1:r) {
#       ind = ind + 1
#       Y[i, ind] = c[j] - sum(runif(1) < cumsum(Psi[, u, j]), na.rm=T)
#     }
#     for (t in 2:TT) {
#       u = k + 1 - sum(runif(1) < cumsum(Pi[u, , t]))
#       for (j in 1:r) {
#         ind = ind + 1
#         Y[i, ind] = c[j] - sum(runif(1) < cumsum(Psi[,
#                                                      u, j]), na.rm=T)
#       }
#     }
#   }
#   cat("------------|\n")
#   out = aggr_data(Y)
#   S = out$data_dis
#   yv = out$freq
#   S = array(t(S), c(r, TT, length(yv)))
#   S = aperm(S)
#   if (r == 1)
#     S = S[, , 1]
#   out = list(Y = Y, S = S, yv = yv)
#
#   return(out)
# }
# draw_lm_basic_cont <-function(piv,Pi,Mu,Si,n){
#
#     #        [Y,yv] = draw_lm_basic(piv,Pi,Mu,Si,n)
#     #
#     # Draw a sample of size n from a Basic Latent Markov model for continuous data with parameter piv, Pi, Mu and Si
#     .Deprecated(new = "drawLMbasiccont",package = "LMest")
#
#     # Preliminaries
#     if(is.vector(Mu)){
#       r = 1
#       k = length(Mu)
#       Mu = matrix(Mu,r,k)
#     }else{
#       r = nrow(Mu)
#       k = ncol(Mu)
#     }
#     TT = dim(Pi)[3]
#     if(r==1) Si = matrix(Si,r,r)
#     # For each subject
#     Y = array(0,c(n,TT,r))
#     cat("------------|\n")
#     cat(" sample unit|\n")
#     cat("------------|\n")
#     for(i in 1:n){
#       if(i/1000==floor(i/1000)) cat(sprintf("%11g",i),"\n",sep=" | ")
#       u = k+1-sum(runif(1)<cumsum(piv))
#       Y[i,1,] = rmvnorm(1,Mu[,u],Si)
#
#       for(t in 2:TT){
#         u = k+1-sum(runif(1)<cumsum(Pi[u,,t]))
#         Y[i,t,] = rmvnorm(1,Mu[,u],Si)
#       }
#     }
#     cat("------------|\n")
#     out = list(Y=Y)
#   }
# draw_lm_cov_latent <- function(X1,X2,param="multilogit",Psi,Be,Ga,fort=TRUE){
#
#   # Draw a sample from LM model with covariates
#   # param = type of parametrization for the transition probabilities:
#   #         multilogit = standard multinomial logit for every row of the transition matrix
#   #         difflogit  = multinomial logit based on the difference between two sets of parameters
#   # fort  = fortran use (FALSE for not use fortran)
#   # X1    = design matrix for the initial probabilities (n by n.cov.)
#   # X2    = design matrix for the initial probabilities (n by TT-1 by n.cov.)
#   .Deprecated(new = "drawLMlatent",package = "LMest")
#
#   # Preliminaries
#   n = nrow(X2)
#   TT = dim(X2)[2]+1
#   dPsi = dim(Psi)
#   if(length(dPsi)==2) r = 1
#   else r = dPsi[3]
#   if(length(dPsi)==1) k = 1
#   else k = dPsi[2]
#   if(length(dPsi)==2) Psi = array(Psi,c(dPsi,1))
#   if(r==1){
#     b=dim(Psi)[1]-1
#   }else{
#     b = rep(0,r)
#     for(j in 1:r) b[j] = sum(!is.na(Psi[,1,j]))-1
#   }
#   # Covariate structure and related matrices: initial probabilities
#   if(is.vector(X1)) X1 = matrix(X1,n,1)
#   nc1 = dim(X1)[2] # number of covariates on the initial probabilities
#   if(k == 2) GBe = as.matrix(c(0,1)) else{
#     GBe = diag(k); GBe = GBe[,-1]
#   }
#   out = aggr_data(X1)
#   Xdis = out$data_dis
#   if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
#   Xlab = out$label
#   Xndis = max(Xlab)
#   XXdis = array(0,c(k,(k-1)*(nc1+1),Xndis))
#   for(i in 1:Xndis){
#     xdis = c(1,Xdis[i,])
#     XXdis[,,i] = GBe%*%(diag(k-1)%x%t(xdis))
#   }
#
#   # for the transition probabilities
#   if(is.matrix(X2)) X2 = array(X2,c(n,TT-1,1))
#   nc2 = dim(X2)[3] # number of covariates on the transition probabilities
#   Z = NULL
#   for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
#   if(nc2==1) Z = as.vector(X2)
#   out = aggr_data(Z); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
#   if(param=="multilogit"){
#     ZZdis = array(0,c(k,(k-1)*(nc2+1),Zndis,k))
#     for(h in 1:k){
#       if(k==2){
#         if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
#       }else{
#         GGa = diag(k); GGa = GGa[,-h]
#       }
#       for(i in 1:Zndis){
#         zdis = c(1,Zdis[i,])
#         ZZdis[,,i,h] = GGa%*%(diag(k-1)%x%t(zdis))
#       }
#     }
#   }else if(param=="difflogit"){
#     Zlab = (((Zlab-1)*k)%x%rep(1,k))+rep(1,n*(TT-1))%x%(1:k)
#     ZZdis = array(0,c(k,k*(k-1)+(k-1)*nc2,Zndis*k))
#     j = 0
#     for(i in 1:Zndis){
#       for(h in 1:k){
#         j = j+1
#         if(k==2){
#           if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
#         }else{
#           GGa = diag(k); GGa = GGa[,-h]
#         }
#         u = matrix(0,1,k); u[1,h] = 1
#         U = diag(k); U[,h] = U[,h]-1
#         U = U[,-1]
#         ZZdis[,,j] = cbind(u%x%GGa,U%x%t(Zdis[i,]))
#       }
#     }
#   }
#
#   # When there is just 1 latent class
#   Y = array(0,c(n,TT,r))
#   if(k == 1){
#     U = array(1,n,TT)
#     for(i in 1:n) for(t in 1:TT){
#       if(r==1){
#         Y[i,t] = which(rmultinom(1,1,Psi)==1)-1
#       }else{
#         for (j in 1:r) Y[i,t,j] = which(rmultinom(1,1,Psi[1:(b[j]+1),j])==1)-1
#       }
#     }
#   }else{
#     # parameters on initial probabilities
#     U = matrix(0,n,TT)
#     be = as.vector(Be)
#     out = prob_multilogit(XXdis,be,Xlab,fort)
#     Piv = out$P
#     for(i in 1:n) U[i,1] = which(rmultinom(1,1,Piv[i,])==1)
#
#     # parameters on transition probabilities
#     if(param=="multilogit"){
#       if(is.list(Ga)) stop("invalid mode (list) for Ga")
#       Ga = matrix(Ga,(nc2+1)*(k-1),k)
#       PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,n,TT))
#       for(h in 1:k){
#         out = prob_multilogit(ZZdis[,,,h],Ga[,h],Zlab,fort)
#         PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1))
#       }
#     }else if(param=="difflogit"){
#       if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
#       if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
#       PI = array(0,c(k,k,n,TT))
#       out = prob_multilogit(ZZdis,Ga,Zlab,fort)
#       Tmp = array(out$P,c(k,n,TT-1,k))
#       PI[,,,2:TT] = aperm(Tmp,c(1,4,2,3))
#     }
#     for(i in 1:n) for(t in 2:TT){
#       U[i,t] = which(rmultinom(1,1,PI[U[i,t-1],,i,t])==1)
#     }
#     for(i in 1:n) for(t in 1:TT) for(j in 1:r){
#       Y[i,t,j] = which(rmultinom(1,1,Psi[1:(b[j]+1),U[i,t],j])==1)-1
#     }
#   }
#   # output
#   if(r==1) Y = matrix(Y,n,TT)
#   out = list(U=U,Y=Y)
#
# }
# draw_lm_cov_latent_cont <- function(X1,X2,param="multilogit",Mu,Si,Be,Ga,fort=TRUE){
#
#   # Draw a sample from LM model with covariates
#   # X1    = design matrix for the initial probabilities (n x n.cov.)
#   # X2    = design matrix for the initial probabilities (n x TT-1 x n.cov.)
#   # param = type of parametrization for the transition probabilities:
#   #         multilogit = standard multinomial logit for every row of the transition matrix
#   #         difflogit  = multinomial logit based on the difference between two sets of parameters
#   # Mu = matrix of means (r x k)
#   # Si = common variance-covariance matrix
#   # Be = parameters on the initial probabilities (if start=2)
#   # Ga = parameters on the transition probabilities (if start=2)
#   # fort  = fortran use (FALSE for not use fortran)
#   .Deprecated(new = "drawLMlatentcont",package = "LMest")
#
#   # Preliminaries
#   n = nrow(X2)
#   TT = dim(X2)[2]+1
#   if(is.vector(Mu)){
#     r = 1
#     k = length(Mu)
#     Mu = matrix(Mu,r,k)
#     Si = matrix(Si,r,r)
#   }else{
#     r = nrow(Mu)
#     k = ncol(Mu)
#   }
#
#   # Covariate structure and related matrices: initial probabilities
#   if(is.vector(X1)) X1 = matrix(X1,n,1)
#   nc1 = dim(X1)[2] # number of covariates on the initial probabilities
#   if(k == 2){
#     GBe = as.matrix(c(0,1))
#   }else{
#     GBe = diag(k); GBe = GBe[,-1]
#   }
#   out = aggr_data(X1)
#   Xdis = out$data_dis
#   if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
#   Xlab = out$label
#   Xndis = max(Xlab)
#   XXdis = array(0,c(k,(k-1)*(nc1+1),Xndis))
#   for(i in 1:Xndis){
#     xdis = c(1,Xdis[i,])
#     XXdis[,,i] = GBe%*%(diag(k-1)%x%t(xdis))
#   }
#
#   # for the transition probabilities
#   if(is.matrix(X2)) X2 = array(X2,c(n,TT-1,1))
#   nc2 = dim(X2)[3] # number of covariates on the transition probabilities
#   Z = NULL
#   for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
#   if(nc2==1) Z = as.vector(X2)
#   out = aggr_data(Z); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
#   if(param=="multilogit"){
#     ZZdis = array(0,c(k,(k-1)*(nc2+1),Zndis,k))
#     for(h in 1:k){
#       if(k==2){
#         if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
#       }else{
#         GGa = diag(k); GGa = GGa[,-h]
#       }
#       for(i in 1:Zndis){
#         zdis = c(1,Zdis[i,])
#         ZZdis[,,i,h] = GGa%*%(diag(k-1)%x%t(zdis))
#       }
#     }
#   }else if(param=="difflogit"){
#     Zlab = (((Zlab-1)*k)%x%rep(1,k))+rep(1,n*(TT-1))%x%(1:k)
#     ZZdis = array(0,c(k,k*(k-1)+(k-1)*nc2,Zndis*k))
#     j = 0
#     for(i in 1:Zndis){
#       for(h in 1:k){
#         j = j+1
#         if(k==2){
#           if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
#         }else{
#           GGa = diag(k); GGa = GGa[,-h]
#         }
#         u = matrix(0,1,k); u[1,h] = 1
#         U = diag(k); U[,h] = U[,h]-1
#         U = U[,-1]
#         ZZdis[,,j] = cbind(u%x%GGa,U%x%t(Zdis[i,]))
#       }
#     }
#   }
#
#   # Draw data
#   Y = array(0,c(n,TT,r))
#   U = matrix(0,n,TT)
#
#   # first time occasion
#   be = as.vector(Be)
#   out = prob_multilogit(XXdis,be,Xlab,fort)
#   Piv = out$P
#   for(i in 1:n) U[i,1] = which(rmultinom(1,1,Piv[i,])==1)
#
#   # following time occasions
#   if(param=="multilogit"){
#     if(is.list(Ga)) stop("invalid mode (list) for Ga")
#     Ga = matrix(Ga,(nc2+1)*(k-1),k)
#     PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,n,TT))
#     for(h in 1:k){
#       out = prob_multilogit(ZZdis[,,,h],Ga[,h],Zlab,fort)
#       PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1))
#     }
#   }else if(param=="difflogit"){
#     if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
#     if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
#     PI = array(0,c(k,k,n,TT))
#     out = prob_multilogit(ZZdis,Ga,Zlab,fort)
#     Tmp = array(out$P,c(k,n,TT-1,k))
#     PI[,,,2:TT] = aperm(Tmp,c(1,4,2,3))
#   }
#   for(i in 1:n) for(t in 2:TT){
#     U[i,t] = which(rmultinom(1,1,PI[U[i,t-1],,i,t])==1)
#   }
#
#   # draw response variables
#   for(i in 1:n) for(t in 1:TT) Y[i,t,] = rmvnorm(1,Mu[,U[i,t]],Si)
#
#   # output
#   if(r==1) Y = matrix(Y,n,TT)
#   out = list(U=U,Y=Y)
#
# }
# draw_lm_mixed <- function(la,Piv,Pi,Psi,n,TT){
#
#   #        [Y,S,yv] = draw_lm_mixed(la,Piv,Pi,Psi,n,TT)
#   #
#   # Draw a sample of size n from a mixed Latent Markov model with specific parameters
#   # Preliminaries
#   .Deprecated(new = "drawLMmixed",package = "LMest")
#
#   k1 = length(la)
#   k2 = nrow(Piv)
#   dd = dim(Psi)
#   l = apply(Psi, c(2,3), function(x) sum(!is.na(x)))[1,]
#   if(length(dd)>2) r = dd[3] else r = 1
#   Psi = array(Psi,c(l,k2,r))
#   # # For each subject
#   Y = matrix(0,n,TT*r)
#   cat("------------|\n")
#   cat(" sample unit|\n")
#   cat("------------|\n")
#   for(i in 1:n){
#     if(i/100==floor(i/100)) cat(sprintf("%11g",i),"\n",sep=" | ")
#     u = k1+1-sum(runif(1)<cumsum(la))
#     v = k2+1-sum(runif(1)<cumsum(Piv[,u]))
#     ind = 0
#     for(j in 1:r){
#       ind = ind+1
#       Y[i,ind] = l[j]-sum(runif(1)<cumsum(Psi[,v,j]), na.rm=T)
#     }
#     for(t in 2:TT){
#       v = k2+1-sum(runif(1)<cumsum(Pi[v,,u])) #check se ok k2
#       for(j in 1:r){
#         ind = ind+1
#         Y[i,ind] = l[j]-sum(runif(1)<cumsum(Psi[,v,j]), na.rm=T)
#       }
#     }
#   }
#   cat("------------|\n")
#   out = aggr_data(Y)
#   S = out$data_dis; yv = out$freq
#   S = array(t(S),c(r,TT,length(yv)))
#   S = aperm(S)
#   if(r==1) S = S[,,1]
#   out = list(Y=Y,S=S,yv=yv)
#   return(out)
# }

