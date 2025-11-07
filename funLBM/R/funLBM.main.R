funLBM.main <-
  function(X,K,L,maxit=100,burn=50,basis.name='fourier',nbasis=15,gibbs.it=5,display=FALSE,init='kmeans',...){
    # This function implements a SEM-Gibbs algorithm for the LFBM

    if (class(X)!="list"){
      N = dim(X)[1]; p = dim(X)[2]; T = dim(X)[3];
    }else{
    N = dim(X[[1]])[1]; p = dim(X[[1]])[2]; T = dim(X[[1]])[3];
    }
    # Computing functional coefs
    if (basis.name == 'spline') basis <- create.bspline.basis(c(0,T),nbasis)
    else if (basis.name == 'fourier') basis <- create.fourier.basis(c(0,T),nbasis)
    else stop('Unavailable basis functions!')
    nbasis = basis$nbasis

    if (class(X)!="list"){
      Xcol = apply(X,3,cbind)
      obj = smooth.basis(1:T,t(Xcol),basis)$fd
      Coefs3 = t(obj$coefs)
      C_tot = array(NA,c(N,p,nbasis))
      for(j in 1:p) C_tot[,j,] = as.matrix(Coefs3[((j-1)*N+1):(j*N),])
      X_tot<-X
    }else{
    obj<-list()
    fus<-list()
    Coefs2<-list()
    for (i in 1:length(X)){
      X_sel<-X[[i]]
      Xcol = apply(X_sel,3,cbind)
      obj_s = smooth.basis(1:T,t(Xcol),basis)$fd
      Coefs = t(obj_s$coefs)
      C = array(NA,c(N,p,nbasis))
      for(j in 1:p) C[,j,] = as.matrix(Coefs[((j-1)*N+1):(j*N),])
      fus[[i]]<-C
      obj[[i]]<-obj_s
      Coefs2[[i]]<-Coefs
    }
    #library(abind)
    C_tot<-abind(fus,along=3)
    X_tot<-abind(X,along=3)
    Coefs3<-abind(Coefs2,along=2)
    }

    #######Kmeans initialization with a fraction of data in order to have true different init of mu parameter
if (init=="kmeans"){
    if (class(X)!="list"){
    z1<-sample(1:dim(X)[1],size=0.3*(dim(X)[1]),replace = FALSE)
    z2<-sample(1:dim(X)[2],size=0.3*(dim(X)[2]),replace = FALSE)
    Xcol_ech = apply(X[z1,z2,],3,cbind)
    obj_s = smooth.basis(1:T,t(Xcol),basis)$fd
    Coefs3_ech = t(obj$coefs)

    }else{
        z1<-sample(1:dim(X[[1]])[1],size=0.3*(dim(X[[1]])[1]),replace = FALSE)
        z2<-sample(1:dim(X[[1]])[2],size=0.3*(dim(X[[1]])[2]),replace = FALSE)

        Coefs2_ech<-list()
        for (i in 1:length(X)){
          X_sel<-X[[i]][z1,z2,]
          Xcol = apply(X_sel,3,cbind)
          obj_s = smooth.basis(1:T,t(Xcol),basis)$fd
          Coefs = t(obj_s$coefs)
          Coefs2_ech[[i]]<-Coefs
        }
        #library(abind)
        Coefs3_ech<-abind(Coefs2_ech,along=2)

      }
}


    # Parameter initialization
    alpha = rep(1/K,K)
    beta = rep(1/L,L)
    if (init=='funFEM'){
      if (display) cat('Initialization: FunFEM...\n')
      if (basis.name == 'spline') basisfunFEM <- create.bspline.basis(c(0,p*T),nbasis)
      else if (basis.name == 'fourier') basisfunFEM <- create.fourier.basis(c(0,p*T),nbasis)
        if (class(X)!="list"){
      Z = t(apply(funFEM(smooth.basis(1:(p*T),apply(X,1,cbind),basisfunFEM)$fd,K,maxit=10)$P,1,function(x) (x>=max(x))+0))
        }else{
      Z = t(apply(funFEM(smooth.basis(1:(p*T),apply(X[[1]],1,cbind),basisfunFEM)$fd,K,maxit=10)$P,1,function(x) (x>=max(x))+0))
        }
      }else if (init=='kmeans'){
      if (display) cat('Initialization: kmeans...\n');
      cls = kmeans(t(apply(X_tot,1,cbind)),K)$cluster  #initialisation sur les coefs ou les data?
      Z = dummy(cls,K)
      # }else if (init=="funHDDC"){
      # if (basis.name == 'spline') basisf <- create.bspline.basis(c(0,p*T),nbasis)
      # else if (basis.name == 'fourier') basisf <- create.fourier.basis(c(0,p*T),nbasis)
      # if (class(X)!="list"){
      # cls=funHDDC(smooth.basis(1:(p*T),apply(X,1,cbind),basisf)$fd,K)$class
      # Z=dummy(cls,K)
      # }else{
      # Xb=list()
      # for (b in 1:length(X)){
      # Xb[[b]]=smooth.basis(1:(p*T),apply(X[[b]],1,cbind),basisf)$fd
      # }
      # cls=funHDDC(Xb,K)$class
      # Z=dummy(cls,K)
      # }
    }else {cat('Initialization: random...\n');Z = t(rmultinom(N,1,alpha))}
    Z = empty.class.check(Z)
    if (display){cat('Z:'); print(colSums(Z))}

    if (init=='funFEM'){
      if (basis.name == 'spline') basisfunFEM <- create.bspline.basis(c(0,N*T),nbasis)
      else if (basis.name == 'fourier') basisfunFEM <- create.fourier.basis(c(0,N*T),nbasis)
       if (class(X)!="list"){
      W = t(apply(funFEM(smooth.basis(1:(N*T),apply(X,2,cbind),basisfunFEM)$fd,L,maxit=10)$P,1,function(x) (x>=max(x))+0))
       } else{
      W = t(apply(funFEM(smooth.basis(1:(N*T),apply(X[[1]],2,cbind),basisfunFEM)$fd,L,maxit=10)$P,1,function(x) (x>=max(x))+0))
       }

    } else if (init=='kmeans'){cls = kmeans(t(apply(X_tot,2,cbind)),L)$cluster; W = dummy(cls,L)
    } else {W = t(rmultinom(p,1,beta))}
    W = empty.class.check(W)
    if (display){cat('W:'); print(colSums(W))}
    Q = rep(list(list()),K)
    Wmat = rep(list(list()),K)
    if (class(X)!="list"){
    mu = array(NA,c(K,L,nbasis))
    }else{
    mu = array(NA,c(K,L,(length(X)*nbasis)))
    }
    #Initialization "fuzzy" of mu parameter
    if (init=="kmeans"){
      for (l in 1:L) mu[,l,] = matrix(rep(colMeans(Coefs3_ech),K)+rnorm(K*nbasis,0,0.01),nrow=K,byrow = TRUE)
    }else{
    for (l in 1:L) mu[,l,] = matrix(rep(colMeans(Coefs3),K)+rnorm(K*nbasis,0,0.01),nrow=K,byrow = TRUE)
    }
    a = b = d = matrix(NA,K,L)

    # Parameter storage
    lik = rep(NA,maxit)
    Alphas = matrix(NA,maxit,K); Betas = matrix(NA,maxit,L)
    Alphas[1,] = alpha; Betas[1,] = beta
    Zs = matrix(NA,N,maxit); Ws = matrix(NA,p,maxit)
    Zs[,1] = max.col(Z); Ws[,1] = max.col(W)

    for (it in 1:maxit){
      if (display) cat('.')
      # M step for a specific block
      for (k in 1:K){
        for (l in 1:L){
          if (sum(W[,l])==1 | sum(Z[,k])==1){x = C_tot[Z[,k]==1,W[,l]==1,]}
          else {x = apply(C_tot[Z[,k]==1,W[,l]==1,],3,cbind)}
          if (is.vector(x)) x = t(x)
          alpha[k] = sum(Z[,k]==1) / N
          alpha[alpha<0.05] = 0.05
          beta[l] = sum(W[,l]==1) / p
          beta[beta<0.05] = 0.05
          mu[k,l,] = colMeans(x)

          if (class(X)!="list"){
            obj$coefs = t(x)
          }else{
          for (f in 0:(length(X)-1)){
            obj[[f+1]]$coefs = t(x[,((f*nbasis+1):((f+1)*nbasis))])
          }
          }
          dc = tryCatch(mypca.fd(obj),error=function(e){stop("One class is empty, re-run the algorithm")})
          d[k,l] = cattell(dc$values)
          a[k,l] = mean(dc$values[1:d[k,l]])
          b[k,l] = mean(dc$values[(d[k,l]+1):length(dc$values)])
          Q[[k]][[l]] = dc$U[,1:d[k,l]]
          Wmat[[k]][[l]]=dc$Wmat
        }
      }

      # SE step
      P = array(NA,c(N,K,L))
      for (r in 1:gibbs.it){
        # Z part
        Pz = estep.Z(C_tot,alpha,beta,mu,a,b,d,Q,W,Wmat)
        Z = tryCatch(t(apply(Pz,1,function(x){rmultinom(1,1,x)})),error=function(e){stop("One class is empty, re-run the algorithm")})
        Z = empty.class.check(Z,Pz)
        if (display){cat('Z:'); print(colSums(Z))}

        # W part
        Pw = estep.W(C_tot,alpha,beta,mu,a,b,d,Q,Z,Wmat)
        W = tryCatch(t(apply(Pw,1,function(x){rmultinom(1,1,x)})),error=function(e){stop("One class is empty, re-run the algorithm")})
        Z = empty.class.check(Z,Pz)
        W = empty.class.check(W,Pw)
        if (display){cat('W:'); print(colSums(W))}
        if (min(colSums(W))<1||min(colSums(Z))<1) stop("One class is empty, re-run the algorithm or choose an other number of clusters")
      }

      # Computing complete likelihood and parameter storage
      lik[it] = compute.CompleteLikelihood(C_tot,alpha,beta,mu,a,b,d,Q,Z,W,Wmat)
      Alphas[it,] = alpha; Betas[it,] = beta
      Zs[,it] = max.col(Z); Ws[,it] = max.col(W)

      # Test for early ending
      if (burn > 5 & it > burn) if (sum(abs(diff(lik[it:(it-5)]))) < 1e-6){
        burn = it - 5
        break
      }
    }
    if (display) cat('\n')

    # Averaging and computing MAP parameters
    alpha = colMeans(Alphas[burn:it,])
    beta = colMeans(Betas[burn:it,])
    Z = dummy(apply(Zs[,burn:it],1,Mode),K)
    Z = empty.class.check(Z,Pz)
    W = dummy(apply(Ws[,burn:it],1,Mode),L)
    W = empty.class.check(W,Pw)
    for (k in 1:K){
      for (l in 1:L){
        if (sum(W[,l])==1 | sum(Z[,k])==1){x = C_tot[Z[,k]==1,W[,l]==1,]}
        else {x = apply(C_tot[Z[,k]==1,W[,l]==1,],3,cbind)}
        mu[k,l,] = colMeans(x)

        if (class(X)!="list"){
          obj$coefs = t(x)
        }else{
          for (q in 0:(length(X)-1)){
            obj[[q+1]]$coefs = t(x[,((q*nbasis+1):((q+1)*nbasis))])
          }
        }

        dc = tryCatch(mypca.fd(obj),error=function(e){stop("One class is empty, re-run the algorithm")})
        d[k,l] = cattell(dc$values)
        a[k,l] = mean(dc$values[1:d[k,l]])
        b[k,l] = mean(dc$values[(d[k,l]+1):length(dc$values)])
        Q[[k]][[l]] = dc$U[,1:d[k,l]]
        Wmat[[k]][[l]]<-dc$Wmat
      }
    }
    b[b<1e-6] <- 1e-6
    #ICL-BIC criterion
    if (class(X)!="list"){
      #p2=p
      nbasis2=nbasis
    }else{
      #p2=length(X)*p
      nbasis2=length(X)*nbasis
    }
    nu = K*L*(nbasis2 +  1+1) + sum(d*(nbasis2-(d+1)/2))
    crit = compute.CompleteLikelihood(C_tot,alpha,beta,mu,a,b,d,Q,Z,W,Wmat) - (K-1)/2*log(N) - (L-1)/2*log(p) - nu/2*log(N*p)

    # Return results
    prms = list(alpha=alpha,beta=beta,mu=mu,a=a,b=b,d=d,Q=Q)
    allPrms = list(Alphas=Alphas[1:it,],Betas=Betas[1:it,],Zs=Zs[,1:it],Ws=Ws[,1:it])
    out = list(basisName=basis.name,nbasis=nbasis,T=T,K=K,L=L,prms=prms,Z=Z,W=W,
               row_clust=max.col(Z),col_clust=max.col(W),allPrms=allPrms,loglik=lik,icl=crit)
    class(out) = "funLBM"
    out
  }
