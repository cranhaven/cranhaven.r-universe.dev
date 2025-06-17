FDA.historical2 <-
function(X1,X2,Y,delta.grid1=1,delta.grid2=1,intercept=TRUE,basis1=NULL,Ld2.basis1=NULL,basis2=NULL,Ld2.basis2=NULL,basis.type=c("fourier"),
                          nbasis1=3,nbasis2=3,timeinv=c(0,1),timegrids=NULL,lambda1=0.01,lambda2=0.01)
{
  N<-dim(X1)[1]             # # of subject
  ntp<-dim(X1)[2]           # # of time points
  
  # design matrix
  X.dim<-function(X)
  {
    q0<-dim(X)[3]
    if(is.na(q0))
    {
      if(intercept)
      {
        q<-2
        
        W<-array(NA,c(N,ntp,q))
        W[,,1]<-matrix(1,nrow=N,ncol=ntp)
        W[,,2]<-X
      }else
      {
        q<-1
        
        W<-array(NA,c(N,ntp,q))
        W[,,1]<-X
      }
    }else
      if(intercept)
      {
        q<-q0+1
        
        W<-array(NA,c(N,ntp,q))
        W[,,1]<-matrix(1,nrow=N,ncol=ntp)
        W[,,2:q]<-X
      }else
      {
        q<-q0
        W<-X
      }
    return(list(q=q,W=W))
  }
  X.dim.re1<-X.dim(X1)
  X.dim.re2<-X.dim(X2)
  
  q1<-X.dim.re1$q
  W1<-X.dim.re1$W
  q2<-X.dim.re2$q
  W2<-X.dim.re2$W
  
  q<-q1+q2
  
  # time grids
  if(is.null(timegrids))
  {
    timegrids<-seq(timeinv[1],timeinv[2],length.out=ntp)
  }
  
  
  # basis functions
  if(is.null(basis1))
  {
    if(basis.type[1]=="fourier")
    {
      basis1<-fourier.basis(timeinv=timeinv,ntp=ntp,nbasis=nbasis1)
      
      Ld2.basis1<-Ld2.fourier(timeinv=timeinv,ntp=ntp,nbasis=nbasis1)
    }
  }else
  {
    nbasis1<-ncol(basis1)
  }
  if(is.null(basis2))
  {
    if(basis.type[1]=="fourier")
    {
      basis2<-fourier.basis(timeinv=timeinv,ntp=ntp,nbasis=nbasis2)
      
      Ld2.basis2<-Ld2.fourier(timeinv=timeinv,ntp=ntp,nbasis=nbasis2)
    }
  }else
  {
    nbasis2<-ncol(basis2)
  }
  
  # lambda
  if(length(lambda1)==1)
  {
    lambda1<-rep(lambda1,q)
  }else
    if(length(lambda1)>q)
    {
      lambda1<-lambda1[1:q]
    }else
    {
      lambda1<-rep(lambda1[1],q)
    }
  if(length(lambda2)==1)
  {
    lambda2<-rep(lambda2,q)
  }else
    if(length(lambda2)>q)
    {
      lambda2<-lambda2[1:q]
    }else
    {
      lambda2<-rep(lambda2[1],q)
    }
  
  
  K1<-c(nbasis1*q1,nbasis1*q2)
  K2<-c(nbasis2*q1,nbasis2*q2)
  K<-c((nbasis1*nbasis2)*q1,(nbasis1*nbasis2)*q2)
  ######################################################
  D.func<-function(W,K1,K2,K,q,delta.grid)
  {
    U<-matrix(0,K,K)
    V<-matrix(0,K,K)
    D<-array(0,c(N,K,ntp))
    for(j in 1:q)
    {
      U1tmp<-apply(array(apply(basis2,1,function(x){return(x%*%t(x))}),c(nbasis2,nbasis2,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
      U2tmp<-apply(array(apply(Ld2.basis1,1,function(x){return(x%*%t(x))}),c(nbasis1,nbasis1,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
      U[((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2)),((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2))]<-lambda1[j]*kronecker(U1tmp,U2tmp)
      
      V1tmp<-apply(array(apply(Ld2.basis2,1,function(x){return(x%*%t(x))}),c(nbasis2,nbasis2,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
      V2tmp<-apply(array(apply(basis1,1,function(x){return(x%*%t(x))}),c(nbasis1,nbasis1,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
      V[((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2)),((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2))]<-lambda2[j]*kronecker(V1tmp,V2tmp)
      
      
      Ws<-array(NA,c(N,nbasis1,ntp))
      Wstmp<-array(NA,c(N,nbasis1,ntp))
      for(i in 1:ntp)
      {
        Wstmp[,,i]<-matrix(W[,i,j],nrow=N)%*%matrix(basis1[i,],ncol=nbasis1)
        
        rtmp<-max(i-delta.grid,1)
        
        Ws[,,i]<-apply(array(Wstmp[,,rtmp:i],c(N,nbasis1,length(rtmp:i))),c(1,2),int.func,timeinv=c(timegrids[rtmp],timegrids[i]),timegrids=timegrids[rtmp:i])
        
        D[,((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2)),i]<-kronecker(t(basis2[i,]),Ws[,,i])
      }
    }
    
    return(list(D=D,U=U,V=V))
  }
  re.D1<-D.func(W1,K1[1],K2[1],K[1],q1,delta.grid1)
  re.D2<-D.func(W2,K1[2],K2[2],K[2],q2,delta.grid2)
  
  D1<-re.D1$D
  D2<-re.D2$D
  
  D<-array(NA,c(dim(D1)[1],dim(D1)[2]+dim(D2)[2],dim(D1)[3]))
  D[,1:(dim(D1)[2]),]<-D1
  D[,(dim(D1)[2]+1):(dim(D1)[2]+dim(D2)[2]),]<-D2
  
  mat1<-array(NA,c(sum(K),sum(K),ntp))
  mat2<-array(NA,c(sum(K),1,ntp))
  for(i in 1:ntp)
  {
    mat1[,,i]<-t(D[,,i])%*%D[,,i]
    mat2[,,i]<-t(D[,,i])%*%Y[,i]
  }
  Q1<-apply(mat1,c(1,2),int.func,timeinv=timeinv,timegrids=timegrids)
  Q2<-apply(mat2,c(1,2),int.func,timeinv=timeinv,timegrids=timegrids)
  
  U1<-re.D1$U
  V1<-re.D1$V
  U2<-re.D2$U
  V2<-re.D2$V
  
  U=V<-matrix(0,sum(K),sum(K))
  U[1:K[1],1:K[1]]<-U1
  U[(K[1]+1):sum(K),(K[1]+1):sum(K)]<-U2
  V[1:K[1],1:K[1]]<-V1
  V[(K[1]+1):sum(K),(K[1]+1):sum(K)]<-V2
  
  g<-solve(Q1+U+V)%*%Q2
  g1<-g[1:K[1]]
  g2<-g[(K[1]+1):sum(K)]
  
  Gmat.func<-function(K1,K2,q,g)
  {
    Gmat<-matrix(0,K1,K2)
    for(j in 1:q)
    {
      Gmat[((j-1)*nbasis1+1):(j*nbasis1),((j-1)*nbasis2+1):(j*nbasis2)]<-matrix(g[((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2))],nbasis1,nbasis2)
    }
    
    return(Gmat)
  }
  Gmat1<-Gmat.func(K1[1],K2[1],q1,g1)
  Gmat2<-Gmat.func(K1[2],K2[2],q2,g2)
  
  Gmat<-matrix(0,dim(Gmat1)[1]+dim(Gmat2)[1],dim(Gmat1)[2]+dim(Gmat2)[2])
  Gmat[1:(dim(Gmat1)[1]),1:(dim(Gmat1)[2])]<-Gmat1
  Gmat[(dim(Gmat1)[1]+1):(dim(Gmat1)[1]+dim(Gmat2)[1]),(dim(Gmat1)[2]+1):(dim(Gmat1)[2]+dim(Gmat2)[2])]<-Gmat2
  ######################################################
  gamma.func<-function(Gmat,q)
  {
    gamma.est<-array(NA,c(ntp,ntp,q))
    for(j in 1:q)
    {
      for(u in 1:ntp)
      {
        for(s in 1:ntp)
        {
          gamma.est[u,s,j]<-t(basis1[u,])%*%Gmat[((j-1)*nbasis1+1):(j*nbasis1),((j-1)*nbasis2+1):(j*nbasis2)]%*%basis2[s,]
        }
      }
    }
    
    return(gamma.est)
  }
  gamma.est1<-gamma.func(Gmat1,q1)
  gamma.est2<-gamma.func(Gmat2,q2)
  
  gamma.est<-array(NA,c(ntp,ntp,q))
  gamma.est[,,1:q1]<-gamma.est1
  gamma.est[,,(q1+1):q]<-gamma.est2
  
  if(intercept)
  {
    dimnames(gamma.est)[[3]]<-c("Intercept",paste0("X",1:(q-1))) 
  }else
  {
    dimnames(gamma.est)<-list(NULL)
    dimnames(gamma.est)[[3]]<-paste0("X",1:q)
  }
  yfit<-apply(D,c(1,3),function(x){return(t(x)%*%g)})
  
  re<-list(coefficients=Gmat,coef.vec=g,coef1=Gmat1,coef2=Gmat2,basis1=basis1,basis2=basis2,gamma.curve=gamma.est,fitted=yfit)
  
  return(re)
}
