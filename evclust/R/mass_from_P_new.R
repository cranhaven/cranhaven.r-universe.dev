mass_from_P_new<-function(P,c,type,pairs=NULL,Omega=TRUE,epsi=1e-4,
                          maxit=100,disp=FALSE){
  
# Generation of a credal partition from bootstrap confidence intervals. 
# Function called by bootclus
  
  F<-makeF(c,type=type,pairs=pairs,Omega=Omega)
  F<-F[-1,]
  f<-nrow(F)
  M1<-as.matrix(P$M1)
  M0<-as.matrix(P$M0)
  n<-nrow(M1)
  
  Mat<-build_matrices(F)
  B<-rbind(Mat$S,Mat$C)
  
  # Mass initialization
 
  m <- matrix(runif(n*f),n,f)
  m<-m/rowSums(m)

  I<-diag(2)
  A<-vector(mode = "list", length = n)
  SAJA<-0
  for(j in 1:n){
    A[[j]]<-I %x% t(m[j,])
    SAJA<-SAJA+t(A[[j]])%*%A[[j]]
  }
  gain<-1
  k<-0
  mtimesA<-function(m,A) return(m%*%A)
  Amat<-t(rbind(rep(1,f),diag(f)))
  b<-c(1,rep(0,f))
  Spred<-10
  while((gain>epsi) & (k<=maxit)){
    S<-0
    k<-k+1
    for(i in 1:n){
      Mstar<-cbind(M1[i,],M0[i,])
      Mstar<-split(Mstar, row(Mstar))
      d<-mapply(mtimesA,Mstar,A)
      d<-as.vector(rowSums(d[,-i])%*%B)
      SAJA<-SAJA-t(A[[i]])%*%A[[i]]
      Q<-t(B)%*%(SAJA)%*%B
      if((c==2) | (Omega==TRUE)){
        d[f]<--1
        Q[,f]<-rep(1,f)
      }
      if(all(eigen(Q,symmetric=TRUE,only.values=TRUE)$values>0)){
        qp<-solve.QP(Dmat=Q, dvec=d, Amat=Amat, bvec=b,meq=1)
        m[i,]<-qp$solution
        S<-S+qp$value
      }else print('Negative Q!')
      A[[i]]<-I %x% t(m[i,])
      SAJA<-SAJA+t(A[[i]])%*%A[[i]]
    }  # end of 'for' loop on objects
    if(disp) print(c(k,S,gain))
    gain <- 0.5*gain+0.5*abs(Spred-S)/(1e-9+abs(S))
    Spred<-S
  }  # end of while loop on epochs
  m<-abs(m)
  m<-m/rowSums(m)
  Clus<-extractMass(m,F,method='mass_from_P',crit=S)
  Phat<-pairwise_mass(Clus)
  BelPl<-array(0,c(2,n,n))
  BelPl[1,,]<-as.matrix(Phat$M1)
  BelPl[2,,]<-1-as.matrix(Phat$M0)
  return(list(Clus=Clus,BelPl=BelPl)) 
}