simulateData2 <- function(n = 100, p = 100, t = 30){

  long = t
  x = seq(0,1,length.out=long)
  K = 4; L = 3

  A = sin(4*pi*x)
  B = 0.75-0.5*(x>0.7 & x<0.9)
  C = dnorm(x,0.2,0.02); C = C / max(C)
  D = sin(10*pi*x)
  fun = rbind(A,B,C,D)

  A2 = cos(4*pi*x)
  B2 = 0.75-0.5*(x>0.2 & x<0.4)
  C2 = dnorm(x,0.2,0.05); C2 = C2 / max(C2)
  D2 = cos(10*pi*x)
  fun2 = rbind(A2,B2,C2,D2)

  noise = 0 / 3
  mu = array(noise,c(K,L,4))
  mu[1,1,1] = 1-3*noise; mu[2,1,1] = 1-3*noise; mu[3,1,2] = 1-3*noise; mu[4,1,4] = 1-3*noise
  mu[1,2,2] = 1-3*noise; mu[2,2,2] = 1-3*noise; mu[3,2,3] = 1-3*noise; mu[4,2,1] = 1-3*noise
  mu[1,3,3] = 1-3*noise; mu[2,3,4] = 1-3*noise; mu[3,3,1] = 1-3*noise; mu[4,3,4] = 1-3*noise

  Z = rep(1:K,n*c(0.2,0.4,0.1,0.3))
  W = rep(1:L,p*c(0.4,0.3,0.3))
  X = array(NA,c(n,p,long))
  X2 = array(NA,c(n,p,long))
  Y = matrix(NA,n,p)

  for (k in 1:K){
    for (l in 1:L){
      nkl = sum(Z==k)*sum(W==l)
      tkl = max.col(t(rmultinom(nkl,1,mu[k,l,])))
      X[Z==k,W==l,] = array(fun[tkl,]+t(replicate(nkl,rnorm(long,0,0.3))),c(sum(Z==k),sum(W==l),long))
      X2[Z==k,W==l,] = array(fun2[tkl,]+t(replicate(nkl,rnorm(long,0,0.3))),c(sum(Z==k),sum(W==l),long))
      Y[Z==k,W==l] = matrix(tkl,nrow=sum(Z==k))
    }
  }
  indx = sample(nrow(X)); indy = sample(ncol(X))
  Z = Z[indx]; W = W[indy]
  X = X[indx,indy,]; Yo = Y; Y = Y[indx,indy]
  X2 = X2[indx,indy,]
  list(data1=X,data2=X2,row_clust=Z,col_clust=W)
}
