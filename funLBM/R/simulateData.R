simulateData <- function(n = 100, p = 100, t = 30){
  long = t
  x = seq(0,1,length.out=long)
  K = 4; L = 3
  
  A = sin(4*pi*x)
  B = 0.75-0.5*(x>0.7 & x<0.9)
  C = dnorm(x,0.2,0.02); C = C / max(C)
  D = sin(10*pi*x)
  fun = rbind(A,B,C,D)
  
  noise = 0.1 / 3
  mu = array(noise,c(K,L,4))
  mu[1,1,1] = 1-3*noise; mu[2,1,1] = 1-3*noise; mu[3,1,2] = 1-3*noise; mu[4,1,4] = 1-3*noise
  mu[1,2,2] = 1-3*noise; mu[2,2,2] = 1-3*noise; mu[3,2,3] = 1-3*noise; mu[4,2,1] = 1-3*noise
  mu[1,3,3] = 1-3*noise; mu[2,3,4] = 1-3*noise; mu[3,3,1] = 1-3*noise; mu[4,3,4] = 1-3*noise
  
  Z = rep(1:K,n*c(0.2,0.4,0.1,0.3))
  W = rep(1:L,p*c(0.4,0.3,0.3))
  X = array(NA,c(n,p,long))
  Y = matrix(NA,n,p)
  
  for (k in 1:K){
    for (l in 1:L){
      nkl = sum(Z==k)*sum(W==l)
      tkl = max.col(t(rmultinom(nkl,1,mu[k,l,])))
      X[Z==k,W==l,] = array(fun[tkl,]+t(replicate(nkl,rnorm(long,0,0.3))),c(sum(Z==k),sum(W==l),long))
      Y[Z==k,W==l] = matrix(tkl,nrow=sum(Z==k))
    }
  }
  indx = sample(nrow(X)); indy = sample(ncol(X))
  Z = Z[indx]; W = W[indy]
  X = X[indx,indy,]; Yo = Y; Y = Y[indx,indy]
  
  list(data=X,row_clust=Z,col_clust=W)
}