  set.seed(12345)

  n=200
  p=4
  q=2
  mu=rep(0,p)
  x = mvtnorm::rmvnorm(n, mu)
  beta<-sapply(1:q, function(k) c(mvtnorm::rmvnorm(1,mu)))
  y = x%*%beta + t(mvtnorm::rmvnorm(q,1:n))
  x0=matrix(x[n,],nrow=1)
  y0=matrix(y[n,],nrow=1)
  n0<-nrow(y0)
  q<-ncol(y)
  B=100
  funs=lm_multi()


  sol<-conformal.multidim.msplit(x,y, x0, train.fun = funs$train.fun,
                                            predict.fun = funs$predict.fun, alpha=0.05,
                                            split=NULL, seed=FALSE, randomized=FALSE,
                                 seed.rand=FALSE,
                                            verbose=FALSE, rho=NULL,score = "max",
                                            s.type = "st-dev",B=B,lambda=0,
                                            tau = 0.1,mad.train.fun = NULL,
                                            mad.predict.fun = NULL)

sol
