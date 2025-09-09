n=20
p=2
q=2
mu=rep(0,p)
x = mvtnorm::rmvnorm(n, mu)
beta<-sapply(1:q, function(k) c(mvtnorm::rmvnorm(1,mu)))
y = x%*%beta + t(mvtnorm::rmvnorm(q,1:n))
x0=matrix(x[n,],nrow=1)
y0=matrix(y[n,],nrow=1)
n0<-nrow(y0)
funs=mean_multi()

sol<-conformal.multidim.jackplus(x,y,x,train.fun = funs$train.fun,
                                      predict.fun = funs$predict.fun)





