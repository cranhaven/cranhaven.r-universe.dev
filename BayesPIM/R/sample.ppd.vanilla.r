sample.ppd.vanilla = function(par.list, Z.X, dist.X, s) {
  par    = as.matrix(par.list[1])
  p1.X   = ncol(Z.X) + 1
  beta.X = par[,1:p1.X, drop=F]
  sigma  = par[,p1.X+1]
  for(i in 2:length(par.list)){
    par = as.matrix(par.list[i])
    beta.X = rbind(beta.X,par[,1:p1.X, drop=F])
    sigma = c(sigma, par[,p1.X+1])
  }
  linterm = cbind(1,Z.X) %*% t(beta.X[s,])
  if(dist.X != 'lognormal'){
    a = sigma[s]^-1
    b = exp(linterm)
  }
  if(dist.X == 'lognormal'){
    a = sigma[s] 
    b = linterm 
  }
  x = apply(rbind(a,b),2, function(x) rdist(n = length(x)-1, par = cbind(x[2:length(x)],x[1]), dist = dist.X ) )
  as.matrix(x)
}