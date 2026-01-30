sample.ppd.xstar = function(par.list, Z.X, Z.W, dist.X, s, g.fixed, type) {
  n = nrow(Z.X)
  x = sample.ppd.vanilla(par.list, Z.X, dist.X, s)
  
  par    = as.matrix(par.list[1])
  p1.X   = ncol(Z.X) + 1
  p1.W   = ncol(Z.W) + 1
  beta.W = par[,(p1.X+2):(p1.X+1+p1.W), drop=F]
  for(i in 2:length(par.list)){
    par    = as.matrix(par.list[i])
    beta.W = rbind(beta.W,par[,(p1.X+2):(p1.X+1+p1.W), drop=F])
  }
  linterm = cbind(1,Z.W) %*% t(beta.W[s,])
  theta   = pnorm(linterm)
  g = matrix(NA, ncol = length(s), nrow =n )
  for(i in 1:n){
    #if(g.fixed[i] == 1) g[i,] = 1
    #if(g.fixed[i] == 0) g[i,] = rbinom( length(s), 1, theta[i,])
    g[i,] = rbinom( length(s), 1, theta[i,])
  }
  if(type == 'xstar') x = x * (1-g)
  if(type == 'x') x[g==1] = NA
  as.matrix(x)
}