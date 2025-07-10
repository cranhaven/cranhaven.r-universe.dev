utility_func = function(fit.prior, Uscore, C){
  #delta
  delta = -as.numeric(fit.prior$alpha.mat[2, 1:(C-1)])
  delta.sd = as.vector(sqrt(diag(as.matrix(fit.prior$vcov))[C:dim(fit.prior$vcov)[1]]))
  #gamma
  gamma = as.numeric(fit.prior$alpha.mat[1, 1:(C-1)])
  gamma.sd = as.vector(sqrt(diag(as.matrix(fit.prior$vcov))[1:(C-1)]))

  d = rbind(cbind(delta, delta.sd),cbind(gamma, gamma.sd))

  samples = apply(d, 1, function(x) rnorm(100, mean = x[1], sd = abs(x[2])))
  u = matrix(NA, nrow = 2, ncol = dim(samples)[1])
  #use posterior samples to calculate proportions
  for (i in 1:dim(samples)[1]){
    Q = matrix(NA, nrow = 2, ncol = C-1)
    prob = matrix(NA, nrow = 2, ncol = C)
    sample = samples[i,]
    for (c in 1:(C-1)){
      Q[1,c] = plogis(sample[c+5])
      Q[2,c] = plogis(sample[c+5] - sample[c])
    }
    for (j in 1:2){
      prob[j,1] <- Q[j,1]
      for (c in 2:(C-1)){
        prob[j,c] <- Q[j,c] - Q[j,(c-1)]
      }
      prob[j,C] <- 1- Q[j, (C-1)]
    }
    u[,i] = prob %*% Uscore
  }
  return(u)
}
