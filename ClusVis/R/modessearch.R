# fkcomputeModes <- function(x, m)
#   prod(dnorm(x, m))
# 
# objectiveModes <- function(x, pi, mu)
#   sum(sapply(1:length(pi), function(k) fkcomputeModes(x, mu[k,]) * pi[k]))
# 
# gradModes <- function(x, pi, mu){
#   fk <- sapply(1:length(pi), function(k) fkcomputeModes(x, mu[k,]))
#   sapply(1:length(x), function(j) sum(fk * pi * mu[,j]) - x[j] * sum(fk * pi))
# }
# 
# 
# onenewtonModes <- function(pi, mu){
#   res <- optim(runif(ncol(mu), apply(mu, 2, min), apply(mu, 2, max)), objectiveModes, pi=pi, mu=mu, gr = gradModes, method = "BFGS", control = list(maxit=2000, fnscale=-1))
#   c(res$convergence, res$par)
# }
# 
# 
# modesSearch <- function(pi, mu, nbinit=200){
#   modes <- t(round(replicate(nbinit, onenewtonModes(pi, mu)), 1))
#   modes <- modes[which(modes[,1]==0),-1]
#   uniquecombs(modes)
# }
