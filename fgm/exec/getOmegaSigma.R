library(mvtnorm)
library(fda)

set.seed(0)
nbasis=10
p= 10
time.grid.length = 15
prob = 0.1
decaying.factor = 3/(1:nbasis)^1.5 

## True precision matrices, one per basis
Omega<-lapply(1:nbasis, function(b){
  omega <- matrix(rbinom(p^2,1,prob)*runif(p^2,min=0.3, max=0.6),p,p)
  diag(omega) <- 0
  for (i in 1:p) {
    row.off.diag.sum = sum(abs(omega[i,]))
    if (row.off.diag.sum!=0) omega[i,]=omega[i,]/ (1.5*row.off.diag.sum)
  }
  diag(omega)<-1
  omega<-0.5*(omega + t(omega))
  omega/decaying.factor[b]
})

Sigma<-lapply(Omega, function(omega) solve(omega))
