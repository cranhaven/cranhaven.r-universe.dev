Lamb_simul <-
function(times, subj, X, y, d, tau,  kn, degree, lambda){

dim = length(y)
X = matrix(X, nrow=dim)
H = length(tau)
px = ncol(X)
n = length(unique(subj))

nlam = length(lambda)

SIC = NULL

for (i in 1:nlam){
ncqrsic = ncqr_simul(times, subj, y, X, tau, kn=kn, degree=degree, lambda=lambda[i], lambcross=lambda[i], d)
hat_btsic = ncqrsic$hat_bt
W = ncqrsic$W

Hpx = rep(seq(1,px), H)

Xbeta = matrix(NA, dim, H*px)
for(h in 1:(H*px))
    {
        Xbeta[,h]=hat_btsic[,Hpx[h]]*X[,Hpx[h]]
    }
res = matrix(NA, dim, H)
SICfid = matrix(NA, dim, H)
for(h in 1:H){
res[,h]=y - rowSums(Xbeta[,((h-1)*px+1):(px*h)])
SICfid[,h] = res[,h]*(tau[h]-1*(res[,h]<0))
}

ressic = c(res)
### SIC ###
plam = length(ressic[which(abs(ressic)<(10^(-2)))])

SIC[i] = log(sum(W*SICfid)/(n*H)) + log(dim)*plam/(dim*2)
}
lambdasic = lambda[which(SIC == min(SIC))]

Lout = list(SIC=SIC, lambdasic = lambdasic)
return(Lout)
}
