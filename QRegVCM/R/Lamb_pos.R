Lamb_pos <-
function(times, subj, X, y, d, tau,  kn, degree, lambda){

dim = length(y)
X = matrix(X, nrow=dim)
px = ncol(X)
n = length(unique(subj))
nlam = length(lambda)


W = Weight_Ni(y, subj)$W

yhatsic = matrix(NA, dim, nlam)
ressic = matrix(NA, dim, nlam)
SIC = NULL
plam = NULL

for (i in 1:nlam)
{
qvcsic = qrvcp_pos(times, subj, y, X, tau=tau, kn=kn, degree=degree, lambda=lambda[i], d)
hat_btsic = qvcsic$hat_bt

yhatsic_k = matrix(NA, dim, px)
for(k in 1:px){
yhatsic_k[,k] = hat_btsic[seq((k-1)*dim+1,k*dim)]*X[,k]
}
yhatsic[,i] = rowSums(yhatsic_k)
ressic[,i] = y - yhatsic[,i]

### SIC ###
plam[i] = length(ressic[which(abs(ressic[,i])<(10^(-2)))])
SIC[i] = log(sum(W*ressic[,i]*(tau-1*(ressic[,i]<0)))/n)+log(dim)*plam[i]/(dim*2)
}
lambdasic = lambda[which(SIC == min(SIC))]

Lout = list(lambdasic = lambdasic)
return(Lout)
}
