QRIndiv <-
function(VecX, tau, times, subj, X, y, d, kn, degree, lambda, gam){
dim = length(subj)
X = matrix(X, nrow=dim)
H = length(tau)
px = ncol(X)

m = numeric(0)
for(k in 1:px){m = c(m, kn[k]+degree[k])}
mtot = sum(m)

if(px != length(VecX))
stop("the length of VecX and the number of covariate(s) must match")

qhat = matrix(NA, dim, H)

lambdasicr = matrix(NA, px, H)
alpha = matrix(NA, mtot, H)
hat_bt = matrix(NA, (dim*px), H)
qhat = matrix(NA, dim, H)

for(h in 1:H){
lambdasicr[,h] = lambdak_indiv(times, subj, X=X, y=y, d=d, tau=tau[h],  kn, degree, lambda, gam)$lambdasicr
btalpha=qrvcp_indiv(times, subj, y=y, X=X, tau=tau[h], kn=kn, degree=degree, lambda=lambdasicr[,h], d=d)
hat_bt[,h] = btalpha$hat_bt
qhat_k = matrix(NA, dim, px)
for(k in 1:px){
qhat_k[,k] = hat_bt[seq((k-1)*dim+1,k*dim),h]*VecX[k] 
}
qhat[,h] = rowSums(qhat_k)
}

out = list(alpha=alpha, hat_bt=hat_bt, qhat = qhat)
return(out)

}
