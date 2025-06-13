Wtau <-
function(times, subj, X, y, d, tau,  kn, degree, lambda, gam, hat_Vt, r50){
dim = length(subj)
X = matrix(X, nrow=dim)
H = length(tau)
px = ncol(X)
n = length(unique(subj))

rvt50 = r50/hat_Vt

lambdasicr_wtau = matrix(NA, px, H)
hat_btr = matrix(NA, (dim*px), H)
qr_h = matrix(NA, dim, H)
for(h in 1:H){ 

lambdasicr_wtau[,h] = lambdak_indiv(times, subj, X=X, y=y, d=d, tau=tau[h],  kn, degree, lambda, gam)$lambdasicr
hat_btr[,h]=qrvcp_indiv(times, subj, y=y, X=X, tau=tau[h], kn=kn, degree=degree, lambda=lambdasicr_wtau[,h], d=d)$hat_bt

qhat_k = matrix(NA, dim, px)
for(k in 1:px){
qhat_k[,k] = hat_btr[seq((k-1)*dim+1,k*dim),h]*X[k] 
}
qr_h[,h] = rowSums(qhat_k)
}

res = matrix(NA, dim, H)
rho_h = matrix(NA, dim, H)
Wtau = NULL
for(h in 1:H){
res[,h] = rvt50 - qr_h[,h]
rho_h[,h] = (tau[h]*res[,h])*(res[,h]>0)+(-(1-tau[h])*res[,h])*(res[,h]<=0)
Wtau[h] = 1/mean(rho_h[,h])
}

out = list(Wtau=Wtau)
return(out)

}
