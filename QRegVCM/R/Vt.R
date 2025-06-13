Vt <-
function(times, subj, X, y, d, tau,  kn, degree, lambda, gam){
dim = length(subj)
X = matrix(X, nrow=dim)
H = length(tau)
px = ncol(X)
n = length(unique(subj))

lambda_all = lambdak_simul(times, subj, X, y, d, tau,  kn, degree, lambda, gam)
lambdasicr = lambda_all$lambdasicr
lambdasic = lambda_all$lambdasic

simul = ncqr_simul(times, subj, y, X, tau, kn, degree, lambda=lambdasicr, lambcross=lambdasic, d)
hat_bt = simul$hat_bt

Hpx = rep(seq(1,px), H)
Xbeta = matrix(NA, dim, H*px)

for(h in 1:(H*px))
    {
        Xbeta[,h]=hat_bt[,h]*X[,Hpx[h]]
    }
qhat_h = matrix(NA, dim, H)
for(h in 1:H){
qhat_h[,h] = rowSums(Xbeta[,((h-1)*px+1):(px*h)])
}

qhat_50 = qhat_h[,which(tau==0.5)]
r50 = y - qhat_50 

lambdasic_Vt=Lamb_indiv(times, subj, X=rep(1,dim), y=log(abs(r50)), d=d[1], tau=0.5,  kn=kn[1], degree=degree[1], lambda)$lambdasic
log_vt=qrvcp_indiv(times, subj, y=log(abs(r50)), X=rep(1,dim), tau=0.5, kn=kn[1], degree=degree[1], lambda=lambdasic_Vt, d=d[1])$hat_bt

hat_Vt = exp(log_vt)

out = list(hat_Vt = hat_Vt, r50=r50)

return(out)

}
