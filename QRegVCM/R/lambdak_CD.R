lambdak_CD <-
function(times, subj, X, y, d, tau,  kn, degree, lambda, alpha0, gam){
dim = length(y)
X = matrix(X, nrow=dim)
px = ncol(X)
lambdasic = Lamb_CD(times, subj, X, y, d, tau,  kn, degree, lambda, alpha0)$lambdasic

qvc2=qrvcp_CD(times, subj, y, X, tau=tau, kn=kn, degree=degree, lambda=rep(0,px), d, alpha0)$hat_bt #only B-Splines

lambdasicr = NULL
for(k in 1:px){
lambdasicr[k] = lambdasic*(max(qvc2[seq((k-1)*dim+1,k*dim)])-min(qvc2[seq((k-1)*dim+1,k*dim)]))^(-(gam))
}

out = list(lambdasic = lambdasic, lambdasicr=lambdasicr)
return(out)
}
