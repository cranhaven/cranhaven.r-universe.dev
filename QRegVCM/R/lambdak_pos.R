lambdak_pos <-
function(times, subj, X, y, d, tau,  kn, degree, lambda, gam){
dim = length(y)
X = matrix(X, nrow=dim)
px = ncol(X)
lambdasic = Lamb_pos(times, subj, X, y, d, tau,  kn, degree, lambda)$lambdasic

qvc2=qrvcp_pos(times, subj, y, X, tau=tau, kn=kn, degree=degree, lambda=rep(0,px), d)$hat_bt #only B-Splines

lambdasicr = NULL
for(k in 1:px){
lambdasicr[k] = lambdasic*(max(qvc2[seq((k-1)*dim+1,k*dim)])-min(qvc2[seq((k-1)*dim+1,k*dim)]))^(-(gam))
}

out = list(lambdasic = lambdasic, lambdasicr=lambdasicr)
return(out)
}
