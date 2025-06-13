lambdak_indiv <-
function(times, subj, X, y, d, tau,  kn, degree, lambda, gam){
dim = length(subj)
X = matrix(X, nrow=dim)
px = ncol(X)
lambdasic = Lamb_indiv(times, subj, X, y, d, tau,  kn, degree, lambda)$lambdasic

qvc2_indiv=qrvcp_indiv(times, subj, y, X, tau, kn=kn, degree=degree, lambda=0, d)$hat_bt
lambdasicr = NULL
for(k in 1:px){
lambdasicr[k] = lambdasic*(max(qvc2_indiv[seq((k-1)*dim+1,k*dim)])-min(qvc2_indiv[seq((k-1)*dim+1,k*dim)]))^(-(gam))
}

out = list(lambdasic = lambdasic, lambdasicr = lambdasicr)
return(out)
}
