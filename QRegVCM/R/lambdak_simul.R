lambdak_simul <-
function(times, subj, X, y, d, tau,  kn, degree, lambda, gam){

H = length(tau)
px = ncol(X)

lambdasic = Lamb_simul(times, subj, X, y, d, tau,  kn, degree, lambda)$lambdasic

ncqr2 = ncqr_simul(times, subj, y=y, X=X, tau, kn=kn, degree=degree, lambda=0, lambcross=1, d)$hat_bt
lambdasicr = NULL
for(h in 1:(H*px)){
        lambdasicr[h] = lambdasic*(diff(range(ncqr2[,h])))^((-(gam)))
    }

out = list(lambdasic = lambdasic, lambdasicr = lambdasicr)
return(out)
}
