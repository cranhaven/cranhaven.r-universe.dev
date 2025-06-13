lambdak_wsimul <-
function(times, subj, X, y, d, tau,  kn, degree, lambda, gam, hat_Vt, Wtau){

H = length(tau)
px = ncol(X)

lambdasic = Lamb_wsimul(times, subj, X, y, d, tau,  kn, degree, lambda, hat_Vt, Wtau)$lambdasic

ncqr2 = ncqr_wsimul(times, subj, y, X, tau, kn, degree, lambda=0, lambcross=1, hat_Vt, Wtau, d)$hat_bt

lambdasicr = NULL
for(h in 1:(H*px)){
        lambdasicr[h] = lambdasic*(diff(range(ncqr2[,h])))^((-(gam)))
    }

out = list(lambdasic = lambdasic, lambdasicr = lambdasicr)
return(out)
}
