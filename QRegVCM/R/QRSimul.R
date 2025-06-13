QRSimul <-
function(VecX, tau, times, subj, X, y, d, kn, degree, lambda, gam){
dim = length(subj)
X = matrix(X, nrow=dim)
H = length(tau)
px = ncol(X)
n = length(unique(subj))

if(px != length(VecX))
stop("the length of VecX and the number of covariate(s) must match")

XX = as.matrix(X)

# transform X to be in [0,1]
if(all(X[,1]==1)) VecX[1]=1 else VecX[1] = (VecX[1] - min(X[,1]))/(max(X[,1])-min(X[,1])) 
if(all(X[,1]==1)) X[,1]=X[,1] else X[,1] = (X[,1] - min(X[,1]))/(max(X[,1])-min(X[,1])) 

for(k in 2:px){
VecX[k] = (VecX[k] - min(X[,k]))/(max(X[,k])-min(X[,k]))
X[,k] = (X[,k] - min(X[,k]))/(max(X[,k])-min(X[,k]))
}

lambda_all = lambdak_simul(times, subj, X, y, d, tau,  kn, degree, lambda, gam)
lambdasicr = lambda_all$lambdasicr
lambdasic = lambda_all$lambdasic

simul = ncqr_simul(times, subj, y, X, tau, kn, degree, lambda=lambdasicr, lambcross=lambdasic, d)
W = simul$W
alpha = simul$alpha
hat_bt = simul$hat_bt


### Estimating qhat
Hpx = rep(seq(1,px), H)
Xbeta = matrix(NA, dim, H*px)

for(h in 1:(H*px))
    {
        Xbeta[,h]=hat_bt[,h]*VecX[Hpx[h]]
    }
qhat_h = matrix(NA, dim, H)
for(h in 1:H){
qhat_h[,h] = rowSums(Xbeta[,((h-1)*px+1):(px*h)])
}

# Transform back hat_bt

HpxB = rep(px,H)
cum_HpxB = cumsum(HpxB)
cum_HpxA = c(1, c(cum_HpxB[seq(1:(H-1))]+1))
  
hat_bt0_ori = hat_bt[,cum_HpxA]  ## originale
hat_btk_ori = hat_bt[,seq(2,px)] ## originale 
hat_btk = matrix(NA, dim, (px-1))
hat_btk0 = matrix(0, dim, (px-1))

for(k in 1:(px-1)){
hat_btk[,k] = hat_btk_ori[,k]/(max(XX[,(k+1)])-min(XX[,(k+1)]))
hat_btk0[,k] = hat_btk_ori[,k]*min(XX[,(k+1)])/(max(XX[,(k+1)])-min(XX[,(k+1)]))
}
hat_bt0 = hat_bt0_ori - rowSums(hat_btk0)

####

out = list(W=W, alpha=alpha, hat_bt0=hat_bt0, hat_btk=hat_btk, qhat_h = qhat_h)
return(out)

}
