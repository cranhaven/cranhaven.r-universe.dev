QRStepwise <-
function(VecX, tau, times, subj, X, y, d, kn, degree, lambda, gam){
dim = length(subj)
X = matrix(X, nrow=dim)
H = length(tau)
px = ncol(X)

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

## Median estimation
lambda50 = lambdak_indiv(times, subj, X=X, y=y, d=d, tau=0.5,  kn, degree, lambda, gam)
lambdasic50=lambda50$lambdasic
lambdasicr50=lambda50$lambdasicr
qvcsicr50=qrvcp_indiv(times, subj, y=y, X=X, tau=0.5, kn=kn, degree=degree, lambda=lambdasicr50, d=d)
alpha50 = qvcsicr50$alpha
hat_bt50 = qvcsicr50$hat_bt
W = qvcsicr50$W
mtot = length(alpha50)

## Complete Down
tauDown = tau[which(tau<0.5)]
Hdown = length(tauDown)
lambdasicrDown = matrix(NA, px, Hdown)
alphaDown = matrix(NA, mtot, Hdown)
hat_btDown = matrix(NA, (px*dim), Hdown)
for(h in (Hdown-1):1){
lambdasicrDown[,Hdown] = lambdak_CD(times, subj, X=X, y=y, d=d, tau=tauDown[Hdown],  kn, degree, lambda, alpha0=alpha50, gam)$lambdasicr
qvcsicr=qrvcp_CD(times, subj, y=y, X=X, tau=tauDown[Hdown], kn=kn, degree=degree, lambda=lambdasicrDown[,Hdown], d=d, alpha0=alpha50)
alphaDown[,Hdown] = qvcsicr$alpha
hat_btDown[,Hdown] = qvcsicr$hat_bt

lambdasicrDown[,h] = lambdak_CD(times, subj, X=X, y=y, d=d, tau=tauDown[h],  kn, degree, lambda, alpha0=alphaDown[,h+1], gam)$lambdasicr
qvcsicr=qrvcp_CD(times, subj, y=y, X=X, tau=tauDown[h], kn=kn, degree=degree, lambda=lambdasicrDown[,h], d=d, alpha0=alphaDown[,h+1])
alphaDown[,h] = qvcsicr$alpha
hat_btDown[,h] = qvcsicr$hat_bt
}

## Complete Up
tauUp = tau[which(tau>0.5)] 
Hup = length(tauUp)
lambdasicrUp = matrix(NA, px, Hup)
alphaUp = matrix(NA, mtot, Hup)
hat_btUp = matrix(NA, (px*dim), Hup)
for(h in 2:Hup){
lambdasicrUp[,1] = lambdak_CU(times, subj, X=X, y=y, d=d, tau=tauUp[1],  kn, degree, lambda, alpha0=alpha50, gam)$lambdasicr
qvcsicr=qrvcp_CU(times, subj, y=y, X=X, tau=tauUp[1], kn=kn, degree=degree, lambda=lambdasicrUp[,1], d=d, alpha0=alpha50)
alphaUp[,1] = qvcsicr$alpha
hat_btUp[,1] = qvcsicr$hat_bt

lambdasicrUp[,h] = lambdak_CU(times, subj, X=X, y=y, d=d, tau=tauUp[h],  kn, degree, lambda, alpha0=alphaUp[,h-1], gam)$lambdasicr
qvcsicr=qrvcp_CU(times, subj, y=y, X=X, tau=tauUp[h], kn=kn, degree=degree, lambda=lambdasicr50, d=d, alpha0=alphaUp[,h-1])
alphaUp[,h] = qvcsicr$alpha
hat_btUp[,h] = qvcsicr$hat_bt
}

lambdasicr = cbind(lambdasicrDown, lambdasicr50, lambdasicrUp)
alpha = cbind(alphaDown, alpha50, alphaUp)
hat_bt = cbind(hat_btDown, hat_bt50, hat_btUp)

## qhat
qhat = matrix(NA, dim, H)
for(h in 1:H){
qhat_k = matrix(NA, dim, px)
for(k in 1:px){
qhat_k[,k] = hat_bt[seq((k-1)*dim+1,k*dim),h]*VecX[k] 
}
qhat[,h] = rowSums(qhat_k)
}

# transform back hat_bt
for(h in 1:H){
hat_btk = matrix(0, dim, px)
hat_btk0 = matrix(0, dim, px)
for(k in 2:px){
hat_btk[,k] = hat_bt[seq((k-1)*dim+1,k*dim),h]/(max(XX[,k])-min(XX[,k]))
hat_btk0[,k] = hat_bt[seq((k-1)*dim+1,k*dim),h]*min(XX[,k])/(max(XX[,k])-min(XX[,k]))
}
hat_bt0 = hat_bt[seq(1,dim),h] - rowSums(hat_btk0)
hat_btk[,1]=hat_bt0
hat_bt[,h] = c(hat_btk)
}

out = list(alpha=alpha, hat_bt=hat_bt, W=W, lambdasicr=lambdasicr, qhat = qhat)
return(out)

}
