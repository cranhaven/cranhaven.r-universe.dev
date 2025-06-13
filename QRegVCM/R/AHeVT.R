AHeVT <-
function(VecX, times, subj, X, y, d, tau,  kn, degree, lambda, gam){


dim = length(subj)
X = matrix(X, nrow=dim)
px = ncol(X)
dim = nrow(X)

XX = as.matrix(X)
# transform X to be in [0,1]
if(all(X[,1]==1)) X[,1]=X[,1] else X[,1] = (X[,1] - min(X[,1]))/(max(X[,1])-min(X[,1])) 
for(k in 2:px){
X[,k] = (X[,k] - min(X[,k]))/(max(X[,k])-min(X[,k]))
}

## Step 1
lambdasicrMed = lambdak_indiv(times, subj, X=X, y=y, d=d, tau=0.5,  kn, degree, lambda, gam)$lambdasicr
hat_btMed=qrvcp_indiv(times, subj, y=y, X=X, tau=0.5, kn=kn, degree=degree, lambda=lambdasicrMed, d=d)$hat_bt

yhat50_k = matrix(NA, dim, px)
for(k in 1:px){
yhat50_k[,k] = hat_btMed[seq((k-1)*dim+1,k*dim)]*X[,k]
}
yhat50 = rowSums(yhat50_k)
res50 = y - yhat50
abs_res50 = abs(res50)
ln_res50 = log(abs_res50)

## Step 2
lambda_ln_res50 = lambdak_indiv(times, subj, X=rep(1,dim), y=ln_res50, d=d[1], tau=0.5,  kn=kn[1], degree=degree[1], lambda, gam)$lambdasicr
hat_ln_VT=qrvcp_indiv(times, subj, y=ln_res50, X=rep(1,dim), tau=0.5, kn=kn[1], degree=degree[1], lambda=lambda_ln_res50, d=d[1])$hat_bt
hat_Vt = exp(hat_ln_VT)

## Step 3
H = length(tau)
C = matrix(NA, dim, H)
for(h in 1:H){
lambdaS = Lamb_indiv(times, subj, X=hat_Vt, y=res50, d=d[1], tau=tau[h],  kn=kn[1], degree=degree[1], lambda)$lambdasic
C[,h] = qrvcp_indiv(times, subj, y=res50, X=hat_Vt, tau=tau[h], kn=kn[1], degree=degree[1], lambda=lambdaS, d=d[1])$hat_bt
}

# transform back hat_bt50
hat_bt50 = matrix(NA, dim, px)
hat_bt50_0k = matrix(0, dim, px)
for(k in 2:px){
hat_bt50[,k] = hat_btMed[seq((k-1)*dim+1,k*dim)]/(max(XX[,k])-min(XX[,k]))
hat_bt50_0k[,k] = hat_btMed[seq((k-1)*dim+1,k*dim)]*min(XX[,k])/(max(XX[,k])-min(XX[,k]))
}
hat_bt50[,1] = hat_btMed[seq(1,dim)] - rowSums(hat_bt50_0k)
hat_bt50 = c(hat_bt50)

### qhat
if(px != length(VecX))
stop("the length of VecX and the number of covariate(s) must match")

qhat = matrix(NA, dim, H)
for(h in 1:H){
qhat50_k = matrix(NA, dim, px)
for(k in 1:px){
qhat50_k[,k] = hat_bt50[seq((k-1)*dim+1,k*dim)]*VecX[k] 
}
qhat50 = rowSums(qhat50_k)

C[,which(tau==0.5)] = rep(0,dim)
qhat[,h] = qhat50 + C[,h]*hat_Vt
}

out = list(hat_bt50 = hat_bt50, yhat50 = yhat50, res50=res50, hat_Vt =hat_Vt, C = C, qhat=qhat)
return(out)
}
