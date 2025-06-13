intpoint_CD <-
function(subj, U, y, kn, degree, d, lambda, tau, px, alpha0){


dim = length(y)
if(px == length(lambda)) lambda=lambda else lambda = rep(lambda, px)
#lambda = rep(lambda, px)

W = Weight_Ni(y, subj)$W

##########
md = numeric(0)
m = numeric(0)
vb0 = numeric(0)

for(k in 1:px){
m = c(m, kn[k]+degree[k])
md=c(md,m[k]-d[k])
vb0 = c(vb0, rep(0, ((m[k]-d[k]))))  ### null vector for b 
}

b = c(W*y, alpha0, vb0)

mtot = sum(m)
dtot = sum(d)

cum_mB = cumsum(m)
cum_mA = c(1, c(cum_mB+1))
D = list()
D0 = list()
DD = matrix(NA, (mtot-dtot), mtot)
cum_mdB = cumsum(md)
cum_mdA = c(1, c(cum_mdB+1))
rhs_pen = matrix(NA, mtot, px)
for(k in 1:px){
D[[k]] = matrix(0, (m[k]-d[k]), mtot)
D0[[k]] = diff(diag(m[k]), diff = d[k]) 
D[[k]][(1:(m[k]-d[k])),(cum_mA[k]:cum_mB[k])]=lambda[k]*D0[[k]]
DD[cum_mdA[k]:cum_mdB[k],]=D[[k]]
rhs_pen[,k] = t(D[[k]])%*%rep(1/2,(m[k]-d[k]))
}

DA = diag(mtot)
TX = as.matrix.csr(DA)
DD = as.matrix.csr(DD)
WU = as.matrix.csr(W*U)
Fidelity = WU
Penalty = DD
FP = rbind(Fidelity, TX, Penalty)

### vector rhs
rhs = (1-tau)*(t(WU)%*%rep(1,dim))+ t(TX)%*%rep(1,mtot) + rowSums(rhs_pen)

tmpmax = floor(1e5 + exp(-12.1)*(WU@ia[mtot]-1)^2.35)
fit = rq.fit.sfn(FP,b,rhs=rhs, control=list(tmpmax=tmpmax))
alpha = fit$coef
intout = list(alpha=alpha, D=D)
return(intout)
}
