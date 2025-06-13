intpoint_pos <-
function(subj, U, y, kn, degree, d, lambda, tau, px){
#require(quantreg) 
#require(SparseM)

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

mtot = sum(m)
dtot = sum(d)

b = c(W*y, rep(0, mtot), vb0)

cum_mB = cumsum(m)
cum_mA = c(1, c(cum_mB+1))
D = list()
D0 = list()
for(k in 1:px){
D[[k]] = matrix(0, (m[k]-d[k]), mtot)
D0[[k]] = diff(diag(m[k]), diff = d[k]) 
D[[k]][(1:(m[k]-d[k])),(cum_mA[k]:cum_mB[k])]=lambda[k]*D0[[k]]
}

DD = matrix(NA, (mtot-dtot), mtot)
cum_mdB = cumsum(md)
cum_mdA = c(1, c(cum_mdB+1))
for(k in 1:px){ 
DD[cum_mdA[k]:cum_mdB[k],]=D[[k]]
}

DD = as.matrix.csr(DD)
DA = as.matrix.csr(-diag(mtot))
WU = as.matrix.csr(W*U)
Fidelity = WU
Penalty = DD
FP = rbind(Fidelity, DA, Penalty)

### vector rhs
rhs_pen = matrix(NA, mtot, px)
for(k in 1:px){
rhs_pen[,k] = t(D[[k]])%*%rep(1/2,(m[k]-d[k]))
}
rhs = (1-tau)*(t(WU)%*%rep(1,dim))+ t(DA)%*%rep(1,mtot) + rowSums(rhs_pen)

tmpmax = floor(1e5 + exp(-12.1)*(WU@ia[mtot]-1)^2.35)
fit = rq.fit.sfn(FP,b,rhs=rhs, control=list(tmpmax=tmpmax))
alpha = fit$coef
intout = list(alpha=alpha, D=D)
return(intout)
}
