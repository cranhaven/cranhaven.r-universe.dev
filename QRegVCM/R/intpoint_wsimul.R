intpoint_wsimul <-
function(times, subj, X, U, y, m, tau, lambda, lambcross, px, hat_Vt, Wtau, d){

dim = length(subj)
X = matrix(X, nrow=dim)
H = length(tau)
px = ncol(X)
n = length(unique(subj))
H = length(tau)
mtot = ncol(U)
dtot = sum(d)


###
W = Weight_Ni(y, subj)$W

wy = matrix(NA, dim, H)
for(h in 1:H){
wy[,h] = Wtau[h]*W*y
}
wyh = c(wy)
b = c(wyh, rep(0, ((H-1)*mtot)), rep(0, (H*(mtot-sum(d)))))

WU = W*U
## CalX 1 to H
M0 = matrix(rep(0,(dim*mtot)), ncol=mtot)

Hmtot = rep(mtot,H)
cum_HmtotB = cumsum(Hmtot)
cum_HmtotA = c(1, c(cum_HmtotB+1))
CX=list()

Hdim = rep(dim, H)
cum_HdimB = cumsum(Hdim)
cum_HdimA = c(1, c(cum_HdimB+1))
All_CX = matrix(NA, H*dim, H*mtot)

rhs_CX = matrix(NA, H*mtot, H)
for(h in 1:H){
CX[[h]] = matrix(0, dim, (mtot*H))
CX[[h]][,(cum_HmtotA[h]:cum_HmtotB[h])] = WU*Wtau[h]
All_CX[cum_HdimA[h]:cum_HdimB[h],]=CX[[h]]
rhs_CX[,h] = t(CX[[h]])%*%rep((1-tau[h]),dim) 
}


## Creating matrices Cal X (to prevent crossingness)
M1 = matrix(rep(0,(mtot*mtot)), ncol=mtot) # matrix -1 1 for TCX
D1 = cbind(-diag(mtot), diag(mtot))
CXNC=list()

HmtotNC = rep(mtot, (H-1))
cum_HmtotNCB = cumsum(HmtotNC)
cum_HmtotNCA = c(1, c(cum_HmtotNCB+1))
All_CXNC = matrix(NA, (H-1)*mtot, H*mtot)

rhs_CXNC = matrix(NA, H*mtot, H-1)
for(h in 1: (H-1)){
CXNC[[h]] = matrix(0, mtot, (mtot*H))
CXNC[[h]][,(cum_HmtotA[h]:cum_HmtotB[h+1])] = lambcross*D1
All_CXNC[cum_HmtotNCA[h]:cum_HmtotNCB[h],]=CXNC[[h]]
rhs_CXNC[,h] = t(CXNC[[h]])%*%rep(1/2, mtot)
}

## Creating matrices Cal X (for the penalty parts)
if((H*px) == length(lambda)) lambda_hpx=lambda else lambda_hpx = rep(lambda, (H*px)) 
#lambda_hpx = rep(lambda, (H*px))
Hmk = rep(m,H)
Hdk = rep(d,H)
cum_HmkB = cumsum(Hmk)
cum_HmkA = c(1, c(cum_HmkB+1))
CXPS = list()
DPS = list()

Hmkdk = Hmk - Hdk
cum_HmkdkB = cumsum(Hmkdk)
cum_HmkdkA = c(1, c(cum_HmkdkB+1))
All_CXPS = matrix(NA, H*(mtot-dtot), H*mtot)

rhs_CXPS = matrix(NA, H*mtot, H*px)
for(h in 1:(H*px)){
CXPS[[h]] = matrix(0, (Hmk[h]-Hdk[h]), (mtot*H))
DPS[[h]] = diff(diag(Hmk[h]), diff = Hdk[h])
CXPS[[h]][(1:(Hmk[h]-Hdk[h])),(cum_HmkA[h]:cum_HmkB[h])]=lambda_hpx[h]*DPS[[h]]
All_CXPS[cum_HmkdkA[h]:cum_HmkdkB[h],]=CXPS[[h]]
rhs_CXPS[,h] = t(CXPS[[h]])%*%rep(1/2, Hmk[h]-Hdk[h])
}

All_CX = as.matrix.csr(All_CX)
All_CXNC = as.matrix.csr(All_CXNC)
All_CXPS = as.matrix.csr(All_CXPS)

FP = rbind(All_CX, All_CXNC, All_CXPS)

## vector rhs
rhs = rowSums(rhs_CX) + rowSums(rhs_CXNC) + rowSums(rhs_CXPS)

#tmpmax = floor(1e5 + exp(-12.1)*(WU@ia[m*(p+1)]-1)^2.35)
fit = rq.fit.sfn(FP,b,rhs=rhs)#, control = list(tmpmax= tmpmax))
alpha = fit$coef
intout = list(alpha=alpha, W=W)
return(intout)
}
