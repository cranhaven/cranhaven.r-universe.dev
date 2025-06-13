ncqr_simul <-
function(times, subj, y, X, tau, kn, degree, lambda, lambcross, d){

dim = length(subj)
X = matrix(X, nrow=dim)
px = ncol(X)
dim = nrow(X)
H = length(tau)

if(px != length(kn) || px != length(degree) || px != length(d))
stop("the number of covariate(s) and the length of kn, degree, and d must match")

if(dim != length(y) || dim != length(subj))
stop("dimension of X, y, subj must match")

##########
m = numeric(0)
B = list()
for(k in 1:px){
m = c(m, kn[k]+degree[k])
B[[k]] = bbase(times,min(times),max(times),kn[k],degree[k])
}
cum_mB = cumsum(m)
cum_mA = c(1, c(cum_mB+1))

# Matrix U
U = NULL
for(k in 1:px)
{
U = cbind(U, X[,k]*B[[k]])
}

W_alpha = intpoint_simul(times, subj, U, y, m, tau, lambda, lambcross, px, d)
alpha = W_alpha$alpha
W = W_alpha$W

# Beta(t)
Hmk = rep(m,H)
Hdk = rep(d,H)
cum_HmkB = cumsum(Hmk)
cum_HmkA = c(1, c(cum_HmkB+1))

Hpx = rep(seq(1,px), H)
coef.X = matrix(NA, dim, H*px)   
for(h in 1:(H*px)){
        coef.X[,h]=B[[Hpx[h]]] %*% alpha[cum_HmkA[h]:cum_HmkB[h]]
    }

hat_bt = coef.X
out = list(hat_bt = hat_bt, alpha=alpha, W=W)
return(out)
}
