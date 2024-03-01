loglik_frailty_corr<- function(X1, X2, Y1, Y2, theta, cuts=NULL, nbase, data, design, base.dist, frailty.dist, agemin, vec=FALSE)
{

if(!design %in% c("pop", "pop+"))  stop("Frailty model is only available for POP or POP+ design.")
  
nb <- sum(nbase) # number of baselines parameters
  
if(base.dist[1]=="lognormal") bparms1 <- c(theta[1], exp(theta[2]))
else bparms1 <- exp(theta[1:nbase[1]])
if(base.dist[2]=="lognormal") bparms2 <- c(theta[(nbase[1]+1)], exp(theta[(nbase[1]+2)]))
else bparms2 <- exp(theta[(nbase[1]+1):nb])

nx1 <- dim(X1)[2]
nx2 <- dim(X2)[2]
xbeta1 <- c(X1%*%theta[(nb+1):(nb+nx1)])
xbeta2 <- c(X2%*%theta[(nb+nx1+1):(nb+nx1+nx2)])
kappa <- exp(theta[(nb+nx1+nx2+1):length(theta)])

time01 <- Y1[,1] - agemin
time02 <- Y2[,1] - agemin
cuts0 <- cuts - agemin
status1 <- Y1[,2]
status2 <- Y2[,2]

ip <- data$proband == 1
wt <- data$weight
wt.p <- wt[ip]

bhaz1 <- hazards(base.dist[1], time01, bparms1, cuts=cuts0)
bhaz2 <- hazards(base.dist[2], time02, bparms2, cuts=cuts0)
bcumhaz1 <- cumhaz(base.dist[1], time01, bparms1, cuts=cuts0)
bcumhaz2 <- cumhaz(base.dist[2], time02, bparms2, cuts=cuts0)

H1 <- bcumhaz1*exp(xbeta1)
H2 <- bcumhaz2*exp(xbeta2)
logh1 <- log(bhaz1) + xbeta1
logh2 <- log(bhaz2) + xbeta2
loglik <-  wt * (status1*logh1 + status2*logh2)

df1 <- data$df1[ip]
df2 <- data$df2[ip]
Hfam1 <- aggregate(H1, list(data$famID), sum)[,2]
Hfam2 <- aggregate(H2, list(data$famID), sum)[,2]

# Ascertainment correction (design = pop, pop+)
cagep <- data$currentage[ip]-agemin
xbeta1.p <- xbeta1[ip]
xbeta2.p <- xbeta2[ip]
bcumhaz1.p <- cumhaz(base.dist[1], cagep, bparms1, cuts=cuts0)
bcumhaz2.p <- cumhaz(base.dist[2], cagep, bparms2, cuts=cuts0)
H1.p <- bcumhaz1.p*exp(xbeta1.p)
H2.p <- bcumhaz2.p*exp(xbeta2.p)


if(frailty.dist%in%c("gamma","lognormal") ){
dla1 <- dlaplace(frailty.dist, g=Hfam1, d=df1, k=kappa[1])
dla2 <- dlaplace(frailty.dist, g=Hfam2, d=df2, k=kappa[2])
logdL <- wt.p*(log(dla1) + log(dla2))

la1.p <-laplace(frailty.dist, H1.p, kappa[1])
la2.p <-laplace(frailty.dist, H2.p, kappa[2])
logasc <- wt.p*log(1-la1.p*la2.p)

}
else if(frailty.dist=="clognormal"){ # numerical integral bivariate normal
gfun <- function(x, d1,d2, H1, H2) exp(x[1]*d1+x[2]*d2-exp(x[1])*H1-exp(x[2])*H2)
sigma <- matrix(c(kappa[1], sqrt(kappa[1]*kappa[2])*kappa[3], sqrt(kappa[1]*kappa[2])*kappa[3], kappa[2]), nrow=2)
pts <- mgauss.hermite(6, mu=c(0,0), sigma=sigma, prune=0) #6(n=32); 5(n=21)
logdL <- wt.p*log(apply(pts$points, 1, gfun, d1=df1, d2=df2, H1=Hfam1, H2=Hfam2) %*% pts$weights)

sfun <- function(x, H1, H2) exp(-exp(x[1])*H1-exp(x[2])*H2)
int.gh <- c(t(apply(pts$points, 1, sfun, H1=H1.p, H2=H2.p) %*% pts$weights))
logasc <- wt.p*log(1-int.gh)
}
else if(frailty.dist=="cgamma"){
# correlated gamma
fun <- function(x1, x2, k0, k1, k2, H1,H2,d1, d2){
  gamma(d1+1)*gamma(d2+1)/(gamma(k0)*gamma(k1)*gamma(k2))*(k0+k1)^(-d1)*(k0+k2)^(-d2)*
    gamma(k0+x1+x2)*(1+H1/(k0+k1)+H2/(k0+k2))^(-k0-x1-x2)*
    gamma(k1+d1-x1)*(1+H1/(k0+k1))^(-k1-d1+x1)/gamma(x1+1)/gamma(d1-x1+1)*
    gamma(k2+d2-x2)*(1+H2/(k0+k2))^(-k2-d2+x2)/gamma(x2+1)/gamma(d2-x2+1)
}
fun1 <- function(vec, x2, k0, k1, k2) sapply(0:vec[1], fun, x2=x2, k0=k0, k1=k1, k2=k2, H1=vec[3], H2=vec[4], d1=vec[1], d2=vec[2])
fun3 <- function(vec, k0, k1, k2) sum(sapply(0:vec[2], fun1, k0=k0, k1=k1, k2=k2, vec=vec))

k1 <- kappa[1]; k2 <- kappa[2]; k0 <- kappa[3]
w1 <- k0+k1; w2 <- k0+k2
logdL <- wt.p*log(apply(cbind(df1, df2, Hfam1, Hfam2), 1, fun3, k0=k0, k1=k1, k2=k2))

logasc <- wt.p*log(1-(1+H1.p/w1+H2.p/w2)^(-k0)*(1+H1.p/w1)^(-k1)*(1+H2.p/w2)^(-k2))
}


slogasc <- sum(logasc[logasc!=-Inf], na.rm=T) 
sloglik <- sum(loglik[loglik!=-Inf], na.rm=T) + sum(logdL, na.rm=T) - slogasc
loglik[ip] <- loglik[ip] + logdL - logasc



#print(c(theta, -sloglik))

if(vec) return(-loglik)
else  return(-sloglik)
}
