loglik_ind<- function(X, Y, theta, cuts=NULL, nbase, data, design, base.dist, agemin, vec=FALSE)
{

if(base.dist=="lognormal") bparms <- c(theta[1], exp(theta[2]))
else bparms <- exp(theta[1:nbase])

xbeta <- c(X%*%theta[-c(1:nbase)])

time0 <- Y[,1] - agemin
cuts0 <- cuts - agemin
status <- Y[,2]
wt <- data$weight

bhaz <- hazards(base.dist, time0, bparms, cuts=cuts0)
bcumhaz <- cumhaz(base.dist, time0, bparms, cuts=cuts0)

H <- bcumhaz*exp(xbeta)
logh <- log(bhaz) + xbeta

loglik <-  wt * (- H + status*logh )
loglik.vec <- loglik
# Ascertainment correction by design

if(design=="cli" | design=="cli+"){
  i.m <- data$relation == 4 & data$gender == 0
  i.f <- data$relation == 4 & data$gender == 1
  i.s <- data$relation == 2 & data$status == 1
  
  ip <-  data$relation == 1 & status == 1
}
else  ip <- data$proband == 1

cagep <- data$currentage[ip]-agemin
xbeta.p <- xbeta[ip]
bcumhaz.p <- cumhaz(base.dist, cagep, bparms, cuts=cuts0)
wt.p <- data$weight[ip]

loglik.vec[ip] <- loglik.vec[ip] - wt.p*log(1-exp(-bcumhaz.p*exp(xbeta.p)))

logasc <- wt.p*log(1-exp(-bcumhaz.p*exp(xbeta.p))) 
slogasc <- sum(logasc[logasc!=-Inf], na.rm=T) 

if(design=="cli" | design=="cli+"){

  cage.m <- data$currentage[i.m]-agemin
  xbeta.m <- xbeta[i.m]
  bcumhaz.m <- cumhaz(base.dist, cage.m, bparms, cuts=cuts0)

  cage.f <- data$currentage[i.f]-agemin
  xbeta.f <- xbeta[i.f]
  bcumhaz.f <- cumhaz(base.dist, cage.f, bparms, cuts=cuts0)
  
  cage.s <- data$currentage[i.s]-agemin
  xbeta.s <- xbeta[i.s]
  bcumhaz.s <- cumhaz(base.dist, cage.s, bparms, cuts=cuts0)
  
  wt.m <- data$weight[i.m]
  wt.f <- data$weight[i.f]
  wt.s <- data$weight[i.s]
  wt.p <- data$weight[ip]

loglik.vec[ip] <- loglik.vec[ip] - wt.p*log(1-exp(-bcumhaz.m*exp(xbeta.m) -bcumhaz.f*exp(xbeta.f)))
loglik.vec[i.s] <- loglik.vec[i.s] - wt.s*log(1-exp(-bcumhaz.s*exp(xbeta.s) ))

slogasc <- slogasc + sum(wt.p*log(1-exp(-bcumhaz.m*exp(xbeta.m) -bcumhaz.f*exp(xbeta.f))), na.rm=T) 
+ sum(wt.s*log(1-exp(-bcumhaz.s*exp(xbeta.s))), na.rm=T)
}

likelihood  <- try(sum(loglik[loglik!=-Inf], na.rm=T) - slogasc)
if(vec){
 return(-aggregate(loglik.vec, by=list(data$famID.byuser), sum)[,2])
}else  return(-likelihood)
}
