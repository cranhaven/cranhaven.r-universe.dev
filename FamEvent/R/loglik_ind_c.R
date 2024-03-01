loglik_ind_c<- function(X1, X2, Y1, Y2, theta, cuts=NULL, nbase, data, design, base.dist, agemin, vec=FALSE)
{

nb <- sum(nbase) # number of baselines parameters
  
if(base.dist[1]=="lognormal") bparms1 <- c(theta[1], exp(theta[2]))
else bparms1 <- exp(theta[1:nbase[1]])
if(base.dist[2]=="lognormal") bparms2 <- c(theta[(nbase[1]+1)], exp(theta[(nbase[1]+2)]))
else bparms2 <- exp(theta[(nbase[1]+1):nb])

nx1 <- dim(X1)[2]
nx2 <- dim(X2)[2]
xbeta1 <- c(X1%*%theta[(nb+1):(nb+nx1)])
xbeta2 <- c(X2%*%theta[(nb+nx1+1):(nb+nx1+nx2)])


time01 <- Y1[,1] - agemin
time02 <- Y2[,1] - agemin
cuts0 <- cuts - agemin
status1 <- Y1[,2]
status2 <- Y2[,2]
wt <- data$weight

bhaz1 <- hazards(base.dist[1], time01, bparms1, cuts=cuts0)
bhaz2 <- hazards(base.dist[2], time02, bparms2, cuts=cuts0)
bcumhaz1 <- cumhaz(base.dist[1], time01, bparms1, cuts=cuts0)
bcumhaz2 <- cumhaz(base.dist[2], time02, bparms2, cuts=cuts0)

H1 <- bcumhaz1*exp(xbeta1)
H2 <- bcumhaz2*exp(xbeta2)
logh1 <- log(bhaz1) + xbeta1
logh2 <- log(bhaz2) + xbeta2

loglik <-  wt * (status1*logh1 + status2*logh2 - H1 - H2)
loglik.vec <- loglik

# Ascertainment correction by design

if(design=="cli" | design=="cli+"){
  i.m <- data$relation == 4 & data$gender == 0
  i.f <- data$relation == 4 & data$gender == 1
  i.s <- data$relation == 2 & data$status != 0 
  ip <- data$relation == 1 & data$status != 0
}
else  ip <- data$proband == 1

cagep <- data$currentage[ip]-agemin
xbeta1.p <- xbeta1[ip]
xbeta2.p <- xbeta2[ip]
bcumhaz1.p <- cumhaz(base.dist[1], cagep, bparms1, cuts=cuts0)
bcumhaz2.p <- cumhaz(base.dist[2], cagep, bparms2, cuts=cuts0)
wt.p <- data$weight[ip]

loglik.vec[ip] <- loglik.vec[ip] - wt.p*log(1-exp(-bcumhaz1.p*exp(xbeta1.p)-bcumhaz2.p*exp(xbeta2.p)))

logasc <- wt.p*log(1-exp(-bcumhaz1.p*exp(xbeta1.p) - bcumhaz2.p*exp(xbeta2.p))) 

slogasc <- sum(logasc[logasc!=-Inf], na.rm=T) 


if(design=="cli" | design=="cli+"){

  cage.m <- data$currentage[i.m]-agemin
  xbeta1.m <- xbeta1[i.m]
  xbeta2.m <- xbeta2[i.m]
  bcumhaz1.m <- cumhaz(base.dist[1], cage.m, bparms1, cuts=cuts0)
  bcumhaz2.m <- cumhaz(base.dist[2], cage.m, bparms2, cuts=cuts0)
  
  cage.f <- data$currentage[i.f]-agemin
  xbeta1.f <- xbeta1[i.f]
  xbeta2.f <- xbeta2[i.f]
  bcumhaz1.f <- cumhaz(base.dist[1], cage.f, bparms1, cuts=cuts0)
  bcumhaz2.f <- cumhaz(base.dist[2], cage.f, bparms2, cuts=cuts0)
  
  cage.s <- data$currentage[i.s]-agemin
  xbeta1.s <- xbeta1[i.s]
  xbeta2.s <- xbeta2[i.s]
  bcumhaz1.s <- cumhaz(base.dist[1], cage.s, bparms1, cuts=cuts0)
  bcumhaz2.s <- cumhaz(base.dist[2], cage.s, bparms2, cuts=cuts0)
  
  wt.s <- data$weight[i.s]
  wt.p <- data$weight[ip]

loglik.vec[ip] <- loglik.vec[ip] - wt.p*log(1-exp(-bcumhaz1.m*exp(xbeta1.m)-bcumhaz2.m*exp(xbeta2.m) - bcumhaz1.f*exp(xbeta1.f) - bcumhaz2.f*exp(xbeta2.f)))
loglik.vec[i.s] <- loglik.vec[i.s] - wt.s*log(1-exp(-bcumhaz1.s*exp(xbeta1.s)-bcumhaz2.s*exp(xbeta2.s) ))

slogasc <- slogasc + sum(wt.p*log(1-exp(-bcumhaz1.m*exp(xbeta1.m)-bcumhaz2.m*exp(xbeta2.m)-bcumhaz1.f*exp(xbeta1.f)-bcumhaz2.f*exp(xbeta2.f))), na.rm=T) 
+ sum(wt.s*log(1-exp(-bcumhaz2.s*exp(xbeta2.s)-bcumhaz2.s*exp(xbeta2.s))), na.rm=T)
}
likelihood  <- sum(loglik[loglik!=-Inf], na.rm=T) - slogasc

#print(c(theta, -likelihood))

if(vec){
 return(-loglik.vec)
}else  return(-likelihood)
}
