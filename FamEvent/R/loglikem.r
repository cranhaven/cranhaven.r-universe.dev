loglikem<- function(X, X0, X1, Y, theta, theta0, cuts, nbase, data, design, base.dist, agemin, vec=TRUE)
{

  xbeta <- c(X%*%theta[-c(1:nbase)])
  xbeta0 <- c(X0%*%theta[-c(1:nbase)])
  xbeta1 <- c(X1%*%theta[-c(1:nbase)])
  
  time0 <- Y[,1] - agemin
  cuts0 <- cuts - agemin
  status <- Y[,2]
  
  if(base.dist == "lognormal"){
    etheta <- c(theta[1], exp(theta[2]))
    etheta0 <- c(theta0[1], exp(theta0[2]))
  }
  else{
    etheta <-  exp(theta[1:nbase])
    etheta0 <-  exp(theta0[1:nbase])
  }
  
  wt <- data$weight

bhaz <- hazards(base.dist, time0, etheta, cuts=cuts0)
bcumhaz <- cumhaz(base.dist, time0, etheta, cuts=cuts0)

H1 <- bcumhaz*exp(xbeta1)
H0 <- bcumhaz*exp(xbeta0)
logh1 <- log(bhaz) + xbeta1
logh0 <- log(bhaz) + xbeta0

  p1 <- cprob(theta0, X1, time0, status, p=data$carrp.geno, base.dist=base.dist, cuts=cuts0, nbase=nbase)
  p0 <- cprob(theta0, X0, time0, status, p=1-data$carrp.geno, base.dist=base.dist, cuts=cuts0, nbase=nbase)
  ex1 <- p1/(p1+p0) #P(x=1|Xp, y)=P(y|x=1)*P(x=1|Xp)/(p1+p0) for EM

ex1[!is.na(data$mgene)] <- data$mgene[!is.na(data$mgene)]

p1[!is.na(data$mgene) & data$mgene==1] <- 1
p1[!is.na(data$mgene) & data$mgene==0] <- 0
p0[!is.na(data$mgene) & data$mgene==1] <- 0
p0[!is.na(data$mgene) & data$mgene==0] <- 1

  loglik <- wt * (- H1 + status*logh1 ) *ex1 + wt * (- H0 + status*logh0 ) * (1-ex1)
  loglik[data$time<=agemin] <- 0
  
# Ascertainment correction by design

ip <- which(data$proband==1)
cagep <- data$currentage[ip] - agemin
xbeta.p <- xbeta[ip]
xbeta0.p <- xbeta0[ip]
xbeta1.p <- xbeta1[ip]

bcumhaz.p <- cumhaz(base.dist, cagep, etheta, cuts=cuts0)
wt.p <- data$weight[ip]
ex1p <- ex1[ip]

#logasc.p <- wt.p*log(1-exp(-bcumhaz.p*exp(xbeta1.p))*ex1p - exp(-bcumhaz.p*exp(xbeta0.p))*(1-ex1p) ) 
logasc.p <- wt.p*log(1-exp(-bcumhaz.p*exp(xbeta.p))) 
logasc.p[logasc.p==-Inf] <- 0

if(design=="cli" | design=="cli+"){
  
  i.m <- data$relation == 4 & data$gender == 0 & data$status == 1
  i.f <- data$relation == 4 & data$gender == 1 & data$status == 1
  i.s <- data$relation == 2 & data$status == 1
  
cage.m <- data$currentage[i.m]-agemin
xbeta.m0 <- xbeta0[i.m]
xbeta.m1 <- xbeta1[i.m]
bcumhaz.m <- cumhaz(base.dist, cage.m, etheta, cuts=cuts0)

cage.f <- data$currentage[i.f]-agemin
xbeta.f0 <- xbeta0[i.f]
xbeta.f1 <- xbeta1[i.f]
bcumhaz.f <- cumhaz(base.dist, cage.f, etheta, cuts=cuts0)
                    
cage.s <- data$currentage[i.s]-agemin
xbeta.s0 <- xbeta0[i.s]
xbeta.s1 <- xbeta1[i.s]
bcumhaz.s <- cumhaz(base.dist, cage.s, etheta, cuts=cuts0)
                                        
wt.m <- data$weight[i.m]
wt.f <- data$weight[i.f]
wt.s <- data$weight[i.s]

#loglik <- wt * (- H1 + status*logh1 ) *ex1 + wt * (- H0 + status*logh0 ) * (1-ex1)

logasc.m <-  wt.m*log(1-exp(-bcumhaz.m*exp(xbeta.m1)))*ex1[i.m] + wt.p*log(1-exp(-bcumhaz.m*exp(xbeta.m0)))*(1-ex1[i.m])
logasc.f <-  wt.f*log(1-exp(-bcumhaz.f*exp(xbeta.f1)))*ex1[i.f] + wt.p*log(1-exp(-bcumhaz.f*exp(xbeta.f0)))*(1-ex1[i.f]) 
logasc.s <-  wt.s*log(1-exp(-bcumhaz.s*exp(xbeta.s1)))*ex1[i.s] + wt.p*log(1-exp(-bcumhaz.s*exp(xbeta.s0)))*(1-ex1[i.s])

sumlogasc <- sum(logasc.p, na.rm=TRUE) + sum(logasc.m,na.rm=TRUE) + sum(logasc.f,na.rm=TRUE) + sum(logasc.s,na.rm=TRUE)
loglik[i.m] <- loglik[i.m] - logasc.m
loglik[i.f] <- loglik[i.f] - logasc.f
loglik[i.s] <- loglik[i.s] - logasc.s

}
else sumlogasc <- sum(logasc.p, na.rm=TRUE)

sumloglik<- sum(loglik, na.rm=TRUE) - sumlogasc
loglik[ip] <- loglik[ip] - logasc.p

#print(c(theta,sumloglik))
if(vec) return(-loglik)
else return(-sumloglik)
            
}