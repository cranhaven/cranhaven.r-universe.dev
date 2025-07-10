##
##  scaleboot: R package for multiscale bootstrap
##  Copyright (C) 2006-2008 Hidetoshi Shimodaira
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
######################################################################
### INTERNAL: MODEL DESCRIPTIONS


##
## lambda :
##   if a numeric is specified, mixing between bayes (lambda=0) and freq (lambda=1)
##   if unspecified (default), calculate psi for fitting bp
##



#### get function
sbpsiget <- function(model) get(paste("sbpsi",model,sep="."))
sbprbget <- function(model) get(paste("sbprb",model,sep="."))



########################################
##
## models with infinite depth
##

##
## onesided: basic onesided models
##

sbprb.onesided <- function(psifunc,beta,s,sm,check) {
  if(check) return(psifunc(beta,check=check))
  a1 <- -psifunc(beta,s)/sqrt(s)
  p1 <- pnorm(a1)
  if(is.null(sm)) return(p1)

  s2 <- s+sm
  a2 <- -psifunc(beta,s2)/sqrt(s2)
  p2 <- pnorm(a2)
  p12 <- prnorm2(a1,a2,sqrt(s)/sqrt(s2))
  c(p1,p2,p12)
}


##
## modified onesided
##

sbpsi.monesided <- function(psifunc,beta,s,k,sp,lambda,check) {
  m <- length(beta)
  betam <- beta[m]
  beta <- beta[-m]
  if(check) {
    a <- psifunc(beta,check=check)
    if(!is.null(a)) 
      a <- list(beta=c(a$beta,betam),mask=c(a$mask,TRUE))
    return(a)
  }

  psifunc(beta,s,k,sp,lambda)
}

sbprb.monesided <- function(psifunc,beta,s,sm,check,mfunc) {
  m <- length(beta)
  betam <- beta[m]
  beta <- beta[-m]
  if(check) {
    a <- psifunc(beta,check=check)
    if(!is.null(a)) 
      a <- list(beta=c(a$beta,betam),mask=c(a$mask,TRUE))
    return(a)
  }

  op <- sboptions()

  a1 <- -psifunc(beta,s)/sqrt(s)
  p1 <- pnorm(a1)
  if(is.null(sm)) return(p1)

  s2 <- s+sm
  a2 <- -psifunc(beta,s2)/sqrt(s2)
  p2 <- pnorm(a2)

  if(op$cor.pom) {
    s3 <- mfunc(s,sm,beta,betam)
#    rho <- sqrt(s/s2)-0.5*betam*s3
    rho <- sqrt(s/s2)-0.5*s3
    if(rho< -0.9999) rho <- -0.9999
    if(rho>  0.9999) rho <-  0.9999
    p12 <- prnorm2(a1,a2,rho)
  } else {
    rho <- sqrt(s/s2)
    s3 <- mfunc(s,sm,beta,betam)
    p12 <- prnorm2(a1,a2,rho) -0.5*denorm2(a1,a2,rho)*s3
  }
  
  c(p1,p2,p12)
}

mfunc.sim <- function(s,sm,beta,betam) {
  ## only valid for sing.3
  beta1 <- beta[2]
  beta2 <- beta[3]
  ## expand around sigma=1
  c1 <- beta1*beta2*(3-2*beta2)
  c2 <- beta1*(beta2-1)^2
  ##
  s2 <- s+sm
  rho <- sqrt(s/s2)
  betam*(c1*c1*rho*(1-rho)+ 2*c2*c2*rho*sm+ 2*c1*c2*sqrt(s)*(1-rho*rho))
}
mfunc.pom <- function(s,sm,beta,betam) {
  beta1 <- beta[2]
  c2 <- beta1
  s2 <- s+sm
  rho <- sqrt(s/s2)  
  betam*2*c2*c2*rho*sm
}


### psi function for polynomial model
## model name convention: poly.1, poly.2,... (poly."k0")
##
## beta : coefficients of psi = sum_{i=0}^{k0-1} beta[i+1] s^i
## s : s = sigma^2 (default: s=1)
## k0-1 : polynomial degree of psi (determined from beta)
## k : use derivatives up to k-1 (default: k=1)
## sp : prediction for s=sp (default: sp=-1)
## aux : ignored
## check : check if beta is at boundary
##
## output: y = sum_{j=0}^{k-1} (d^j psi)/(d s^j) (sp-s)^j/j!
##           = sum_{j=0}^{k-1} (sp-s)^j/j! *
##             sum_{i=j}^{k0-1} beta[i+1] i(i-1)...(i-j+1) s^{i-j}
##
## details:
## (d^j s^i)/(d s^j) = i(i-1)...(i-j+1) s^{i-j} for j=0,...,i
## (d^j s^i)/(d s^j) = 0 for j=i+1,...
## (d^j psi)/(d s^j) = sum_{i=j}^{k0-1} beta[i+1] i(i-1)...(i-j+1) s^{i-j}
## a = (sp-s)^j/j! 
## b = {0@j,j!/0!,(j+1)!/1!,(j+2)!/2!,...,(k0-1)!/(k0-j-1)!}  # length=k0
## s0 = {1,s,s^2,...,s^(k0-1)}  # length=k0

sbpsi.poly <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE) {
  if(check) {
    b <- beta
    op <- sboptions()
    low <- sbmagini(length(b),op$low1.poly)
    upp <- sbmagini(length(b),op$upp1.poly)
    a1 <- b<=low+0.01; b[a1] <- low[a1]
    a2 <- b>=upp-0.01; b[a2] <- upp[a2]
    y <- !(a1|a2) # valid parameter range
    if(all(y)) return(NULL) else return(list(beta=b,mask=y))    
  }
  k0 <-length(beta) 
  y <- s0 <- s^(0:(k0-1)) 
  a <- b <- 1
  for(j in seq(1,length=min(k-1,k0-1))) {
    a <- a*(sp-s)/j
    b <- b*c(rep(0,j),1:(k0-j))
    y <- y+a*b*c(rep(0,j),s0[1:(k0-j)])
  }
  sum(y*beta)
}

### probabiliy function for polynomial model
##
## beta : parameters
## s : scale (sigma^2)
## sm : scales for multistep (only 2-step is implemented)
##
sbprb.poly <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.onesided(sbpsi.poly,beta,s,sm,check) 


### pom: modified poly models
sbprb.pom <- function(beta,s=1,sm=NULL,check=FALSE) {
  sbprb.monesided(sbpsi.poly,beta,s,sm,check,mfunc.pom)
}

sbpsi.pom <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE) {
  sbpsi.monesided(sbpsi.poly,beta,s,k,sp,lambda,check)
}


### design matrix for polynomial model
##
## sa : vector of sigma^2's
## output: design matrix of z-value (instead of psi)

sbmat.poly <- function(par,sa,mag=1) {
  k0 <-length(par)
  m0 <- length(sa)
  mag <- rep(mag,length=k0)
  x <- matrix(0,m0,k0)
  dimnames(x) <- list(names(sa),names(par))
  for(i in 1:k0) x[,i] <- mag[i]*sa^(i-1.5)
  x
}


### psi function for singular model
## model name convention: sing.(k0+1) (k0=2,3,4,...)
##
## beta : beta={b[0],b[1],...,b[k0-1],b[k0]}
##            ={beta[[1]],...,beta[[k0+1]]}
## s : s = sigma^2 (default: s=1)
## k : use derivatives up to k-1 (default: k=1)
## sp : prediction for s=sp (default: sp=-1)
## aux : ignored
##
## singularity parameter : 0 <= b[k0] <= 1
## a = x/(1-x) with x=b[k0] so that 0 <= a <= inf
##
## psi = b[0] + sum_{i=1}^{k0-1} b[i]* (1+a)*s^i / (1+a*sqrt(s))
## 
## output: y = sum_{j=0}^{k-1} (d^j psi)/(d s^j) (sp-s)^j/j!
##     = b[0] + sum_{i=1}^{k0-1} b[i]*(1+a)*
##    sum_{j=0}^{k-1} ( d^j (s^i / (1+a*sqrt(s))) )/( d s^j ) * (sp-s)^j/j!
##
## details:
## b = 1+ a*sqrt(s)
## fj = (1/b)*(sp-s)^j/j!* (2*s*b)^(-j) ; j=0,1,...,k-1
## xj = sbsingd(a*sqrt(s),i,j); j=0,...,k-1
## wi = 1 for i=0;
##    = sum_{j=0}^{k-1} ( d^j (s^i / (1+a*sqrt(s))) )/( d s^j ) * (sp-s)^j/j!
##      for i=1,...,k0-1
##    = s^i*sum_{j=0}^{k-1} fj*xj
## y = b[0] + sum_{i=1}^{k0-1} b[i]*wi[i]*(1+a)
##
##
## When a is infinite:
##   psi = b[0] + sum_{i=1}^{k0-1} b[i]* s^(i-0.5)
##
## output: y = sum_{j=0}^{k-1} (d^j psi)/(d s^j) (sp-s)^j/j!
##     = b[0] + sum_{i=1}^{k0-1} b[i]*
##    sum_{j=0}^{k-1} ( d^j s^(i-0.5) )/( d s^j ) * (sp-s)^j/j!
##     = b[0] + sum_{i=1}^{k0-1} b[i]*w[i]
##
##  w[i] = sum_{j=0}^{k-1} ( d^j s^(i-0.5) )/( d s^j ) * (sp-s)^j/j!
##       = sum_{j=0}^{k-1} (i-0.5)(i-1.5)...(i-j+0.5) s^{i-j-0.5} * (sp-s)^j/j!
##       = sum_{j=0}^{k-1} x[j]*f[j]
##  x[j] = (i-0.5)(i-1.5)...(i-j+0.5) s^{i-j-0.5} (for each i)
##  f[j] =(sp-s)^j/j!; j=0,1,...,k-1
##
## note:
## (d^j s^(i-0.5))/(d s^j) = (i-0.5)(i-1.5)...(i-j+0.5) s^{i-0.5-j} for j>=1
##

sbpsi.sing <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE) {
  k0 <- length(beta)-1 # number of polynomial coefficients
  if(k0<2) stop("length(beta) must >= 3")
  b <- beta[1:k0]
  u <- beta[k0+1]
  if(check) {
    op <- sboptions()
    low <- sbmagini(length(b),op$low1.poly)
    upp <- sbmagini(length(b),op$upp1.poly)
    a1 <- b<=low+0.01; b[a1] <- low[a1]
    a2 <- b>=upp-0.01; b[a2] <- upp[a2]
    a5 <- u <= 0.01; u[a5] <- 0
    a6 <- u >= 0.99; u[a6] <- 1
    
    beta[1:k0] <- b[1:k0]
    beta[k0+1] <- u
    y <- !c((a1|a2),(a5|a6)) # valid parameter range
    if(all(y)) return(NULL) else return(list(beta=beta,mask=y))
  }
  if(u<0) u<-0 else if(u>1) u<-1
  a <- u/(1-u)
  if(is.finite(a)) {
    ## |a| < oo
    ## calculate factors for beta's
    a0 <- a*sqrt(s); b <- 1 + a0 # for convenience
    fj <- (1/b)*((sp-s)/(2*s*b))^(0:(k-1))/c(1,cumprod(seq(length=k-1)))
    xj <- rep(0,k) # j=0,...,k-1
    wi <- rep(0,k0-1)  # i=1,...,k0-1
    for(i in 1:(k0-1)) {
      for(j in 0:(k-1)) xj[j+1] <- sbsingd(a0,i,j)
      wi[i] <- s^i*sum(fj*xj)
    }
    wi <- wi*(1+a)
  } else {
    ## |a| = oo
    fj <- (sp-s)^(0:(k-1))/c(1,cumprod(seq(length=k-1)))
    xj <- rep(0,k) # j=0,...,k-1
    wi <- rep(0,k0-1)  # i=1,...,k0-1
    for(i in 1:(k0-1)) {
      xj[1] <- s^(i-0.5)
      for(j in seq(1,length=k-1)) xj[j+1] <- xj[j]*(i-j+0.5)/s
      wi[i] <- sum(fj*xj)
    }
  }
  ## output
  as.numeric(sum(wi*beta[-c(1,k0+1)])+beta[1])
}

sbprb.sing <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.onesided(sbpsi.sing,beta,s,sm,check) 


## ### sim0: modified sing model with betak=1 fixed
## sbprb.sim0 <- function(beta,s=1,sm=NULL,check=FALSE) {
##   m <- length(beta)
##   beta <- c(beta[-m],1,beta[m])  # singular parm=1 (fixed)
##   a <- sbprb.monesided(sbpsi.sing,beta,s,sm,check,mfunc.sim)
##   if(check && !is.null(a)) {
##     a <- list(beta=a$beta[-m],mask=a$mask[-m])
##   }
##   return(a)
## }

## sbpsi.sim0 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE) {
##   m <- length(beta)
##   beta <- c(beta[-m],1,beta[m])  # singular parm=1 (fixed)
##   sbpsi.monesided(sbpsi.sing,beta,s,k,sp,lambda,check)
## }


### sim: modified sing model with betak variable
sbprb.sim <- function(beta,s=1,sm=NULL,check=FALSE) {
  sbprb.monesided(sbpsi.sing,beta,s,sm,check,mfunc.sim)
}

sbpsi.sim <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE) {
  sbpsi.monesided(sbpsi.sing,beta,s,k,sp,lambda,check)
}


##
## sbsingd(x,i,j) calculates
## 2^j * s^(-i+j) * (1+ a*sqrt(s))^(j+1)  D[ s^i/(1+a*sqrt(s)), {s,j} ]
## with s = x^2/a^2
##
## Thus, D[ s^i/(1+a*sqrt(s)), {s,j} ] =
##   sbsingd(a*sqrt(s),i,j) * 2^(-j) * s^(i-j) * (1+ a*sqrt(s))^(-j-1)
##   
sbsingd <- function(x,i,j) {
  Power <- function(x,y) x^y
  switch(j+1,
         ## j=0
         1,

         ## j=1
         2*i + (-1 + 2*i)*x,

         ## j=2
         4*(-1 + i)*i + (1 + 4*i*(-3 + 2*i))*x + 
         (3 + 4*(-2 + i)*i)*Power(x,2),

         ## j=3
         8*(-2 + i)*(-1 + i)*i + (-3 + 6*i*(11 + 2*i*(-7 +
         2*i)))*x + 12*(-1 + 2*Power(-2 + i,2)*i)*Power(x,2) + (-15 +
         2*i*(23 + 2*i*(-9 + 2*i)))*Power(x,3),

         ## j=4
         16*(-3 + i)*(-2 + i)*(-1 + i)*i + (15 - 496*i +
         824*Power(i,2) - 416*Power(i,3) + 64*Power(i,4))*x + (75 +
         24*(-2 + i)*i*(-7 + 2*i)*(-3 + 2*i))*Power(x,2) + (141 +
         8*i*(-120 + i*(145 - 60*i + 8*Power(i,2))))*Power(x,3) + (-7
         + 2*i)*(-5 + 2*i)*(-3 + 2*i)*(-1 + 2*i)*Power(x,4),

         ## j=5
         32*(-4 + i)*(-3 + i)*(-2 + i)*(-1 + i)*i + (-105 +
         10*i*(475 + 4*i*(-231 + 2*i*(77 + i*(-21 + 2*i)))))*x +
         10*(-63 + 2*i*(-7 + 2*i)*(-3 + 2*i)*(29 + 4*(-6 +
         i)*i))*Power(x,2) + (-1530 + 80*(-4 + i)*i*(-51 + 2*i*(34 +
         i*(-15 + 2*i))))*Power(x,3) + 10*(-183 + 2*i*(575 + 4*i*(-195
         + 2*i*(52 + (-12 + i)*i))))*Power(x,4) + (-9 + 2*i)*(-7 +
         2*i)*(-5 + 2*i)*(-3 + 2*i)*(-1 + 2*i)*Power(x,5),

         ## j=6
         64*(-5 + i)*(-4 + i)*(-3 + i)*(-2 + i)*(-1 + i)*i +
         (945 + 12*(-4 + i)*i*(1151 + 4*i*(-552 + i*(357 - 92*i +
         8*Power(i,2)))))* x + (6615 + 60*(-4 + i)*i*(709 + 4*i*(-316
         + i*(195 + 4*(-12 + i)*i))))* Power(x,2) + (19530 + 40*i*(-11
         + 2*i)* (657 + i*(-11 + 2*i)*(101 - 44*i +
         8*Power(i,2))))*Power(x,3) + 30*(1029 + 4*i*(-2399 + i*(4043
         + 4*i*(-684 + i*(221 + 2*(-17 + i)*i)))))* Power(x,4) +
         3*(8895 + 4*i* (-13370 + i*(19663 + 4*i*(-3080 + i*(945 +
         4*i*(-35 + 2*i))))))* Power(x,5) + (-11 + 2*i)*(-9 + 2*i)*(-7
         + 2*i)*(-5 + 2*i)*(-3 + 2*i)* (-1 + 2*i)*Power(x,6),

         ## j=7
         128*(-6 + i)*(-5 + i)*(-4 + i)*(-3 + i)*(-2 + i)* (-1
         + i)*i + (-10395 + 14*i* (53967 + 2*i*(-63457 + 2*i* (28459 +
         2*i*(-6295 + 2*i*(733 - 86*i + 4*Power(i,2)))))))*x +
         168*(-495 + 2*i*(8069 + 2*i* (-9027 + 2*i*(3923 + 2*i*(-845 +
         i*(192 + (-22 + i)*i))))))* Power(x,2) + (-288225 + 70*i*
         (79239 + 2*i*(-83367 + 2*i* (34867 + 2*i*(-7281 + 2*i*(805 -
         90*i + 4*Power(i,2)))))))* Power(x,3) + 560*(-999 + 4*(-6 +
         i)*i* (-523 + 2*i*(466 + i*(-329 + i*(109 + (-17 +
         i)*i)))))*Power(x,4) + 21*(-30945 + 2*i*(132295 + 2*i*
         (-116973 + 2*i*(44059 + 2*i*(-8515 + 2*i*(885 - 94*i +
         4*Power(i,2)))))))*Power(x,5) + 56*(-7785 + 2*i*(22809 + 2*i*
         (-17829 + 2*i*(6263 + 2*i*(-1155 + i*(232 + (-24 +
         i)*i))))))* Power(x,6) + (-13 + 2*i)*(-11 + 2*i)*(-9 +
         2*i)*(-7 + 2*i)*(-5 + 2*i)* (-3 + 2*i)*(-1 + 2*i)*Power(x,7),

         ## j=8
         256*(-7 + i)*(-6 + i)*(-5 + i)*(-4 + i)*(-3 + i)*(-2 +
         i)*(-1 + i)*i + (135135 + 16*(-6 + i)*i*(123649 +
         2*i*(-144702 + i*(128469 + 8*i*(-6984 + i*(1587 + 4*i*(-45 +
         2*i))))) ))*x + (1216215 + 112*(-6 + i)*i* (72303 +
         2*i*(-81058 + i* (70139 + 8*i*(-3728 + i*(829 + 4*(-23 +
         i)*i))))))*Power(x,2) + (4833675 + 112*i*(-1035540 +
         i*(2376999 + 2*i*(-1108552 + i*(537271 + 8*i*(-18380 +
         i*(2861 + 4*i*(-59 + 2*i))))))))* Power(x,3) + 35*(316305 +
         16*i*(-15 + 2*i)* (21116 + i*(-15 + 2*i)*(2859 + 2*i*(-15 +
         2*i)*(66 + i*(-15 + 2*i)))))* Power(x,4) + 7*(2274075 + 16*i*
         (-1597770 + i*(3215709 + 2*i*(-1385828 + i*(632521 +
         8*i*(-20590 + i*(3071 + 4*i*(-61 + 2*i))))))))*Power(x,5) +
         7*(2076435 + 16*i*(-1045242 + i*(1921509 + 2*i*(-785348 +
         i*(345233 + 8*i*(-10918 + i*(1591 + 4*(-31 + i)*i)))))))*
         Power(x,6) + (7921305 + 16*i* (-2857176 + i*(4689403 +
         2*i*(-1797264 + i*(756651 + 8*i*(-23184 + i*(3297 + 4*i*(-63
         + 2*i))))))))*Power(x,7) + (-15 + 2*i)*(-13 + 2*i)*(-11 +
         2*i)*(-9 + 2*i)*(-7 + 2*i)*(-5 + 2*i)* (-3 + 2*i)*(-1 +
         2*i)*Power(x,8),

         ## j=9
         512*(-8 + i)*(-7 + i)*(-6 + i)*(-5 + i)*(-4 + i)*(-3 +
         i)*(-2 + i)*(-1 + i)* i + (-2027025 + 18*i*(11698935 +
         8*i*(-3863707 + 2*i*(2047589 + i*(-1150051 + 2*i* (189357 +
         4*i*(-9443 + i*(1122 + i*(-73 + 2*i)))))))))*x + (-20270250 +
         36*i*(26883795 + 8*i*(-8589383 + 2*i*(4462937 + i*(-2466471 +
         2*i* (400169 + 4*i*(-19677 + 2*i*(1153 + 2*(-37 +
         i)*i))))))))* Power(x,2) + (-90810720 + 84*i* (31397157 +
         16*i*(-4822890 + i*(4896115 + 2*i*(-1327860 + i*(423783 +
         8*i*(-10260 + i*(1185 + i*(-75 + 2*i)))))))))* Power(x,3) +
         126*(-1898325 + 2*i*(18697905 + 8*i*(-5481271 + 2*i*(2705771
         + i*(-1436047 + 2*i*(224907 + 4*i*(-10709 + 2*i*(609 + (-38 +
         i)*i))))))))* Power(x,4) + 126*(-3234825 + 16*(-8 +
         i)*i*(-356355 + 2*i*(372568 + i*(-330387 + 8*i*(19218 +
         i*(-5075 + i*(764 + i*(-61 + 2*i))))))))* Power(x,5) +
         42*(-11040975 + 2*i*(57268845 + 8*i*(-14802549 + 2*i*(6784543
         + i*(-3407517 + 2*i*(510279 + 4*i*(-23391 + 2*i*(1287 +
         2*(-39 + i)*i))))))) )*Power(x,6) + 36*(-9651600 +
         i*(74461695 + 16*i*(-8836474 + i*(7709257 + 2*i*(-1869868 +
         i*(545349 + 8*i*(-12236 + i*(1323 + i*(-79 + 2*i)))))))))*
         Power(x,7) + 18*(-8822205 + 2*i*(25231533 + 8*i*(-5388695 +
         2*i*(2213735 + i*(-1031415 + 2*i*(146027 + 4*i*(-6405 +
         i*(680 + (-40 + i)*i))))))))* Power(x,8) + (-17 + 2*i)*(-15 +
         2*i)*(-13 + 2*i)*(-11 + 2*i)*(-9 + 2*i)* (-7 + 2*i)*(-5 +
         2*i)*(-3 + 2*i)*(-1 + 2*i)*Power(x,9),

         ## j=10
         1024*(-9 + i)*(-8 + i)*(-7 + i)*(-6 + i)*(-5 + i)*(-4
         + i)*(-3 + i)* (-2 + i)*(-1 + i)*i + (34459425 + 20*(-8 +
         i)*i*(26012223 + 8*i*(-8571528 + i*(9040577 + 4*i*(-1259592 +
         i*(410391 + 2*i*(-40368 + i*(4713 + 4*i*(-75 + 2*i)))))))))*
         x + 45*(8423415 + 4*(-8 + i)*i* (14732877 + 8*i*(-4714136 +
         i*(4889267 + 4*i*(-671744 + i*(216045 + 2*i*(-20984 + i*(2419
         + 4*(-38 + i)*i))))))))* Power(x,2) + 60*(31486455 +
         2*i*(-541215099 + 4*i*(351577329 + 4*i*(-95548977 + i*
         (56655857 + 2*i* (-10169985 + 4*i*(577290 + i*(-83406 +
         i*(7431 + 4*i*(-93 + 2*i))))))) )))*Power(x,3) + 420*
         (13378365 + 2*i*(-157809807 + 16*i*(24661530 + i*(-26213485 +
         i*(15273383 + 2*i* (-2700453 + 4*i*(151203 + i*(-21570 +
         i*(1899 + 2*(-47 + i)*i)))))))) )*Power(x,4) + 126*(87612525
         + 4*i*(-19 + 2*i)*(19746585 + i*(-19 + 2*i)*(2373187 +
         4*i*(-19 + 2*i)*(27233 + i*(-19 + 2*i)*(545 - 76*i +
         8*Power(i,2))))))*Power(x,5) + 210*(71138925 +
         4*i*(-228163527 + i*(515965125 + 8*i*(-64650042 + i*(36043333
         + 4*i* (-3072165 + i*(666657 + 2*i*(-46236 + i*(3969 + 4*(-48
         + i)*i)))))))) )*Power(x,6) + 60*(233731575 +
         2*i*(-1141183449 + 16*i*(150943329 + i*(-145586179 +
         i*(78940805 + 2*i*(-13165131 + 4*i*(701379 + i*(-95802 +
         i*(8115 + 4*i*(-97 + 2*i))))))) )))*Power(x,7) + 180*
         (49358925 + 2*i*(-184413633 + 4*i*(89988433 + 4*i*(-20709339
         + i*(10869459 + 2*i* (-1768375 + 4*i*(92365 + i*(-12412 +
         i*(1037 + (-49 + i)*i)))))))))* Power(x,8) + 5*(697225725 +
         4*i*(-992188494 + i*(1753973595 + 8*i*(-190735380 + i*
         (96375257 + 4*i* (-7624386 + i*(1559019 + 2*i*(-102960 +
         i*(8481 + 4*i*(-99 + 2*i))))) )))))*Power(x,9) + (-19 +
         2*i)*(-17 + 2*i)*(-15 + 2*i)*(-13 + 2*i)*(-11 + 2*i)*(-9 +
         2*i)* (-7 + 2*i)*(-5 + 2*i)*(-3 + 2*i)*(-1 + 2*i)*Power(x,10)
  )
}

### psi function for spherical model
## model name convention: sphe.3
##
## beta : parameter vector
## beta[1] = v
## beta[2] = logsx(1/a) for the region = {y : ||y||<a}
## beta[3] = log(nu) where nu=degrees of freedom
##
## s : s = sigma^2 (default: s=1)
## k : use derivatives up to k-1 (default: k=1)
## if(k==0) then, returns z-value of chisq p-value
## sp : prediction for s=sp (default: sp=-1)
## aux : ignored
## check : check if beta is at boundary
##
## output:
## psi(s) = sqrt(s)*qnorm(1-bp)
## sum_{j=0}^{k-1} ((sp-s)^j/j!) * (d^j psi(s)/d s^j)
##

## parameter conversion
parsphere <- function(beta) {
  if(length(beta)!=3) stop("length(beta) must = 3")
  v <- beta[1]
  a <- 1/expsx(beta[2])
  nu <- exp(beta[3])
  return(c(v,a,nu))
}

## psi function
sbpsi.sphe <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE) {
  op <- sboptions()
  p <- parsphere(beta) # p=c(v,a,nu)
  v <- p[1]; a <- p[2]; nu <- p[3]
  if(check) {
    a1 <- a3 <- TRUE
    ## check beta0
    if((v+a)*a < 0) {
      v <- -2*a - v # it does not change the probability
      a1 <- FALSE
    }
    ## check degrees of freedom
    if(nu <= 1.01) {nu <- 1.0; a3 <- FALSE}
    ## save
    if(a1&&a3) return(NULL)
    else {
      beta[1] <- v; beta[3] <- log(nu)
      y <- c(TRUE,TRUE,a3)
      return(list(beta=beta,mask=y))
    }
  }

  ## speical case: chisq p-value
  if(k==0 || ((k!=1) && op$chisq.sphe)) {
    zval <- zsphere(p[1],p[2],p[3],1,1,au=TRUE)
    return(zval)
  }

  ## probability calculation
  s0 <- s
  mypsi <- function(s) sqrt(s)*zsphere(p[1],p[2],p[3],s,s0)
  y <- mypsi(s)

  k <- round(k)
  w <- 1
  if(is.finite(y) && k >= 2) {
    for(j in 1:(k-1)) {
      w <- w * (sp-s) / j
      d <- nderiv(mypsi,s,j)
      y <- y + w*d
    }
  }
  y
}

sbprb.sphe <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.onesided(sbpsi.sphe,beta,s,sm,check) 

## internal function
## calculate z-value for spherical model
## v = signed distance
## a = radius of hypothesis
## s = sigma^2
## s0 = s for reference (to switch chisq or norm approx)
## au: for p-value calculation
zsphere <- function(v,a,nu,s,s0=s,au=FALSE) {
  b <- sign(a)*abs(a+v)
  if(au) { a1 <- a; a <- -b; b <- -a1; v <- -v }
  if(a<0) { v <- -v; a <- -a; b <- -b; lowtail <- FALSE }
  else lowtail <- TRUE
  c <- (nu-1)/(a+b)
  sq0 <- sqrt(s0)
  z0 <- v/sq0 + c*sq0
  if(b^2/s0 < 1e5 && abs(z0)<5) {
    lowtail2 <- z0>0
    lowtail1 <- xor(lowtail,!lowtail2)
    z <- -qnorm(pchisq(a^2/s,nu,b^2/s,lower.tail=lowtail1),lower.tail=lowtail2)
  } else { ## normal approx (for improving accuracy, instead for speed)
    sigma <- sqrt(s)
    z <- v/sigma + c*sigma
  }
#  if(is.infinite(z)) browser()
  return(z[[1]])
}

## generic psi function
## zfun(s,beta)
sbpsi.generic <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE,zfun,eps=0.01) {
  if(check) return(NULL)
  mypsi <- function(s) sqrt(s)*zfun(s,beta)
  y <- mypsi(s)

  k <- round(k)
  w <- 1
  if(k >= 2) {
    for(j in 1:(k-1)) {
      w <- w * (sp-s) / j
      d <- nderiv(mypsi,s,j,eps)
      y <- y + w*d
    }
  }
  y
}

#####################################################################################
###
###  EXPERIMENTAL CODE FOR DIFFERENCE MODELS
###
###  Shimodaira (2010)
###  Ann Inst Stat Math (2010) 62:189-208 DOI 10.1007/s10463-009-0247-z
###  Frequentist and Bayesian measures of confidence via multiscale
###     bootstrap for testing three regions
###
###  Note: There are so many addition to the whole code about this experimental features
###        These parts many be removed in future.
###        They can be identified by keywords such as twosided, onesided, revsided, lambda
###        In particular, parameter "lambda" is not used in most of model sepecifications.
########################################
##
## models with finite depth
##

sbpsi.twosided <- function(func,k1,typea,typec,beta,s,k,sp,lambda,check) {
  if(check) return(func(k1,typea,typec,beta,s,k,sp,check=check))

  x <- func(k1,typea,typec,beta,s,k,sp)
  psi1 <- x[1]; psi2 <- x[2]
  
  if(is.null(lambda)) {
    sigma <- sqrt(s)
    psi <- sigma*wzval(psi1/sigma,psi2/sigma,-1)
  } else {
    psi <- wzval(psi1,psi2,2*lambda-1)
  }
  if(typea) psi else -psi
}

sbprb.twosided <- function(func,k1,typea,typec,beta,s,sm,check) {
  if(check) return(func(k1,typea,typec,beta,s,check=check))
  x1 <- -func(k1,typea,typec,beta,s)/sqrt(s)
  p1 <- pnorm(x1[1]) - pnorm(x1[2])
  if(is.null(sm)) return(if(typea) p1 else 1-p1)

  s2 <- s+sm
  x2 <- -func(k1,typea,typec,beta,s2)/sqrt(s2)
  p2 <- pnorm(x2[1]) - pnorm(x2[2])
#  if(x1[1]<=x1[2] || x2[1]<=x2[2]) browser()
  p12 <- prnorm4(x1[1],x2[1],x1[2],x2[2],sqrt(s)/sqrt(s2))

  if(typea) c(p1,p2,p12) else c(1-p1,1-p2,1+p12-p1-p2)
}


### psi generic function for polynomial-difference model
## (convex or concave)
##
## returns only (psi1,psi2)
##
## k1=1 or 2 : number of difference parameters
## typea : TRUE for type-a/c, FALSE for type-b/d
## typec : TRUE for type-c/d, FALSE for type-a/b
## beta = c(beta1,diff)
##   beta1 is for psi1
##   diff is the difference for psi2
##
sbpsipoa <- function(k1,typea,typec,beta,s=1,k=1,sp=-1,check=FALSE) {
  ## prepare beta1 and beta2
  k0 <- length(beta)-k1 # number of polynomial coefficients
  if(k0<k1) stop("too few parameters") # can be replaced by k0<1
  b <- beta[1:k0] # beta parm
  if(k0==1) b <- c(b,0)
  d <- beta[(k0+1):(k0+k1)] # dif parm
  if(k1==1) d <- c(d,0)
  else if(k1!=2) stop("k1 out of range")
  if(typea) beta1 <- b else beta1 <- -b
  if(typec) beta2 <- c(beta1[1]+d[1],-beta1[2]-d[2],-beta1[c(-1,-2)])
  else beta2 <- c(beta1[1]+d[1],beta1[2]+d[2],beta1[c(-1,-2)])

  ## checking the parameter range
  if(check) {
    op <- sboptions()
    ## check beta0
    v1 <- sum(beta1[1:2])
    v2 <- sum(beta2[1:2])
    if(abs(v1)>abs(v2)) {
      ## swap psi1 and psi2
      b <- -beta2
      a0 <- TRUE
    } else {
      b <- beta1
      a0 <- FALSE
    }
    ## check the limits
    low <- sbmagini(length(b),op$low1.poly)
    upp <- sbmagini(length(b),op$upp1.poly)
    a1 <- b<=low+0.01; b[a1] <- low[a1]
    a2 <- b>=upp-0.01; b[a2] <- upp[a2]

    low <-  op$low1.poa
    upp <-  op$upp1.poa
    a3 <- d<=low+0.01; d[a3] <- low[a3]
    a4 <- d>=upp-0.01; d[a4] <- upp[a4]
    
    ## save
    if(!typea) b <- -b 
    beta[1:k0] <- b[1:k0]
    beta[(k0+1):(k0+k1)] <- d[1:k1]
    y <- !c((a1|a2)[1:k0],(a3|a4)[1:k1]) # valid parameter range
    if(all(y)&&!a0) return(NULL) else return(list(beta=beta,mask=y))
  }

  ## compute psi functions
  psi1 <- sbpsi.poly(beta1,s,k,sp)
  psi2 <- sbpsi.poly(beta2,s,k,sp)
  c(psi1,psi2)
}

### psi functions for polynomial-difference models

sbpsi.poa1 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsipoa,k1=1,typea=TRUE,typec=FALSE,beta,s,k,sp,lambda,check)

sbpsi.poa2 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsipoa,k1=2,typea=TRUE,typec=FALSE,beta,s,k,sp,lambda,check)

sbpsi.pob1 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsipoa,k1=1,typea=FALSE,typec=FALSE,beta,s,k,sp,lambda,check)

sbpsi.pob2 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsipoa,k1=2,typea=FALSE,typec=FALSE,beta,s,k,sp,lambda,check)

sbpsi.poc1 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsipoa,k1=1,typea=TRUE,typec=TRUE,beta,s,k,sp,lambda,check)

sbpsi.poc2 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsipoa,k1=2,typea=TRUE,typec=TRUE,beta,s,k,sp,lambda,check)

sbpsi.pod1 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsipoa,k1=1,typea=FALSE,typec=TRUE,beta,s,k,sp,lambda,check)

sbpsi.pod2 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsipoa,k1=2,typea=FALSE,typec=TRUE,beta,s,k,sp,lambda,check)

### probability functions for polynomial-difference models

sbprb.poa1 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsipoa,k1=1,typea=TRUE,typec=FALSE,beta,s,sm,check)

sbprb.poa2 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsipoa,k1=2,typea=TRUE,typec=FALSE,beta,s,sm,check)

sbprb.pob1 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsipoa,k1=1,typea=FALSE,typec=FALSE,beta,s,sm,check)

sbprb.pob2 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsipoa,k1=2,typea=FALSE,typec=FALSE,beta,s,sm,check)

sbprb.poc1 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsipoa,k1=1,typea=TRUE,typec=TRUE,beta,s,sm,check)

sbprb.poc2 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsipoa,k1=2,typea=TRUE,typec=TRUE,beta,s,sm,check)

sbprb.pod1 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsipoa,k1=1,typea=FALSE,typec=TRUE,beta,s,sm,check)

sbprb.pod2 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsipoa,k1=2,typea=FALSE,typec=TRUE,beta,s,sm,check)



### psi generic function for singular-difference model
## (convex or concave)
## beta : beta={b[0],b[1],...,b[k0-1],u,d[0],...,d[k1-1]}
##
sbpsisia <- function(k1,typea,typec,beta,s=1,k=1,sp=-1,check=FALSE) {
  ## prepare beta1 and beta2
  k0 <- length(beta)-k1-1 # number of polynomial coefficients
  if(k0<2) stop("too few parameters")
  b <- beta[1:k0]
  u <- beta[k0+1] # written as x in sbpsi.sing
  d <- beta[(k0+2):(k0+1+k1)] # dif parm
  if(k1==1) d <- c(d,0)
  else if(k1!=2) stop("k1 out of range")
  if(typea) beta1 <- b else beta1 <- -b
  if(typec) beta2 <- c(beta1[1]+d[1],-beta1[2]-d[2],-beta1[c(-1,-2)])
  else beta2 <- c(beta1[1]+d[1],beta1[2]+d[2],beta1[c(-1,-2)])
  
  ## checking the parameter range
  if(check) {
    op <- sboptions()
    ## check beta0
    v1 <- sum(beta1[1:2])
    v2 <- sum(beta2[1:2])
    if(abs(v1)>abs(v2)) {
      ## swap psi1 and psi2
      b <- -beta2
      a0 <- TRUE
    } else {
      b <- beta1
      a0 <- FALSE
    }
    ## check the limits
    low <- sbmagini(length(b),op$low1.poly)
    upp <- sbmagini(length(b),op$upp1.poly)
    a1 <- b<=low+0.01; b[a1] <- low[a1]
    a2 <- b>=upp-0.01; b[a2] <- upp[a2]

    low <-  op$low1.poa
    upp <-  op$upp1.poa
    a3 <- d<=low+0.01; d[a3] <- low[a3]
    a4 <- d>=upp-0.01; d[a4] <- upp[a4]
    
    a5 <- u <= 0.01; u[a5] <- 0
    a6 <- u >= 0.99; u[a6] <- 1

    ## save
    if(!typea) b <- -b    
    beta[1:k0] <- b[1:k0]
    beta[k0+1] <- u
    beta[(k0+2):(k0+1+k1)] <- d[1:k1]
    y <- !c((a1|a2)[1:k0],(a5|a6),(a3|a4)[1:k1]) # valid parameter range
    if(all(y)&&!a0) return(NULL) else return(list(beta=beta,mask=y))
  }

  ## compute psi functions
  psi1 <- sbpsi.sing(c(beta1,u),s,k,sp)
  psi2 <- sbpsi.sing(c(beta2,u),s,k,sp)
  c(psi1,psi2)
}

### psi functions for singular-difference models

sbpsi.sia1 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsisia,k1=1,typea=TRUE,typec=FALSE,beta,s,k,sp,lambda,check)

sbpsi.sia2 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsisia,k1=2,typea=TRUE,typec=FALSE,beta,s,k,sp,lambda,check)

sbpsi.sib1 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsisia,k1=1,typea=FALSE,typec=FALSE,beta,s,k,sp,lambda,check)

sbpsi.sib2 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsisia,k1=2,typea=FALSE,typec=FALSE,beta,s,k,sp,lambda,check)

sbpsi.sic1 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsisia,k1=1,typea=TRUE,typec=TRUE,beta,s,k,sp,lambda,check)

sbpsi.sic2 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsisia,k1=2,typea=TRUE,typec=TRUE,beta,s,k,sp,lambda,check)

sbpsi.sid1 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsisia,k1=1,typea=FALSE,typec=TRUE,beta,s,k,sp,lambda,check)

sbpsi.sid2 <- function(beta,s=1,k=1,sp=-1,lambda=NULL,aux=NULL,check=FALSE)
  sbpsi.twosided(sbpsisia,k1=2,typea=FALSE,typec=TRUE,beta,s,k,sp,lambda,check)


### probability functions for singular-difference models

sbprb.sia1 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsisia,k1=1,typea=TRUE,typec=FALSE,beta,s,sm,check)

sbprb.sia2 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsisia,k1=2,typea=TRUE,typec=FALSE,beta,s,sm,check)

sbprb.sib1 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsisia,k1=1,typea=FALSE,typec=FALSE,beta,s,sm,check)

sbprb.sib2 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsisia,k1=2,typea=FALSE,typec=FALSE,beta,s,sm,check)

sbprb.sic1 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsisia,k1=1,typea=TRUE,typec=TRUE,beta,s,sm,check)

sbprb.sic2 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsisia,k1=2,typea=TRUE,typec=TRUE,beta,s,sm,check)

sbprb.sid1 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsisia,k1=1,typea=FALSE,typec=TRUE,beta,s,sm,check)

sbprb.sid2 <- function(beta,s=1,sm=NULL,check=FALSE)
  sbprb.twosided(sbpsisia,k1=2,typea=FALSE,typec=TRUE,beta,s,sm,check)

###
###  END OF EXPERIMENTAL CODE FOR DIFFERENCE MODELS
###
#####################################################################################

######################################################################
### INTERNAL: MODEL INITIAL VALUES

### decompose model names
## models : vector of model names
sbname <- function(models) {
  x <- strsplit(models,"\\.")
  x1 <- sapply(x,"[[",1)  # base name
  x2 <- as.numeric(sapply(x,"[[",2)) # parameter size
  for(i in seq(along=x)) x[[i]] <- x[[i]][c(-1,-2)] # aux information
  list(base=x1,size=x2,aux=x)
}

### utilize previous fitting
##
## size : parameter size
## y : sbfit fits
## cfun(z,size) : logical function to match the conditions
sbprevini <- function(size,y,cfun,
                      bfun=function(par1,size) {
                        size1 <- length(par1)
                        if(size1<size) c(par1,rep(0,size-size1))
                        else par1[1:size]} ) {
  z <- sbname(names(y))
  i <- which(cfun(z,size))
  if(length(i)>0) {
    i <- i[length(i)] # the last one
    prev <- bfun(y[[i]]$par*y[[i]]$mag,size)
  } else prev <- NULL
  prev
}

### set mag values
##
## size : size of beta
## mag : default mag
sbmagini <- function(size,mag) {
  len <- length(mag)
  if(size<=len) mag[1:size]
  else c(mag,rep(mag[len],size-len))
}

### wls fitting 1
## mat : function(beta,sa,mag=1) for design matrix of z-value
## init : initial beta, but only length and name are used
sbwlsfit1 <- function(bp,nb,sa,mat,init,mag=1,tol=1e-10) {
  bz <- -qnorm(bp)
  uu <- !is.infinite(bz) # use only these elements for wls
  if(sum(uu)<1) return(NULL) # no data
  bz <- bz[uu]; bp <- bp[uu]; nb <- nb[uu]; sa <- sa[uu]
  X <- mat(init,sa,mag)
  if(sum(uu)<ncol(X)) return(NULL)  # too few data
  vv <- (1-bp)*bp/(dnorm(bz)^2*nb) # var(bz)
  fit <- lsfit(X,bz,1/vv,intercept=F) # WLS(weighted least squares)
  list(par=fit$coef)
}

### initial value for polynomial model
##
## size : parameter size
## x : sbfit parameters
## y : sbfit fits
sbini.poly <- function(size,x,y,aux=NULL) {
  par <- rep(0,size)
  names(par) <- paste("beta",0:(size-1),sep="")
  inits <- as.matrix(par)

  ## set mag
  op <- sboptions()
  mag <- sbmagini(size,op$mag.poly)
  trg <- sbmagini(size,0)
  omg <- sbmagini(size,op$omg.poly)

  ## wls fitting
  fit0 <- sbwlsfit1(x$bp,x$nb,x$sa,sbmat.poly,par,mag)
  if(!is.null(fit0)) inits <- cbind(inits,fit0$par)

  ## utilize the previous poly fitting
  inits <- cbind(inits,sbprevini(size,y,
      function(z,size) z$base == "poly" & z$size < size)/mag)
  list(inits=inits,mag=mag,omg=omg,trg=trg)
}

sbini.pom <- function(size,x,y,aux=NULL) {
  a <- sbini.poly(size-1,x,y,aux)
  a$inits <- rbind(a$inits,0)
  op <- sboptions()
  a$mag <- c(a$mag, op$mag1.pom)
  a$omg <- c(a$omg, op$omg1.pom)
  a$trg <- c(a$trg, 0)
  a
}

### initial values for singular model
##
## size : parameter size
## x : sbfit parameters
## y : sbfit fits
sbini.sing <- function(size,x,y,aux=NULL) {
  par <- rep(0,size)
  names(par) <- paste("beta",0:(size-1),sep="")
  inits <- as.matrix(par)
  k0 <- size-1

  ## set mag
  op <- sboptions()
  mag <- c(sbmagini(k0,op$mag.poly),op$mag1.sing)
  trg <- c(sbmagini(k0,0),0)
  omg <- c(sbmagini(k0,op$omg.poly),op$omg1.sing)
  
  ## wls fitting
  fit0 <- sbwlsfit1(x$bp,x$nb,x$sa,sbmat.poly,rep(0,k0),mag[1:k0])
  if(!is.null(fit0)) inits <- cbind(inits,c(fit0$par,0))

  ## utilize the previous poly
  inits <- cbind(inits,sbprevini(size,y,
     function(z,size) z$base == "poly" & z$size <= k0)/mag)

  ## utilize the previous sing
  par1 <- sbprevini(size,y,
    function(z,size) z$base == "sing" & z$size < size,
    function(par1,size) {
      size1 <- length(par1)
      c(par1[1:(size1-1)],rep(0,size-size1),par1[size1])
    })
  inits <- cbind(inits,par1/mag)
  list(inits=inits,mag=mag,omg=omg,trg=trg)
}


### initial values for modified singular model
##
## size : parameter size
## x : sbfit parameters
## y : sbfit fits
sbini.sim <- function(size,x,y,aux=NULL) {
  par <- rep(0,size)
  names(par) <- paste("beta",0:(size-1),sep="")
  inits <- as.matrix(par)
  k0 <- size-2

  ## set mag
  op <- sboptions()
  mag <- c(sbmagini(k0,op$mag.poly),op$mag1.sing,op$mag1.sim)
  trg <- c(sbmagini(k0,0),0,0)
  omg <- c(sbmagini(k0,op$omg.poly),op$omg1.sing,op$omg1.sim)
  
  ## wls fitting
  fit0 <- sbwlsfit1(x$bp,x$nb,x$sa,sbmat.poly,rep(0,k0),mag[1:k0])
  if(!is.null(fit0)) inits <- cbind(inits,c(fit0$par,0,0))

  ## utilize the previous poly
  inits <- cbind(inits,sbprevini(size,y,
     function(z,size) z$base == "poly" & z$size <= k0)/mag)

  ## utilize the previous sing
  par1 <- sbprevini(size,y,
    function(z,size) z$base == "sing" & z$size <= size-1,
    function(par1,size) {
      size1 <- length(par1)
      c(par1[1:(size1-1)],rep(0,size-size1-1),par1[size1],0)
    })
  inits <- cbind(inits,par1/mag)
  
  ## utilize the previous pom
  par1 <- sbprevini(size,y,
    function(z,size) z$base == "pom" & z$size <= size-1, 
    function(par1,size) {
      size1 <- length(par1)
      c(par1[1:(size1-1)],rep(0,size-size1-1),0,par1[size1])
    })
  inits <- cbind(inits,par1/mag)

  ## utilize the previous sim
  par1 <- sbprevini(size,y,
    function(z,size) z$base == "sim" & z$size < size, 
    function(par1,size) {
      size1 <- length(par1)
      c(par1[1:(size1-2)],rep(0,size-size1),par1[c(size1-1,size1)])
    })
  inits <- cbind(inits,par1/mag)

  list(inits=inits,mag=mag,omg=omg,trg=trg)
}



### modified singular models
## sbini.sim0 <- function(size,x,y,aux=NULL) {
##   a <- sbini.sing(size,x,y,aux)
##   a$inits[size,] <- 0  
##   op <- sboptions()
##   a$mag[size] <- op$mag1.sim
##   a$omg[size] <- op$omg1.sim
##   a$trg[size] <- 0
##   a
## }

## sbini.sim <- function(size,x,y,aux=NULL) {
##   a <- sbini.sing(size-1,x,y,aux)
##   a$inits <- rbind(a$inits,0)
##   op <- sboptions()
##   a$mag <- c(a$mag, op$mag1.sim)
##   a$omg <- c(a$omg, op$omg1.sim)
##   a$trg <- c(a$trg, 0)
##   a
## }

### initial value for spherical model
##
## size : parameter size
## x : sbfit parameters
## y : sbfit fits
sbini.sphe <- function(size,x,y,aux=NULL) {
  if(size != 3) stop("size must = 3")
  par <- rep(0,size)
  names(par) <- paste("beta",0:(size-1),sep="")
  inits <- as.matrix(par)

  ## set mag
  op <- sboptions()
  mag <- op$mag.sphe
  trg <- c(0,0,0)
  omg <- op$omg.sphe

  ## find "poly.2" model
  par1 <- sbprevini(2,y,
      function(z,size) z$base == "poly" & z$size == size)
  v1 <- par1[1] # signed distance (beta0)
  c1 <- par1[2] # curvature term (beta1)
  nu1 <- 2
  inits <- cbind(inits,
                 c(v1,logsx(2*c1/(nu1-1)),log(nu1))/mag)

  list(inits=inits,mag=mag,omg=omg,trg=trg)
}

### initial value for polynomial-difference models
##
## size : parameter size
## x : sbfit parameters
## y : sbfit fits
sbinipoa <- function(size,x,y,aux=NULL,k1,typea,typec) {
  k0 <- size-k1
  if(k0<1) stop("k should be larger")
  if(k1>2 || k1<0) stop("k1 out of range")

  ## set mag
  op <- sboptions()
  mag <- c(sbmagini(k0,op$mag.poly),op$mag1.poa[1:k1])
  trg <- c(rep(0,k0),op$trg1.poa[1:k1])
  omg <- c(sbmagini(k0,op$omg.poly),op$omg1.poa[1:k1])

  ## default value
  x0 <- op$low1.poa[1] # default difference
  x1 <- op$upp1.poa[1] # maximum
  par <- rep(0,size)
  names(par) <- paste("beta",0:(size-1),sep="")
  par[k0+1] <- x0
  inits <- as.matrix(par/mag)

  ## utilize the previous poly with x0
  par1 <- sbprevini(size,y,
    function(z,size) z$base == "poly" & z$size <= k0,
    function(par1,size) {
      size1 <- length(par1)
      if(typea) a <- c(par1,rep(0,k0-size1),max(0,-par1[1]*2)+x0)
      else a <- c(par1,rep(0,k0-size1),max(0,par1[1]*2)+x0)
      if(k1==1) a else c(a,0)
     })
  inits <- cbind(inits,par1/mag)

  ## utilize the previous poly with x1
  par1 <- sbprevini(size,y,
    function(z,size) z$base == "poly" & z$size <= k0,
    function(par1,size) {
      size1 <- length(par1)
      a <- c(par1,rep(0,k0-size1),x1)
      if(k1==1) a else c(a,0)
     })
  inits <- cbind(inits,par1/mag)

  ## utilize the previous poa or pob
  if(typec) {
    if(typea) na1 <- "poc" else na1 <- "pod"
  } else {
    if(typea) na1 <- "poa" else na1 <- "pob"
  }
  na2 <- paste(na1,k1,sep="") # to find  po[ab]{k1}.k-1
  par1 <- sbprevini(size,y,
    function(z,size) z$base == na2 & z$size-k1 <= k0,
    function(par1,size) {
      size1 <- length(par1)
      c(par1[1:(size1-k1)],rep(0,size-size1),par1[(size1-k1+1):size1])
    })
  inits <- cbind(inits,par1/mag)
  if(k1==2) {
    na2 <- paste(na1,1,sep="") # to find po[ab]1.k-1
    par1 <- sbprevini(size,y,
      function(z,size) z$base == na2 & z$size-1 <= k0,
      function(par1,size) {
        size1 <- length(par1)
        c(par1[1:(size1-1)],rep(0,size-size1-1),par1[size1],0)
      })
    inits <- cbind(inits,par1/mag)
  }
  list(inits=inits,mag=mag,omg=omg,trg=trg)
}

sbini.poa1 <- function(size,x,y,aux=NULL) sbinipoa(size,x,y,aux,k1=1,typea=TRUE,typec=FALSE)
sbini.poa2 <- function(size,x,y,aux=NULL) sbinipoa(size,x,y,aux,k1=2,typea=TRUE,typec=FALSE)
sbini.pob1 <- function(size,x,y,aux=NULL) sbinipoa(size,x,y,aux,k1=1,typea=FALSE,typec=FALSE)
sbini.pob2 <- function(size,x,y,aux=NULL) sbinipoa(size,x,y,aux,k1=2,typea=FALSE,typec=FALSE)

sbini.poc1 <- function(size,x,y,aux=NULL) sbinipoa(size,x,y,aux,k1=1,typea=TRUE,typec=TRUE)
sbini.poc2 <- function(size,x,y,aux=NULL) sbinipoa(size,x,y,aux,k1=2,typea=TRUE,typec=TRUE)
sbini.pod1 <- function(size,x,y,aux=NULL) sbinipoa(size,x,y,aux,k1=1,typea=FALSE,typec=TRUE)
sbini.pod2 <- function(size,x,y,aux=NULL) sbinipoa(size,x,y,aux,k1=2,typea=FALSE,typec=TRUE)

### initial value for singular-difference models
##
## size : parameter size
## x : sbfit parameters
## y : sbfit fits
sbinisia <- function(size,x,y,aux=NULL,k1,typea,typec) {
  k0 <- size-k1-1
  if(k0<2) stop("k should be larger")
  if(k1>2 || k1<0) stop("k1 out of range")

  ## set mag
  op <- sboptions()
  mag <- c(sbmagini(k0,op$mag.poly),op$mag1.sing,op$mag1.poa[1:k1])
  trg <- c(rep(0,k0),0,op$trg1.poa[1:k1])
  omg <- c(sbmagini(k0,op$omg.poly),op$omg1.sing,op$omg1.poa[1:k1])
  
  ## default value
  x0 <- op$low1.poa[1] # default difference
  x1 <- op$upp1.poa[1] # maximum
  par <- rep(0,size)
  names(par) <- paste("beta",0:(size-1),sep="")
  par[k0+2] <- x0
  inits <- as.matrix(par/mag)

  ## utilize the previous sing with x0
  par1 <- sbprevini(size,y,
    function(z,size) z$base == "sing" & z$size-1 <= k0,
    function(par1,size) {
      size1 <- length(par1)
      if(typea) a <- c(par1[-size1],rep(0,k0-size1+1),par1[size1],max(0,-par1[1]*2)+x0)
      else a <- c(par1[-size1],rep(0,k0-size1+1),par1[size1],max(0,par1[1]*2)+x0)
      if(k1==1) a else c(a,0)      
     })
  inits <- cbind(inits,par1/mag)

  ## utilize the previous sing with x1
  par1 <- sbprevini(size,y,
    function(z,size) z$base == "sing" & z$size-1 <= k0,
    function(par1,size) {
      size1 <- length(par1)
      a <- c(par1[-size1],rep(0,k0-size1+1),par1[size1],x1)
      if(k1==1) a else c(a,0)      
     })
  inits <- cbind(inits,par1/mag)

  ## utilize the previous sia or pib
  ## utilize the previous poa or pob
  if(typec) {
    if(typea) na1 <- "sic" else na1 <- "sid"
  } else {
    if(typea) na1 <- "sia" else na1 <- "sib"
  }
  na2 <- paste(na1,k1,sep="") # to find  si[ab]{k1}.k-1
  par1 <- sbprevini(size,y,
    function(z,size) z$base == na2 & z$size-k1-1 <= k0,
    function(par1,size) {
      size1 <- length(par1)
      c(par1[1:(size1-k1-1)],rep(0,size-size1),par1[(size1-k1):size1])
    })
  inits <- cbind(inits,par1/mag)
  if(k1==2) {
    na2 <- paste(na1,1,sep="") # to find si[ab]1.k-1
    par1 <- sbprevini(size,y,
      function(z,size) z$base == na2 & z$size-2 <= k0,
      function(par1,size) {
        size1 <- length(par1)
        c(par1[1:(size1-2)],rep(0,size-size1-1),par1[(size1-1):size1],0)
      })
    inits <- cbind(inits,par1/mag)
  }
  list(inits=inits,mag=mag,omg=omg,trg=trg)
}

sbini.sia1 <- function(size,x,y,aux=NULL) sbinisia(size,x,y,aux,k1=1,typea=TRUE,typec=FALSE)
sbini.sia2 <- function(size,x,y,aux=NULL) sbinisia(size,x,y,aux,k1=2,typea=TRUE,typec=FALSE)
sbini.sib1 <- function(size,x,y,aux=NULL) sbinisia(size,x,y,aux,k1=1,typea=FALSE,typec=FALSE)
sbini.sib2 <- function(size,x,y,aux=NULL) sbinisia(size,x,y,aux,k1=2,typea=FALSE,typec=FALSE)

sbini.sic1 <- function(size,x,y,aux=NULL) sbinisia(size,x,y,aux,k1=1,typea=TRUE,typec=TRUE)
sbini.sic2 <- function(size,x,y,aux=NULL) sbinisia(size,x,y,aux,k1=2,typea=TRUE,typec=TRUE)
sbini.sid1 <- function(size,x,y,aux=NULL) sbinisia(size,x,y,aux,k1=1,typea=FALSE,typec=TRUE)
sbini.sid2 <- function(size,x,y,aux=NULL) sbinisia(size,x,y,aux,k1=2,typea=FALSE,typec=TRUE)

######################################################################
### EOF
