#
#  event : A Library of Special Functions for Event Histories
#  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public Licence as published by
#  the Free Software Foundation; either version 2 of the Licence, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public Licence for more details.
#
#  You should have received a copy of the GNU General Public Licence
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#  SYNOPSIS
#
#  pbirth(frequencies, p, intensity="negative binomial",
#	type="spectral decomposition", print.level=0, ndigit=10,
#	gradtol=0.00001, steptol=0.00001, fscale=1, iterlim=100,
#	typsize=abs(p), stepmax=10*sqrt(p%*%p))
#
#  DESCRIPTION
#
#    Function to fit overdispersed count data as a birth process

pbirth <- function(frequencies, p, intensity="negative binomial",
	type="spectral decomposition", print.level=0, ndigit=10,
	gradtol=0.00001, steptol=0.00001, fscale=1, iterlim=100,
	typsize=abs(p), stepmax=10*sqrt(p%*%p)){
call <- sys.call()
#
# find distribution and method
#
intensity <- match.arg(intensity,c("binomial","binomial exponential",
	"binomial logistic","binomial total","Poisson","Poisson exponential",
	"negative binomial","gen negative binomial"))
type <- match.arg(type,c("spectral decomposition","series approximation"))
#
# set up frequencies
#
if(is.vector(frequencies))frequencies <- matrix(frequencies,nrow=1)
n <- dim(frequencies)[2]-1
nr <- dim(frequencies)[1]
np <- length(p)
n1 <- NULL
for(i in 1:nr)n1 <- c(n1,length(frequencies[i,!is.na(frequencies[i,])])-1)
#
# create appropriate intensity function
#
lambda <- switch(intensity,
	"binomial"=function(p,nn,n) (n-nn)*exp(p[1]),
	"binomial exponential"=function(p,nn,n) (n-nn)*exp(p[1]+p[2]*nn),
	"binomial logistic"=function(p,nn,n)
		(n-nn)*exp(p[1])/(1+exp(p[2]+p[3]*nn)),
	"binomial total"=function(p,nn,n)
		(n-nn)*exp(p[1]+p[4]*n)/(1+exp(p[2]+p[3]*nn)),
	"Poisson"=function(p,nn,n) rep(exp(p[1]),length(nn)),
	"Poisson exponential"=function(p,nn,n) exp(p[1]+p[2]*nn),
	"negative binomial"=function(p,nn,n) exp(p[1])*(exp(p[2])+nn),
	"gen negative binomial"=
		function(p,nn,n) exp(p[1])*(exp(p[2])+nn)^(1-exp(p[3])))
#
# create probability functions
#
x <- matrix(0,n+1,n+1)
i1 <- matrix(1:(n+1),n+1,2)
i2 <- cbind(1:n,2:(n+1))
prob <- function(p,n) {
	x[i1[1:(n+1),]] <- -lambda(p,0:n,n)
	x[i2[1:n,,drop=FALSE]] <- lambda(p,0:(n-1),n)
	pr <- mexp(x[1:(n+1),1:(n+1)],type=type)[1,]
	pr}
#	z <- eigen(x[1:(n+1),1:(n+1)],sym=FALSE)
#	pr <- (z$vectors%*%diag(exp(z$values))%*%solve(z$vectors))[1,]}
prob1 <- function(p){
	pr <- NULL
	for(i in 1:nr)pr <- c(pr,prob(p,n1[i]))
	pr}
#
# call nlm to optimize
#
freq <- as.vector(t(frequencies))
freq <- freq[!is.na(freq)]
like <- function(p) -sum(freq*log(prob1(p)),na.rm=TRUE)
z0 <- nlm(like,p=p, hessian=TRUE, print.level=print.level,
	typsize=typsize, ndigit=ndigit, gradtol=gradtol, stepmax=stepmax,
	steptol=steptol, iterlim=iterlim, fscale=fscale)
#
# calculate se's
#
if(np==1)cov <- 1/z0$hessian
else {
	a <- if(any(is.na(z0$hessian))||any(abs(z0$hessian)==Inf))0
		else qr(z0$hessian)$rank
	if(a==np)cov <- solve(z0$hessian)
	else cov <- matrix(NA,ncol=np,nrow=np)}
se <- sqrt(diag(cov))
nn <- sum(frequencies)
#
# back transform coefficients
#
p <- z0$estimate
pr <- prob(p,n)
pt <- vector(mode="double",np)
pr1 <- exp(-exp(p[1]))
if(intensity=="binomial")pt <- 1-pr1
else if(intensity=="Poisson")pt <- sum(pr*0:n)
else if(intensity=="negative binomial"||intensity=="gen negative binomial"){
	pr2 <- exp(p[2])
	pt[1] <- pr2/pr1-pr2
	pt[2] <- pr2
	if(intensity=="gen negative binomial")pt[3] <- 1-exp(p[3])}
else pt <- exp(-exp(p))
#
# create values from intensity function
#
an <- switch(intensity,
	"binomial"=rep(exp(p[1]),n+1),
	"binomial exponential"=exp(p[1]+p[2]*0:n),
	"binomial logistic"=exp(p[1])/(1+exp(p[2]+p[3]*0:n)),
	"binomial total"=exp(p[1]+p[4]*n)/(1+exp(p[2]+p[3]*0:n)),
	"Poisson"=rep(exp(p[1]),n+1),
	"Poisson exponential"=exp(p[1]+p[2]*0:n),
	"negative binomial"=exp(p[1])*(exp(p[2])+0:n),
	"gen negative binomial"=
		exp(p[1])*(exp(p[2])+0:n)^(1-exp(p[3])))
fitted.values <- nn*pr
residuals <- (frequencies-fitted.values)/sqrt(fitted.values)
z1 <- list(
	call=call,
	intensity=intensity,
	lambda=lambda,
	an=an,
	frequencies=frequencies,
	maxlike=z0$minimum,
	aic=z0$minimum+np,
	fitted.values=fitted.values,
	prob=pr,
	residuals=residuals,
	initial.values=p,
	coefficients=p,
	pt=pt,
	se=se,
	cov=cov,
	corr=cov/(se%o%se),
	gradient=z0$gradient,
	iterations=z0$iterations,
	error=z0$error,
	code=z0$code)
class(z1) <- "pbirth"
return(z1)}

### standard method
###

deviance.pbirth <- function(object, ...) 2*object$maxlike

### print method
###
print.pbirth <- function(x, ...){
  z <- x ## legacy / S3methods consistency
np <- length(z$coefficients)
cat("\nCall:",deparse(z$call),sep="\n")
cat("\n")
t <- deparse(z$lambda)
cat(z$intensity,"intensity function:",t[2:length(t)],sep="\n")
cat("-Log likelihood   ",z$maxlike,"\n")
cat("AIC               ",z$aic,"\n")
cat("Iterations        ",z$iterations,"\n\n")
cat("Coefficients:\n")
coef.table <- cbind(z$coefficients,z$se,z$pt)
dn <- paste("p",1:np,sep="")
dimnames(coef.table) <- list(dn,c("estimate","se","parameter"))
print.default(coef.table,digits=4,print.gap=2)
if(np>1){
	cat("\nCorrelations:\n")
	dimnames(z$corr) <- list(seq(1,np),seq(1,np))
	print.default(z$corr,digits=4)}
invisible(z)}
