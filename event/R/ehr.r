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
#	pp(y, censor=1)
#	ident(y, id)
#	tpast(y)
#	ttime(y, id)
#	bp(y, id, censor=1)
#	tccov(y, x, id)
#	tvcov(y, x, tx)
#	vdm(y, x, id=NULL, tx=NULL, factor=FALSE, time=FALSE)
#	ehr(point, lambda=NULL, linear=NULL, plambda=NULL, wt=1,
#		envir=parent.frame(), print.level=0,
#		typsize=rep(1,length(plambda)), ndigit=10,
#		gradtol=0.00001, stepmax=max(10*sqrt(plambda%*%plambda),10),
#		steptol=0.0004, iterlim=100, fscale=1)
#
#  DESCRIPTION
#
#    Functions for setting up and fitting counting process models

### point process created from times (y) between events
###
pp <- function(y, censor=1){
#
# y must contain integers
#
if(!is.vector(y,mode="numeric"))stop("y must be a numeric vector")
if(min(y)<=0)stop("All times must be positive")
if(any(round(y)!=y))stop("Times must be integers")
if(any(censor!=0&&censor!=1))stop("Censor indicator must be zeros and ones")
if(length(censor)!=1&&length(censor)!=length(y))
	stop("Time and censor vectors must be the same length")
#
# create vector of zeros with ones at events
#
point <- rep(0, sum(y))
point[cumsum(y)] <- censor
point}

### individual identification vector
###
ident <- function(y, id){
if(min(y)<=0)stop("All times must be positive")
if(!is.vector(id,mode="numeric"))stop("id must be a numeric vector")
if(length(y)!=length(id))stop("Time and id vectors must be the same length")
if(length(unique(id))!=max(id)-min(id)+1||min(id)!=1)
	stop("id must be consecutive numbers")
rep(id, y)}

### time past since previous event
###
tpast <- function(y){
if(min(y)<=0)stop("All times must be positive")
unlist(lapply(as.list(y), seq))}

### total time elapsed for each individual
###
ttime <- function(y, id){
if(length(idd <- ident(y,id))==1)return(idd)
z <- capply(rep(1,length(idd)),idd,cumsum)
names(z) <- NULL
z}

### number of previous events for each individual, for birth processes
### one should be added, if process starts at an event
###
bp <- function(y, id, censor=1){
bp1 <- function(i) c(0,cumsum(i)[1:(length(i)-1)])
if(length(point <- pp(y, censor=censor))==1)return(point)
if(length(idd <- ident(y, id))==1)return(idd)
z <- capply(point, idd, bp1)
names(z) <- NULL
z}

### time-constant covariate - id must be numbered consecutively
### x has one value for each distinct id
###
tccov <- function(y, x, id){
if(length(y)!=length(id))stop("Time and id must be the same length")
if(length(x)!=length(unique(id)))
	stop("There must be one covariate value per individual")
if(length(idd <- ident(y, id))==1)return(idd)
x[idd]}

### time-varying covariate - tx gives the times at which x changes
### may also be used to create weight vector
###
tvcov <- function(y, x, tx){
if(min(y)<=0||min(tx)<0)stop("All times must be positive")
if(length(x)!=length(tx))
	stop("Covariate and time vectors must be the same length")
if(sum(y)!=sum(tx))stop("Total response time must equal total covariate time")
rep(x, tx)}

### design matrix
###
vdm <- function(y, x, id=NULL, tx=NULL, factor=FALSE, time=FALSE) {
if(time){
	if(length(xx <- tvcov(y, x, tx))==1)return(xx)}
else if(length(xx <- tccov(y, x, id))==1)return(xx)
if(factor)xx <- factor(xx)
wr(~xx)$design}

### fit an intensity function to event histories, where point is
### produced by point <- pp(y) and lambda is the log intensity function
###
ehr <- function(point, lambda=NULL, linear=NULL, plambda=NULL, delta=1,
	envir=parent.frame(), print.level=0, typsize=rep(1,length(plambda)),
	ndigit=10, gradtol=0.00001, iterlim=100, fscale=1,
	stepmax=max(10*sqrt(plambda%*%plambda),10), steptol=0.0004){
call <- sys.call()
if(any(point<0))stop("Response vector must be non-negative integers")
n <- length(point)
npl <- length(plambda)
#
# set up deltas if necessary
#
dt <- any(delta>1)
if(dt){
	if(length(point)!=length(delta))stop("point and delta must be the same length")
	delta <- log(delta)}
#
# find linear part of each regression and save model for printing
#
if(inherits(linear,"formula")&&is.null(lambda)){
	lambda <- linear
	linear <- NULL}
if(inherits(linear,"formula")){
	linmodel <- if(!is.null(attr(finterp(linear,.envir=envir),"parameters")))
			attr(finterp(linear,.envir=envir),"model")}
else linmodel <- NULL
#
# check if linear contains W&R formula
#
if(inherits(linear,"formula")){
	tmp <- attributes(finterp(linear,.envir=envir))
	lf1 <- length(tmp$parameters)
	if(!is.character(tmp$model))stop("linear must be a W&R formula")
	else if(length(tmp$model)==1)stop("linear must contain covariates")
	rm(tmp)}
else lf1 <- 0
#
# transform formula to function and check number of parameters
#
if(inherits(lambda,"formula")){
	linarg <- if(lf1>0) "linear" else NULL
	lambda4 <- finterp(lambda,.envir=envir,.args=linarg)
	npt1 <- length(attr(lambda4,"parameters"))
	if(is.character(attr(lambda4,"model"))){
	# W&R formula
		if(length(attr(lambda4,"model"))==1){
		# intercept model
			lambda3 <- if(dt)
				function(p) lambda(p,p[lf1]*rep(1,n))+delta
			else function(p) lambda(p,p[lf1]*rep(1,n))
			attributes(lambda3) <- attributes(lambda4)}
		else {
			lambda3 <- lambda4
			rm(lambda4)}}
	else {
	# formula with unknowns
		if(npl!=npt1&&lf1==0){
			cat("\nParameters are ")
			cat(attr(lambda4,"parameters"),"\n")
			stop(paste("plambda should have",npt1,"estimates"))}
		if(dt){
			lambda3 <- if(lf1>0)
				function(p,linear) lambda4(p,linear)+delta
				else function(p) lambda4(p)+delta
			attributes(lambda3) <- attributes(lambda4)}
		else lambda3 <- lambda4
		if(is.list(plambda)){
			if(!is.null(names(plambda))){
				o <- match(attr(lambda4,"parameters"),names(plambda))
				plambda <- unlist(plambda)[o]
				if(sum(!is.na(o))!=length(plambda))stop("invalid estimates for lambda - probably wrong names")}
			else plambda <- unlist(plambda)}}}
else if(!is.function(lambda)){
	lambda3 <- if(dt)function(p) p[1]*rep(1,n)+delta
		else function(p) p[1]*rep(1,n)
	npt1 <- 1}
else {
	lambda4 <- fnenvir(lambda,.envir=envir)
	npt1 <- length(attr(lambda4,"parameters"))-(lf1>0)
	if(dt)lambda3 <- if(lf1>0)function(p,linear) lambda(p,linear)+delta
		else function(p) lambda(p)+delta
	else lambda3 <- lambda
	attributes(lambda3) <- attributes(lambda4)
	rm(lambda4)}
#
# if linear part, modify location function appropriately
#
if(lf1>0){
	if(is.character(attr(lambda3,"model")))
		stop("lambda cannot be a W&R formula if linear is supplied")
	dm1 <- wr(linear,data=envir)$design
	if(is.null(lambda2))lambda2 <- lambda3
	lambda1 <- function(p)lambda3(p,dm1%*%p[(npt1+1):(npt1+lf1)])}
else {
	if(lf1==0&&length(lambda3(plambda))==1){
		lambda1 <- function(p) lambda3(p)*rep(1,n)
		attributes(lambda1) <- attributes(lambda3)}
	else lambda1 <- lambda3}
#
# give appropriate attributes to lambda1 for printing
#
if(is.null(attr(lambda1,"parameters"))){
	attributes(lambda1) <- if(is.function(lambda)){
		if(!inherits(lambda,"formulafn"))
			attributes(fnenvir(lambda,.envir=envir))
		else attributes(lambda)}
		else attributes(fnenvir(lambda1))}
#
# check that correct number of estimates was supplied
#
nlp <- npt1+lf1
if(nlp!=npl)stop(paste("plambda should have",nlp,"initial estimates"))
#
# set up likelihood function, check that it returns appropriate values
# and call nlm
#
fn <- function(p) {
	l <- lambda1(p)
	sum(exp(l)-point*l)}
if(fscale==1)fscale <- fn(plambda)
if(is.na(fn(plambda)))
	stop("Likelihood returns NAs: probably invalid initial values")
z0 <- nlm(fn, p=plambda, hessian=TRUE, print.level=print.level, typsize=typsize,
	ndigit=ndigit, gradtol=gradtol, stepmax=stepmax,
	steptol=steptol, iterlim=iterlim, fscale=fscale)
if(any(point>1))z0$minimum <- z0$minimum+sum(lgamma(point+1))
#
# calculate se's
#
if(length(plambda)==1)cov <- 1/z0$hessian
else {
	a <- if(any(is.na(z0$hessian))||any(abs(z0$hessian)==Inf))0
		else qr(z0$hessian)$rank
	if(a==length(plambda))cov <- solve(z0$hessian)
	else cov <- matrix(NA,ncol=length(plambda),nrow=length(plambda))}
se <- sqrt(diag(cov))
z1 <- list(
	call=call,
	intensity=lambda1,
	linear=linear,
	linmodel=linmodel,
	maxlike=z0$minimum,
	aic=z0$minimum+length(plambda),
	coefficients=z0$estimate,
	se=se,
	cov=cov,
	corr=cov/(se%o%se),
	gradient=z0$gradient,
	iterations=z0$iter,
	error=z0$error,
	code=z0$code)
class(z1) <- "intensity"
return(z1)}

### standard method

deviance.intensity <- function(object, ...) 2*object$maxlike

### print method
###
print.intensity <- function(x, ...) {
  z <- x ## legacy / S3methods consistency
np <- length(z$coefficients)
cat("\nCall:",deparse(z$call),sep="\n")
cat("\n")
if(z$code>2)cat("Warning: no convergence - error",z$code,"\n\n")
cat("Log intensity function:\n")
if(!is.null(attr(z$intensity,"formula")))
	cat(deparse(attr(z$intensity,"formula")),sep="\n")
else if(!is.null(attr(z$intensity,"model"))){
	t <- deparse(attr(z$intensity,"model"))
	t[1] <- sub("expression\\(","",t[1])
	t[length(t)] <- sub("\\)$","",t[length(t)])
	cat(t,sep="\n")}
if(inherits(z$linear,"formulafn"))
	cat("Linear part: ",deparse(attr(z$linear,"formula")),sep="\n")
cat("\n-Log likelihood   ",z$maxlike,"\n")
cat("AIC               ",z$aic,"\n")
cat("Iterations        ",z$iterations,"\n\n")
cat("Coefficients:\n")
cname <- if(is.character(attr(z$intensity,"model")))attr(z$intensity,"model")
	else if(length(grep("linear",attr(z$intensity,"parameters")))>0)
	attr(z$intensity,"parameters")[grep("\\[",attr(z$intensity,"parameters"))]
	else attr(z$intensity,"parameters")
if(!is.null(z$linmodel))cname <- c(cname,z$linmodel)
coef.table <- cbind(z$coefficients, z$se)
dimnames(coef.table) <- list(cname, c("estimate", "se"))
print.default(coef.table, digits=4, print.gap=2)
if(np>1){
	cat("\nCorrelations:\n")
	dimnames(z$corr) <- list(seq(1,np),seq(1,np))
	print.default(z$corr, digits=4)}
invisible(z)}

# examples of linear log intensity functions
#exponential <- ~1
#Weibull <- ~log(time(y))
#extreme.value <- ~time(y)
#birth1 <- ~bp(y,id)
#birth2 <- ~log(1+bp(y,id))

# examples of nonlinear log intensity functions
#negative.binomial <- function(p) p[1]+log(p[2]+bp(y,id))
#gen.negative.binomial <- function(p) p[1]+p[3]*log(p[2]+bp(y,id))
