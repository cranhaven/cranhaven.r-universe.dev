#
#  growth : A Library of Normal Distribution Growth Curve Models
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
#     carma(response=NULL, ccov=NULL, times=NULL, torder=0, interaction,
#	arma=c(0,0,0), parma=NULL, pre=NULL, position=NULL, iopt=TRUE,
#	resid=TRUE, transform="identity", delta=NULL, envir=parent.frame(),
#	print.level=0, typsize=abs(p), ndigit=10, gradtol=0.00001,
#	steptol=0.00001, iterlim=100, fscale=1, stepmax=10*sqrt(p%*%p))
#
#  DESCRIPTION
#
#    Function to fit the multivariate normal distribution with ARMA
#  and random intercept using Kalman filtering in continuous time

# .First.lib <- function(lib, pkg)
# 	library.dynam("growth", pkg, lib)
# require(rmutil)



#' Continuous ARMA for Unequally Spaced Repeated Measurements
#' 
#' \code{carma} is designed to handle a polynomial within subject design matrix
#' with unequally spaced observations which can be at different times for
#' different subjects. The origin of time is taken as the mean time of all the
#' subjects. The within subject errors are assumed to be independent Gaussian
#' or have a continuous time ARMA(p,q) Gaussian structure with the option to
#' include measurement error. The between subject random coefficients are
#' assumed to have an arbitrary covariance matrix. The fixed effect design
#' matrix is a polynomial of equal or higher order than the within subject
#' design matrix. This matrix can be augmented by covariates multiplied by
#' polynomial design matrices of any order up to the order of the first
#' partition of the design matrix. The method is based on exact maximum
#' likelihood using the Kalman filter to calculate the likelihood.
#' 
#' For clustered (non-longitudinal) data, where only random effects will be
#' fitted, \code{times} are not necessary.
#' 
#' Marginal and individual profiles can be plotted using
#' \code{\link[rmutil]{mprofile}} and \code{\link[rmutil]{iprofile}} and
#' residuals with \code{\link[rmutil]{plot.residuals}}.
#' 
#' For any ARMA of order superior to an AR(1), the (complex) roots of the
#' characteristic equation are printed out; see Jones and Ackerson (1991) for
#' their use in calculation of the covariance function.
#' 
#' 
#' @aliases carma fitted.carma mprofile.carma residuals.carma
#' @param response A list of two column matrices with response values and times
#' for each individual, one matrix or dataframe of response values, or an
#' object of either class, \code{response} (created by
#' \code{\link[rmutil]{restovec}}) or \code{repeated} (created by
#' \code{\link[rmutil]{rmna}} or \code{\link[rmutil]{lvna}}). If the
#' \code{repeated} data object contains more than one response variable, give
#' that object in \code{envir} and give the name of the response variable to be
#' used here.
#' @param ccov A matrix of columns of baseline covariates with one row per
#' individual, a model formula using vectors of the same size, or an object of
#' class, \code{tccov} (created by \code{\link[rmutil]{tcctomat}}). If response
#' has class, \code{repeated}, the covariates must be specified as a Wilkinson
#' and Rogers formula unless none are to be used.
#' @param times When response is a matrix, a vector of possibly unequally
#' spaced times when they are the same for all individuals or a matrix of
#' times. Not necessary if equally spaced. Ignored if response has class,
#' \code{response} or \code{repeated}.
#' @param torder Order of the polynomial in time to be fitted.
#' @param interaction Vector indicating order of interactions of covariates
#' with time.
#' @param transform Transformation of the response variable: \code{identity},
#' \code{exp}, \code{square}, \code{sqrt}, or \code{log}.
#' @param arma Vector of three values: order of AR, order of MA, binary
#' indicator for presence of measurement error. Not required for an AR(1) if an
#' initial estimate is supplied. If only one value is supplied, it is assumed
#' to be the order of the AR.
#' @param parma Initial estimates of ARMA parameters. For example, with
#' \code{arma=c(1,0,0)}, an AR(1), the parameter is \code{parma[1]=log(theta)},
#' where \code{theta} is the positive, continuous time autoregressive
#' coefficient. The finite step autoregression coefficient for a step of length
#' \code{delta} is then \code{alpha=exp(-delta*theta)} i.e.
#' \code{alpha=exp(-delta*exp(parma[1]))}.
#' @param pre Initial estimates of random effect parameters.
#' @param position Two column matrix with rows giving index positions of random
#' effects in the covariance matrix.
#' @param iopt TRUE if optimization should be performed.
#' @param resid TRUE if residuals to be calculated.
#' @param delta Scalar or vector giving the unit of measurement for each
#' response value, set to unity by default. For example, if a response is
#' measured to two decimals, \code{delta=0.01}. Ignored if response has class,
#' \code{response} or \code{repeated}.
#' @param envir Environment in which model formulae are to be interpreted or a
#' data object of class, \code{repeated}, \code{tccov}, or \code{tvcov}; the
#' name of the response variable should be given in \code{response}. If
#' \code{response} has class \code{repeated}, it is used as the environment.
##' @param print.level Arguments for nlm.
##' @param typsize Arguments for nlm.
##' @param ndigit Arguments for nlm.
##' @param gradtol Arguments for nlm.
##' @param stepmax Arguments for nlm.
##' @param steptol Arguments for nlm.
##' @param iterlim Arguments for nlm.
##' @param fscale Arguments for nlm.
#' @param object An object of class, \code{carma}.
#' @param z An object of class, \code{carma}.
#' @param x An object of class, \code{carma}.
#' @param plotse Plot the standard errors around the marginal profile curve.
#' @param recursive If TRUE, recursive residuals or fitted values are given;
#' otherwise, marginal ones.
#' @param correlation logical; print correlations.
#' @param digits number of digits to print.
#' @param ... additional arguments.
#' @return A list of class \code{carma} is returned that contains all of the
#' relevant information calculated, including error codes.
#' @author R.H. Jones and J.K. Lindsey
#' @seealso \code{\link[growth]{elliptic}}, \code{\link[repeated]{gar}},
#' \code{\link[repeated]{gnlmix}}, \code{\link[repeated]{glmm}},
#' \code{\link[repeated]{gnlmm}}, \code{\link[rmutil]{iprofile}},
#' \code{\link[repeated]{kalseries}}, \code{\link[rmutil]{mprofile}},
#' \code{\link[rmutil]{plot.residuals}}, \code{\link[growth]{potthoff}},
#' \code{\link[rmutil]{read.list}}, \code{\link[rmutil]{restovec}},
#' \code{\link[rmutil]{rmna}}, \code{\link[rmutil]{tcctomat}},
#' \code{\link[rmutil]{tvctomat}}.
#' @references Jones, R. H. and Ackerson, L. M. (1991) Serial correlation in
#' unequally spaced longitudinal data. Biometrika, 77, 721-731.
#' 
#' Jones, R.H. (1993) Longitudinal Data Analysis with Serial Correlation: A
#' State-space Approach. Chapman and Hall
#' @keywords models
#' @examples
#' 
#' y <- matrix(rnorm(40),ncol=5)
#' x1 <- gl(2,4)
#' x2 <- gl(2,1,8)
#' # independence with time trend
#' carma(y, ccov=~x1, torder=2)
#' # AR(1)
#' carma(y, ccov=~x1, torder=2, arma=c(1,0,0), parma=-0.5)
#' carma(y, ccov=~x1, torder=3, interact=3, arma=c(1,0,0), parma=-1)
#' # ARMA(2,1)
#' carma(y, ccov=~x1+x2, interact=c(2,0), torder=3,arma=c(2,1,0),
#' 	parma=c(0.3,2,0.7))
#' # random intercept
#' carma(y, ccov=~x1+x2, interact=c(2,0), torder=3, pre=-0.4, 
#' 	position=c(1,1))
#' # random coefficients
#' carma(y, ccov=~x1+x2, interact=c(2,0), torder=3, pre=c(-0.4,0.1), 
#' 	position=rbind(c(1,1),c(2,2)))
#' 
#' @export
carma <- function(response=NULL, ccov=NULL, times=NULL, torder=0, interaction,
	arma=c(0,0,0), parma=NULL, pre=NULL, position=NULL, iopt=TRUE,
	resid=TRUE, transform="identity", delta=NULL, envir=parent.frame(),
	print.level=0, typsize=abs(p), ndigit=10, gradtol=0.00001,
	steptol=0.00001, iterlim=100, fscale=1, stepmax=10*sqrt(p%*%p)){
#
# likelihood function returning parameter values
#
kalman <- function(p){
	z <- .Fortran("kalman",
		np=as.integer(length(p)),
		par=as.double(p),
		like=double(1),
		xx=matrix(0,nrow=nlp+1,ncol=nlp+1),
		y=as.double(y),
		sse=double(1),
		nq=as.integer(nre),
		nlp=as.integer(nlp),
		ns=as.integer(ns),
		nt=as.integer(nt),
		model=as.integer(arma),
		t=as.double(times),
		nobs=as.integer(nobs(response)),
		nod=as.integer(nod),
		as.integer(position),
		cv=as.double(ccov),
		ncv=as.integer(ncv),
		nxcv=as.integer(interaction),
		nx=as.integer(torder),
		p=double(length(p)),
		x=double(nlp+1),
		state=double(maxre*(nlp+1)),
		innov=double(nlp+1),
		cstate=complex(maxar*(nlp+1)),
		exx=complex(nlp+1),
		#DUP=FALSE,
		PACKAGE="growth")
	list(like=z$like/2,
		sse=z$sse,
		xx=z$xx)}
#
# likelihood function for nlm
#
kalmanl <- function(p){
	z <- .Fortran("kalman",
		np=as.integer(length(p)),
		par=as.double(p),
		like=double(1),
		xx=matrix(0,nrow=nlp+1,ncol=nlp+1),
		y=as.double(y),
		sse=double(1),
		nq=as.integer(nre),
		nlp=as.integer(nlp),
		ns=as.integer(ns),
		nt=as.integer(nt),
		model=as.integer(arma),
		t=as.double(times),
		nobs=as.integer(nobs(response)),
		nod=as.integer(nod),
		as.integer(position),
		cv=as.double(ccov),
		ncv=as.integer(ncv),
		nxcv=as.integer(interaction),
		nx=as.integer(torder),
		p=double(length(p)),
		x=double(nlp+1),
		state=double(maxre*(nlp+1)),
		innov=double(nlp+1),
		cstate=complex(maxar*(nlp+1)),
		exx=complex(nlp+1),
		#DUP=FALSE,
		PACKAGE="growth")
	z$like/2}
#
# Fortran constants for dependence parameters
#
maxre <- 6
maxar <- 6
maxma <- 5
call <- sys.call()
#
# check transformation
#
if(!missing(transform))transform <- match.arg(transform,c("identity",
	"exp","square","sqrt","log"))
#
# if envir, remove extra (multivariate) responses
#
type <- "unknown"
respenv <- exists(deparse(substitute(response)),envir=parent.frame())&&
	inherits(response,"repeated")&&!inherits(envir,"repeated")
if(!respenv&&inherits(envir,"repeated")){
	if(!is.null(envir$NAs)&&any(envir$NAs))
		stop("carma does not handle data with NAs")
	cn <- deparse(substitute(response))
	if(length(grep("\"",cn))>0)cn <- response
	if(length(cn)>1)stop("only one response variable allowed")
	response <- envir
	col <- match(cn,colnames(response$response$y))
	if(is.na(col))stop(paste("response variable",cn,"not found"))
	type <- response$response$type[col]
	if(dim(response$response$y)[2]>1){
		response$response$y <- response$response$y[,col,drop=FALSE]
		if(!is.null(response$response$delta)){
			response$response$delta <- response$response$delta[,col,drop=FALSE]
			if(all(response$response$delta==1)||all(is.na(response$response$delta)))response$response$delta <- NULL}}}
#
#if response is not a data object, make one and handle covariates
#
if(inherits(response,"repeated")){
	if(dim(response$response$y)[2]>1)
		stop("carma only handles univariate responses")
	if(!is.null(response$NAs)&&any(response$NAs))
		stop("carma does not handle data with NAs")
	type <- response$response$type
	# only keep covariates used in model in data object
	if(is.null(ccov))response$ccov <- NULL
	else if(inherits(ccov,"formula")){
		if(any(is.na(match(rownames(attr(terms(ccov,data=response),"factors")),colnames(response$ccov$ccov)))))
			stop("ccov covariate(s) not found")
		tmp <- wr(ccov,data=response,expand=FALSE)$design
		response$ccov$ccov <- tmp[,-1,drop=FALSE]
		rm(tmp)}
	else stop("ccov must be a W&R formula")
	ncv <- if(is.null(response$ccov$ccov)) 0
		 else  dim(response$ccov$ccov)[2]}
else {
	if(!inherits(response,"response"))
		response <- restovec(response,times,delta=delta)
	type <- response$type
	if(dim(response$y)[2]>1)stop("carma only handles univariate responses")
	if(!is.null(ccov)){
	# find covariates and put in data object
		if(!inherits(ccov,"tccov")){
			ccname <- deparse(substitute(ccov))
			if(ccname=="~1")stop("ccov must contain variables")
			if((is.matrix(ccov)&&is.null(colnames(ccov)))){
				ccname <- deparse(substitute(ccov))
				if(dim(ccov)[2]>1){
					tmp <- NULL
					for(i in 1:dim(ccov)[2])tmp <- c(tmp,paste(ccname,i,sep=""))
					ccname <- tmp}}
			ccov <- tcctomat(ccov,names=ccname)}
		ncv <- dim(ccov$ccov)[2]}
	else ncv <- 0
	response <- rmna(response=response, ccov=ccov)
	if(!is.null(ccov))rm(ccov)}
if(type!="unknown"&&type!="continuous")stop("continuous data required")
if(!is.null(response$response$nest)&&arma[1]>0)
	stop("carma does not handle two levels of nesting with an AR")
if(!is.null(response$response$censor))stop("carma does not handle censoring")
y <- response$response$y
ns <- length(nobs(response))
if(!is.null(response$ccov))ccov <- t(response$ccov$ccov)
#
# check times
#
torder <- torder+1
if(!is.null(response$response$times)){
	if(torder>length(unique(response$response$times)))
		stop("torder is too large for the number of distinct times")
	ave <- mean(response$response$times)
	times <- response$response$times-ave}
else {
	if(any(arma>0)||!is.null(parma))stop("No times. ARMA cannot be fitted")
	else if(torder>1)stop("No times. Time trends cannot be fitted.")
	ave <- 0
	times <- 1:length(y)}
#
# check interactions of covariates with times
#
if(missing(interaction)) {
	if(ncv==0)interaction <- NULL
	else interaction <- rep(1,ncv)}
else if(length(interaction)!=ncv)
	stop(paste("The vector, interaction, must have length, ", ncv))
else interaction <- interaction+1
if(!is.null(interaction)&&any(interaction>torder))
	stop("Interactions cannot have higher order than torder")
#
# check arma
#
if(length(arma)!=3){
	if(length(arma)==1)arma <- c(arma,0,0)
	else stop("The model specification must have length 3")}
if(sum(arma)==0&&!missing(parma)){
	if(length(parma)==1)arma[1] <- 1
	else stop("If an ARMA is required, arma must be supplied")}
if(arma[1]>maxar)stop(paste("Maximum order of AR is ",maxar))
if(arma[2]>maxma)stop(paste("Maximum order of MA is ",maxma))
if(arma[2]>=arma[1]&&arma[2]!=0)
	stop(paste("Order of MA(",arma[2],") >= order of AR(",arma[1],")"))
if(arma[1]==0&&arma[3]!=0)
	stop("Cannot have observation errors without AR structure")
np <- arma[1]+arma[2]+arma[3]
p <- parma
if(np>0&&length(parma)!=np)
	stop(paste(np,"initial arma estimates must be supplied"))
#
# check random coefficients
#
if(!is.null(pre))pre[pre==0] <- 0.01
nre <- length(pre)
if(nre>maxre)stop(paste("Only ",maxre," random effects allowed"))
if(is.null(position)){
	if(nre==1)position <- c(1,1)
	else if(nre==2)position <- rbind(c(1,1),c(2,2))
	else if(nre>2)stop("If random effects are required, position must be supplied")}
if(!is.null(position)){
	if(!is.matrix(position))position <- t(as.matrix(position))
	if(dim(position)[1]!=nre)
		stop(paste("Random effects position must have",nre,"rows"))
	if(dim(position)[2]!=2)stop("Position matrix must have two columns")
	if((any(position[,1]<1)||any(position[,1]>nre)||any(position[,2]<1)||
		any(position[,2]>nre))||any(position[,1]>position[,2]))
		stop("Position for covariance matrix out of range")
	if(max(position[,2])>torder)
		warning("number of random effects greater than order of polynomial in time")
	nod <- dim(position)[1]
	if(nod>nre*(nre+1)/2)
		stop(paste("Only ",nre*(nre+1)/2," elements allowed in the random effects covariance matrix"))
	else if(nod<nre)stop("Not enough initial estimates of random effects")
	p <- c(p,pre)
	np <- length(p)}
else {
	position <- NULL
	nod <- 0}
nt <- sum(nobs(response))
nlp <- torder
if(ncv>0) nlp <- nlp+sum(interaction)
#
# calculate transformation and Jacobian
#
if(transform=="identity")jacob <- 0
else if(transform=="exp"){
	jacob <- -sum(y)
	y <- exp(y)}
else if(any(y==0))stop("Zero response values: invalid transformation")
else if(transform=="square"){
	jacob <- -sum(log(abs(y)))-length(response$response$y)*log(2)
	y  <- y^2}
else if(any(y<0))stop("Nonpositive response values: invalid transformation")
else if(transform=="sqrt"){
	jacob <- sum(log(y))/2+length(response$response$y)*log(2)
	y <- sqrt(y)}
else if(transform=="log"){
	jacob <- sum(log(y))
	y <- log(y)}
#
# include delta in Jacobian
#
if(!is.null(response$response$delta)){
	if(length(response$response$delta)==1)jacob <- jacob-nt*log(response$response$delta)
	else jacob <- jacob -sum(log(response$response$delta))}
#
# estimate model
#
if(iopt&&np>0){
	if(fscale==1)fscale <- kalmanl(p)
	z0 <- nlm(kalmanl, p=p, hessian=TRUE, print.level=print.level,
		typsize=typsize, ndigit=ndigit, gradtol=gradtol, stepmax=stepmax,
		steptol=steptol, iterlim=iterlim, fscale=fscale)
	p <- z0$estimate}
z2 <- kalman(p)
like <- z2$like+jacob
sse <- z2$sse
mse <- sse/(nt-nlp-np)
aic <- like+np+nlp+1
#
# get beta coefficients
#
z1 <- .Fortran("back",
	xx=z2$xx,
	as.integer(nlp),
	#DUP=FALSE,
	PACKAGE="growth")
beta <- z1$xx[1:nlp,nlp+1]
#
# calculate se's
#
z1 <- .Fortran("ttvert",
	xx=z1$xx,
	as.integer(nlp),
	#DUP=FALSE,
	PACKAGE="growth")
vbeta <- z1$xx[1:nlp,1:nlp]*mse
if(nlp>1){
	betase <- sqrt(diag(vbeta))
	corr <- vbeta/(betase%o%betase)}
else {
     betase <- sqrt(vbeta)
     corr <- vbeta/betase^2}
if(np>0){
	if(np==1){
		nlcov <- 1/z0$hessian
		nlse <- sqrt(nlcov)}
	else {
		a <- if(any(is.na(z0$hessian))||any(abs(z0$hessian)==Inf))0
			else qr(z0$hessian)$rank
		if(a==np)nlcov <- solve(z0$hessian)
		else nlcov <- matrix(NA,ncol=np,nrow=np)
		nlse <- sqrt(diag(nlcov))}
	if(length(nlse)>1)nlcorr <- nlcov/(nlse%o%nlse)
	else nlcorr <- nlcov/nlse^2
	coef <- z0$estimate
	grad <- z0$gradient
	code <- z0$code
	iterations <- z0$iterations}
else nlcov <- nlse <- nlcorr <- coef <- grad <- code <- iterations <- NULL
#
# calculate random coefficient covariance matrix
#
if(nod>0){
	a <- matrix(0,nrow=max(position[,2]),ncol=max(position[,2]))
	ii <- sum(arma)
	for(i in 1:nod){
		ii <- ii+1
		a[position[i,1],position[i,2]] <- p[ii]}}
else a <- NULL
#
# calculate recursive fitted values
#
if(resid){
	coln <- NULL
	if(ncv>0)for(i in 1:ns)for(j in 1:nobs(response)[i])
		coln <- rbind(coln,ccov[1:ncv,i])
	pred <- rep(beta[1],nt)
	cc <- matrix(rep(1,nt),ncol=1)
	if(torder>1)for(i in 1:(torder-1)) {
		cc <- cbind(cc,times^i)
		pred <- pred+beta[i+1]*times^i}
	if(ncv>0){
		jj <- torder
		for(i in 1:ncv)for(j in 1:interaction[i]){
			jj <- jj+1
			cc <- cbind(cc,cc[,j]*coln[,i])
			pred <- pred+beta[jj]*cc[,j]*coln[,i]}}
	z <- .Fortran("resid",
		np=as.integer(length(p)),
		par=as.double(coef),
		beta=as.double(beta),
		ave=as.double(ave),
		pred=double(nt),
		sdr=double(nt),
		res=double(nt),
		y=as.double(y),
		sse=as.double(sse),
		nq=as.integer(nre),
		nlp=as.integer(nlp),
		ns=as.integer(ns),
		nt=as.integer(nt),
		model=as.integer(arma),
		t=as.double(times),
		nobs=as.integer(nobs(response)),
		nod=as.integer(nod),
		as.integer(position),
		cv=as.double(ccov),
		nxcv=as.integer(interaction),
		nx=as.integer(torder),
		ncv=as.integer(ncv),
		p=double(length(p)),
		x=double(nlp+1),
		#DUP=FALSE,
		PACKAGE="growth")
	rpred <- z$pred
	sdr <- z$sdr
	rres <- z$res}
else pred <- res <- rpred <- sdr <- rres <- NULL
z <- list(
	call=call,
	response=response$response,
	transform=transform,
	ccov=response$ccov,
	torder=torder-1,
	interaction=interaction-1,
	np=np,
	nlp=nlp,
	nre=nre,
	nod=nod,
	arma=arma,
	maxlike=like,
	aic=aic,
	df=nt-nlp-np-1,
	coefficients=coef,
	nlse=nlse,
	nlcov=nlcov,
	nlcorr=nlcorr,
	are=a,
	position=position,
	beta=beta,
	betase=betase,
	vbeta=vbeta,
	corr=corr,
	sse=sse,
	mse=mse,
	pred=pred,
	rpred=rpred,
	sdrpred=sdr,
	rresiduals=rres,
	gradient=grad,
	iterations=iterations,
	code=code)
class(z) <- "carma"
if(resid)class(z) <- c(class(z),"recursive")
return(z)}

### standard methods
#' @describeIn carma Coefficients
#' @export 
coef.carma <- function(object, ...) list(beta=object$beta,coef=object$coefficients)
#' @describeIn carma Deviance
#' @export 
deviance.carma <- function(object, ...) 2*object$maxlike
#' @export 
fitted.carma <- function(object, recursive=TRUE, ...) if(recursive) object$rpred else object$pred
#' @describeIn carma Residuals
#' @export 
residuals.carma <- function(object, recursive=TRUE, ...){
if(recursive)return(object$rresiduals)
else {
	if(object$transform=="exp")object$response$y <- exp(object$response$y)
	else if(object$transform=="square")object$response$y  <- object$response$y^2
	else if(object$transform=="sqrt")object$response$y <- sqrt(object$response$y)
	else if(object$transform=="log")object$response$y <- log(object$response$y)
	return((object$response$y-object$pred)/sqrt(object$mse))}}

### print method
#' @describeIn carma Print method
#' @export 
print.carma <- function(x,digits=max(3,.Options$digits-3),correlation=TRUE,...){
  z <- x; # S3 consistency
if(!is.null(z$ccov$ccov))nccov <- dim(z$ccov$ccov)[2]
else nccov <- 0
cat("\nCall:",deparse(z$call),sep="\n")
cat("\n")
if(!is.null(z$code)&&z$code>2)cat("Warning: no convergence - error",z$code,"\n\n")
cat("Number of subjects    ",length(nobs(z)),"\n")
cat("Number of observations",length(z$response$y),"\n")
if(!is.null(z$response$times))cat("Mean time             ",mean(z$response$times),"\n")
cat("Transformation        ",z$trans,"\n\n")
cat("-Log likelihood   ",z$maxlike,"\n")
cat("Degrees of freedom",z$df,"\n")
cat("AIC               ",z$aic,"\n")
if(!is.null(z$iterations))cat("Iterations        ",z$iterations,"\n\n")
cat("Estimates of linear parameters\n")
if(inherits(z$ccov$linear,"formula"))
	cat("Formula: ",deparse(z$ccov$linear[[2]]),"\n")
coef.table <- cbind(z$beta, z$betase)
cname <- "(Intercept)"
if(z$torder>0)cname <- c(cname,paste("t^",1:z$torder,sep=""))
if(nccov>0)for(i in 1:nccov){
	cname <- c(cname,colnames(z$ccov$ccov)[i])
	if(!is.na(z$interaction[1])&&z$interaction[i]>0){
		cname <- c(cname,paste(colnames(z$ccov$ccov)[i],".t^",1:z$interaction[i],sep=""))}}
dimnames(coef.table) <- list(cname, c("estimate", "se"))
print.default(coef.table, digits=digits, print.gap=2)
if(z$nlp>1&&correlation){
	cat("\nCorrelation matrix of linear parameters\n")
	dimnames(z$corr) <- list(seq(1,z$nlp),seq(1,z$nlp))
	print.default(z$corr, digits=digits)}
cat("\n(REML) Variance ",z$mse,"\n")
if(z$np>0){
	cat("\nEstimates of nonlinear parameters\n")
	n1 <- z$arma[1]+z$arma[2]
	if(n1>1) {
		z0 <- .Fortran("roots",
			as.integer(n1),
			as.double(z$coef[1:n1]),
			r=complex(n1),
			#DUP=FALSE,
			PACKAGE="growth")
		tmp <- if(any(Im(z0$r)!=0))z0$r else Re(z0$r)
		title <- "Roots"}
	if(z$arma[1]>0){
		cat("Autoregression\n")
		n2 <- z$arma[1]
		if(n2==1&&z$arma[2]==0){
			tmp <- exp(-exp(z$coef[1]))
			title <- "AR"}
		coef.table <- cbind(z$coef[1:n2],z$nlse[1:n2],
			tmp[1:n2])
		dimnames(coef.table) <- list(seq(1,z$arma[1]),
			c("estimate","se",title))
		print.default(coef.table, digits=digits, print.gap=2)}
	if(z$arma[2]>0){
		cat("Moving average\n")
		n1 <- z$arma[1]+1
		n2 <- z$arma[1]+z$arma[2]
		coef.table <- cbind(z$coef[n1:n2],z$nlse[n1:n2],
			tmp[n1:n2])
		dimnames(coef.table) <- list(seq(1,z$arma[2]),
			c("estimate","se",title))
		print.default(coef.table, digits=digits, print.gap=2)}
	if(z$arma[3]>0){
		cat("Measurement error\n")
		n1 <- z$arma[1]+z$arma[2]+1
		coef.table <- cbind(z$coef[n1],z$nlse[n1],exp(z$coef[n1]))
		dimnames(coef.table) <- list("1",c("estimate","se",""))
		print.default(coef.table, digits=digits, print.gap=2)}}
if(z$nod>0){
	cat("Estimated unscaled factored between subject covariance matrix\n")
	n1 <- z$arma[1]+z$arma[2]+z$arma[3]+1
	n2 <- n1+z$nre-1
	coef.table <- cbind(z$position[1:z$nre,,drop=FALSE],
		z$are[z$position[1:z$nre,,drop=FALSE]],z$nlse[n1:n2])
	dimnames(coef.table) <- list(rep("",z$nre),
		c("i1","i2","estimate","se"))
	print.default(coef.table, digits=digits, print.gap=2)
	cat("Estimated between subject covariance matrix\n")
	tmps <- max(z$position[,2])
	wrk <- matrix(0,ncol=tmps,nrow=tmps)
	for(j in 1:tmps)for(i in 1:j){
		for(l in 1:i)wrk[i,j] <- wrk[i,j]+z$are[l,i]*
			z$are[l,j]*z$mse
		wrk[j,i] <- wrk[i,j]}
	dimnames(wrk) <- list(1:tmps,1:tmps)
	print.default(wrk, digits=digits)}
if(z$np>1&&correlation){
	cat("\nCorrelation matrix of nonlinear parameters\n")
	dimnames(z$nlcorr) <- list(seq(1,z$np),seq(1,z$np))
	print.default(z$nlcorr, digits=digits)}}

### special marginal profiles with se's
#' @describeIn carma Special marginal profiles with SEs
#' @export 
mprofile.carma <- function(z, times=NULL, ccov, plotse=TRUE, ...){
#
# if there are time-constant covariates, calculate how many and check
# for what values plot is to be made
#
if(!is.null(z$ccov$ccov)){
	nccov <- dim(z$ccov$ccov)[2]
	if(missing(ccov))stop("Covariate values must be supplied")
	else if(nccov!=length(ccov))
		stop("Incorrect number of covariate values")}
else nccov <- 0
#
# obtain plotting times
#
z$ptimes <- if(is.null(times))seq(min(z$response$times),
	max(z$response$times),length.out=25) else times
npts <- length(z$ptimes)
#
# calculate the polynomial
#
z$pred <- rep(z$beta[1],npts)
cc <- matrix(rep(1,npts),ncol=1)
if(z$torder>0)for(i in 1:z$torder) {
	cc <- cbind(cc,(z$ptimes-mean(z$response$times))^i)
	z$pred <- z$pred+z$beta[i+1]*(z$ptimes-mean(z$response$times))^i}
if(nccov>0){
	jj <- z$torder+1
	for(i in 1:nccov)for(j in 1:(z$interaction[i]+1)){
		jj <- jj+1
		cc <- cbind(cc,cc[,j]*ccov[i])
		z$pred <- z$pred+z$beta[jj]*cc[,j]*ccov[i]}}
se <- NULL
for(i in 1:npts) se <- c(se,sqrt(cc[i,]%*%z$vbeta%*%cc[i,]))
#
# response was transformed, transform fitted values
#
if(z$transform=="exp"){
	if(plotse){
		se1 <- log(z$pred+2*se)
		se2 <- log(z$pred-2*se)}
	z$pred <- log(z$pred)}
else if(z$transform=="square"){
	if(plotse){
		se1 <- sqrt(z$pred+2*se)
		se2 <- sqrt(z$pred-2*se)}
	z$pred  <- sqrt(z$pred)}
else if(z$transform=="sqrt"){
	if(plotse){
		se1 <- (z$pred+2*se)^2
		se2 <- (z$pred-2*se)^2}
	z$pred <- z$pred^2}
else if(z$transform=="log"){
	if(plotse){
		se1 <- exp(z$pred+2*se)
		se2 <- exp(z$pred-2*se)}
	z$pred <- exp(z$pred)}
else {
	if(plotse){
		se1 <- z$pred+2*se
		se2 <- z$pred-2*se}}
#
# standard errors, if requested
#
if(plotse)z$pse <- cbind(se1,se2)
class(z) <- "mprofile"
invisible(z)}
